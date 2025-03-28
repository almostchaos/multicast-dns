(ns dns.multicast.server
  (:require
    [clojure.core.async :as async :refer [>!! alts!!]]
    [clojure.string :as string]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug info warn]]
    [tick.core :as t])
  (:import (java.net InetAddress)))

(def port 5353)
(def address "224.0.0.251")

(defn- query? [message]
  (not (:QR (first message))))

(defn- known-question? [question]
  (let [type (:QTYPE question)]
    (or
      (= type:PTR type)
      (= type:SRV type)
      (= type:TXT type))))

(defmacro on-term-signal [& handler]
  `(.addShutdownHook
     (Runtime/getRuntime)
     (new Thread
          (fn []
            (debug "sigterm captured")
            ~@handler))))

(defn- drain-channel-sequence [channel exit]
  (lazy-seq
    (let [[item channel] (alts!! [channel exit])]
      (when item
        (cons item (drain-channel-sequence channel exit))))))

(defn listen [bind-address]
  (let [registered-resources (atom nil)
        running (atom true)
        queried-resources (async/chan 100)

        receive (fn [_ _ packet]
                  (let [message (decode-message packet)]
                    (when (query? message)
                      (->>
                        (rest message)
                        (filter known-question?)
                        (map (fn [{type :QTYPE resource :QNAME}]
                               (cond
                                 (= type:PTR type) [ptr-answer resource]
                                 (= type:SRV type) [srv-answer resource]
                                 (= type:TXT type) [txt-answer resource])))
                        (run! (partial >!! queried-resources))))))


        {send         :send
         close-socket :close} (socket bind-address port receive :multicast address)

        respond (fn [answer parameters]
                  (async/go
                    (let [random-delay (long (rand 500))
                          [type name] parameters]
                      (try
                        (Thread/sleep random-delay)
                        (debug "sending response for" (str name "." type))
                        (send address port (apply answer parameters))
                        (catch Exception e
                          (warn "cannot respond to query," (.getMessage e)))))))]
    (future
      (info "listening...")

      (while @running
        (let [timed-exit (async/timeout 1000)
              now (t/instant)
              valid? (fn [[_ _ expiry]] (t/< now expiry))
              valid-registered-resources (filter valid? @registered-resources)]
          (->>
            (distinct (drain-channel-sequence queried-resources timed-exit))
            (run! (fn [[answer queried-resource]]
                    (->>
                      valid-registered-resources
                      (filter (fn [[name _ _]]
                                (string/ends-with? name queried-resource)))
                      (run! (fn [[_ parameters expiry]]
                              (let [answer-ttl (t/seconds (t/between now expiry))
                                    current-parameters (conj parameters :ttl answer-ttl)]
                                (respond answer current-parameters))))))))

          (reset! registered-resources valid-registered-resources))))

    {:advertise (fn [service-type service-instance host port & {txt :txt ttl :ttl}]
                  (let [name (str service-instance "." service-type)
                        parameters [service-type service-instance :host host :port port :txt txt]
                        one-year (* 365 24 60 60)
                        expiry (t/>> (t/instant) (t/new-duration (or ttl one-year) :seconds))]
                    (swap! registered-resources conj [name parameters expiry])))
     :stop      (fn []
                  (debug "stopping...")
                  (swap! running not)
                  (close-socket)
                  (async/close! queried-resources)
                  (debug "stopped listening"))}))

(defn -main [& args]
  (let [{advertise :advertise shutdown :stop} (listen "0.0.0.0")
        host (.getHostName (InetAddress/getLocalHost))]
    (advertise "_zzzzz._tcp.local" "B" host 36663 :txt {:path "/b" :q 0})
    (advertise "_airplay._tcp.local" "A" host 36663)
    (advertise "_spotify-connect._tcp.local" "A" host 36663)
    (advertise "_googlecast._tcp.local" "A" host 36663 :txt {:a 1 :b 2 :c "three"})
    (advertise "_googlecast._tcp.local" "B" host 663 :ttl 300)
    (advertise "_octoprint._tcp.local" "A" host 36663 :txt {:bla 123})
    (on-term-signal
      (info "shutting down...")
      (shutdown)
      (shutdown-agents))))