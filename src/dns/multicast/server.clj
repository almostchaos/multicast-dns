(ns dns.multicast.server
  (:require
    [clojure.core.async :as async :refer [>!! alts!! offer!]]
    [clojure.string :as string]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [dns.multicast.config-server :as config-server]
    [org.httpkit.server :as http]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug info warn]]
    [tick.core :as t])
  (:gen-class))

(def port 5353)
(def address "224.0.0.251")
(def http-port 3000)

(defn- query? [message]
  (not (:QR (first message))))

(defn- known-question? [question]
  (let [type (:QTYPE question)]
    (or
      (= type:PTR type)
      (= type:SRV type)
      (= type:TXT type))))

(defn- on-term-signal [handler]
  (.addShutdownHook
    (Runtime/getRuntime)
    (Thread.
      (fn []
        (debug "sigterm captured")
        (handler)))))

(defn- drain-channel-sequence [channel exit]
  (lazy-seq
    (let [[item channel] (alts!! [channel exit])]
      (when item
        (cons item (drain-channel-sequence channel exit))))))

(defn listen [bind-address control]
  (let [registered-resources (atom nil)
        running (atom true)
        queried-resources (async/chan 100)]
    (socket
      bind-address
      port
      ;;receive
      (fn [_ _ packet]
        (try
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
                (run! (partial offer! queried-resources)))))
          (catch Exception e
            (warn "cannot process incoming packet," (.getMessage e)))))
      ;;send/close
      (fn [send close]
        (let [respond
              (fn [answer parameters]
                (async/go
                  (let [random-delay (long (rand 500))
                        [type name] parameters]
                    (try
                      (async/<! (async/timeout random-delay))
                      (debug "sending response for" (str name "." type))
                      (send address port (apply answer parameters))
                      (catch Exception e
                        (warn "cannot respond to query," (.getMessage e)))))))

              advertise
              (fn [service-type service-instance host port & {txt :txt ttl :ttl}]
                (let [name (str service-instance "." service-type)
                      parameters [service-type service-instance :host host :port port :txt txt]
                      one-year (* 365 24 60 60)
                      expiry (t/>> (t/instant) (t/new-duration (or ttl one-year) :seconds))]
                  (swap! registered-resources conj [name parameters expiry])))

              stop
              (fn []
                (debug "stopping...")
                (reset! running false)
                (close)
                (async/close! queried-resources)
                (debug "stopped listening"))]
          (future
            (info "listening...")
            (while @running
              (let [timed-exit (async/timeout 1000)
                    now (t/instant)
                    valid? (fn [[_ _ expiry]] (t/< now expiry))]
                ;;discard expired resources
                (swap! registered-resources (fn [resources] (vec (filter valid? resources))))
                (->>
                  (distinct (drain-channel-sequence queried-resources timed-exit))
                  (run! (fn [[answer queried-resource]]
                          (->>
                            @registered-resources
                            (filter (fn [[name _ _]]
                                      (string/ends-with? name queried-resource)))
                            (run! (fn [[_ parameters expiry]]
                                    (let [answer-ttl (t/seconds (t/between now expiry))
                                          current-parameters (conj parameters :ttl answer-ttl)]
                                      (respond answer current-parameters)))))))))))

          (control advertise stop)))
      :multicast address)))

(defn -main [& args]
  (listen
    "0.0.0.0"
    (fn [advertise shutdown]
      (let [host "127.0.0.1"]
        (advertise "_zzzzz._tcp.local" "B" "127.0.0.1" 36663 :txt {:path "/b" :q 0})
        (advertise "_airplay._tcp.local" "A" "127.0.0.1" 36663)
        (advertise "_spotify-connect._tcp.local" "A" host 36663 :txt {:CPath "/zc" :VERSION "1.0" :Stack "SP"})
        (advertise "_googlecast._tcp.local" "A" "127.0.0.1" 36663)
        (advertise "_googlecast._tcp.local" "B" host 663 :ttl 30)
        (advertise "_octoprint._tcp.local" "A" host 36663 :txt {:bla 123}))
      (let [app (config-server/app advertise)
            shutdown-config-server (http/run-server app {:port http-port})]
        (on-term-signal
          (fn []
            (info "shutting down...")
            (shutdown)
            (shutdown-config-server :timeout 100)
            (shutdown-agents)))))))
