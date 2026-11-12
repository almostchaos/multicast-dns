(ns dns.multicast.service.server
  (:require
    [clojure.core.async :as async :refer [alts!! offer!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [dns.multicast.service.config-server :as config-server]
    [dns.multicast.service.registry :refer [create-registry]]
    [org.httpkit.server :as http]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug info warn]])
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
  (let [registry (create-registry)
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
                (registry
                  (fn [_ register]
                    (register
                      (str service-instance "." service-type)
                      [service-type service-instance :host host :port port :txt txt :ttl (or ttl 120)]))))

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
              (let [timed-exit (async/timeout 1000)]
                (->>
                  (distinct (drain-channel-sequence queried-resources timed-exit))
                  (run!
                    (fn [[answer queried-resource]]
                      (->>
                        (registry (fn [lookup _] (lookup queried-resource)))
                        (run! (fn [[_ info]] (respond answer info))))))))))

          (control advertise stop)))
      :multicast address)))

(defn -main [& args]
  (listen
    "0.0.0.0"
    (fn [advertise shutdown]
      (let [host "127.0.0.1"]
        (advertise "_ssh._tcp.local" "B" "127.0.0.1" 36663 :txt {:path "/b" :q 0} :ttl 30)
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
