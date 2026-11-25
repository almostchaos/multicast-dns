(ns dns.multicast.service.server
  (:require
    [clojure.core.async :as async :refer [alts!! offer!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug info warn]])
  (:gen-class))

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

(defn- drain-channel-sequence [channel exit]
  (lazy-seq
    (let [[item port] (alts!! [channel exit])]
      (when item
        (cons item (drain-channel-sequence port exit))))))

(defn listen [service-registry ip-registry bind-address]
  (let [running (atom true)
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
              (fn [answer [service-type service-instance configured-info]]
                (ip-registry
                  (fn [lookup _]
                    (async/go
                      (let [random-delay (long (rand 500))
                            ip (lookup (:host configured-info))
                            enhanced-info (if ip
                                            (assoc configured-info :ip ip) configured-info)]
                        (try
                          (async/<! (async/timeout random-delay))
                          (debug "sending response for" (str service-instance "." service-type))
                          (send address port (answer service-type service-instance enhanced-info))
                          (catch Exception e
                            (warn "cannot respond to query," (.getMessage e)))))))))

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
                        (service-registry (fn [lookup _] (lookup queried-resource)))
                        (run! (fn [[_ info]] (respond answer info))))))))))

          stop))
      :multicast address)))