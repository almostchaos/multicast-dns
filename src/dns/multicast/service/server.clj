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

(defn- answer? [message]
  (:QR (first message)))

(defn- query? [message]
  (not (answer? message)))

(defn- drain-channel-sequence [channel exit]
  (lazy-seq
    (let [[item port] (alts!! [channel exit])]
      (when item
        (cons item (drain-channel-sequence port exit))))))

(defn listen [lookup-service lookup-ip bind-address]
  (let [running (atom true)
        query-messages (async/chan)
        answer-messages (atom [])]                          ;;use cache with ttl
    (socket
      bind-address
      port

      ;;receive
      (fn [_ _ packet]
        (try
          (let [message (decode-message packet)]
            (when (query? message)
              (offer! query-messages message)))
          (catch Exception e
            (warn "cannot process incoming packet," (.getMessage e)))))

      ;;send/close
      (fn [send close]
        (let [respond
              (fn [answer [service-type service-instance configured-info]]
                (async/go
                  (let [random-delay (long (rand 50))
                        ip (lookup-ip (:host configured-info))
                        enhanced-info
                        (if ip
                          (assoc configured-info :ip ip) configured-info)]
                    (try
                      (async/<! (async/timeout random-delay))
                      (debug "sending response for" (str service-instance "." service-type))
                      (send address port (answer service-type service-instance enhanced-info))
                      (catch Exception e
                        (warn "cannot respond to query," (.getMessage e)))))))

              stop
              (fn []
                (debug "stopping...")
                (reset! running false)
                (close)
                (async/close! query-messages)
                (debug "stopped listening"))]

          (future
            (info "listening...")
            (while @running
              (let [timed-exit (async/timeout 1000)]
                (->>
                  (distinct (drain-channel-sequence query-messages timed-exit))
                  (run!
                    (fn [[header & sections]]
                      (let [query-count (:QDCOUNT header)
                            queries (take query-count sections)]

                        (if (and (= 1 query-count) (= type:A (:QTYPE (first queries))))

                          (let [resource (:QNAME (first queries))]
                            (respond a-answer ["_dns._tcp.local" resource {:host resource}]))

                          (run!
                            (fn [{type :QTYPE resource :QNAME}]
                              (run!
                                (fn [[_ info]]
                                  (cond
                                    (= type:PTR type) (respond ptr-answer info)
                                    (= type:SRV type) (respond srv-answer info)
                                    (= type:TXT type) (respond txt-answer info)))
                                (lookup-service resource)))
                            queries)))))))))

          stop))
      :multicast address)))