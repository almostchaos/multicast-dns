(ns dns.multicast.browse
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer [a-query srv-query]]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug]]))

(def mdns-port 5353)
(def multicast-host "224.0.0.251")

(defn- resource-type-matcher [type] (fn [section] (= type (:TYPE section))))
(def match-ptr (resource-type-matcher type:PTR))
(def match-srv (resource-type-matcher type:SRV))
(def match-a (resource-type-matcher type:A))

(defn- result-sequence [messages end-callback]
  (lazy-seq
    (let [message (<!! messages)]
      (if (nil? message)
        (do
          (end-callback)
          message)
        (cons message (result-sequence messages end-callback))))))

(defn name->ip [name]
  (debug "starting client ...")
  (debug "listening for dns response ...")

  (let [message-bytes (a-query name)
        messages (async/timeout 5000)
        queue-message (fn [& parameters]
                        (>!! messages parameters))
        {send :send close :close} (socket multicast-host mdns-port queue-message)]

    (debug "sending mdns request")
    (send multicast-host mdns-port message-bytes)
    (debug "sent mdns request")

    (->>
      (result-sequence messages close)
      (map
        (fn [[host port message]]
          (decode-message message)))
      (filter
        (fn [message]
          (let [header (first message)
                body (rest message)
                answer-count (:ANCOUNT header)]
            (and
              (> answer-count 0)
              (some match-a body)
              (= name (-> (filter match-a message) first :NAME))))))
      (map
        (fn [message]
          (async/close! messages)
          (-> (filter match-a message) first :RDATA)))
      (to-array)
      (first))))

(defn services-of-type [service-path]
  (debug "starting browser ...")
  (debug "listening for mdns response ...")

  (let [message-bytes (srv-query service-path)
        messages (async/timeout 10000)
        queue-message (fn [& parameters]
                        (>!! messages parameters))
        {send :send close :close} (socket multicast-host mdns-port queue-message)]

    (debug "sending mdns request")
    (send multicast-host mdns-port message-bytes)
    (debug "sent mdns request")

    (->>
      (result-sequence messages close)
      (map
        (fn [[host port message]] (decode-message message)))
      (filter
        (fn [message]
          (let [header (first message)
                body (rest message)
                answer-count (:ANCOUNT header)]
            (and
              (> answer-count 0)
              (some match-a body)
              (some match-ptr body)
              (= service-path (-> (filter match-ptr message) first :NAME))))))
      (map
        (fn [message]
          (let [a (first (filter match-a message))]
            (:NAME a)))))))

(defn -main [& args]
  (run! println (services-of-type "_smb._tcp.local"))
  ;;(println (name->ip "octopi.local"))

  (shutdown-agents))