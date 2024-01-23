(ns dns.multicast.browse
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer [a-query ptr-query]]
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
          nil)
        (cons message (result-sequence messages end-callback))))))

(defn name->ip [name]
  (let [message-bytes (a-query name)
        messages (async/timeout 2000)
        queue-message (fn [& parameters]
                        (>!! messages parameters))
        {send :send close :close} (socket multicast-host mdns-port queue-message)]

    (send multicast-host mdns-port message-bytes)
    (->
      (->>
        (result-sequence messages close)
        (map
          (fn [[host port message]]
            (decode-message message)))
        (filter
          (fn [message]
            (= name (-> (filter match-a message) (first) (:NAME)))))
        (map
          (fn [message]
            ;close channel as soon as first match is acquired
            (async/close! messages)
            (-> (filter match-a message) first :RDATA))))
      (to-array)
      (first))))

(defn service->names [service-path]
  (let [message-bytes (ptr-query service-path)
        messages (async/chan 10)
        queue-message (fn [& parameters]
                        (>!! messages parameters))
        {send :send close :close} (socket multicast-host mdns-port queue-message)]

    (send multicast-host mdns-port message-bytes)
    ;listen a limited time for responses
    (future
      (Thread/sleep 2000)
      (async/close! messages))
    (set
      (->>
        (result-sequence messages close)
        (map
          (fn [[host port message]] (decode-message message)))
        (filter
          (fn [message]
            (and
              (some match-a message)
              (= service-path (-> (filter match-ptr message) (first) (:NAME))))))
        (map
          (fn [message]
            (-> (filter match-a message) (first) (:NAME))))))))

(defn -main [& args]
  (run! println (service->names "_octoprint._tcp.local"))
  ;;(println (name->ip "octopi.local"))

  (shutdown-agents))