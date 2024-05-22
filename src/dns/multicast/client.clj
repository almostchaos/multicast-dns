(ns dns.multicast.client
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer [a-query ptr-query]]
    [socket.io.udp :refer [socket]]))

(def port 5353)
(def address "224.0.0.251")

(defn- resource-type-matcher [type] (fn [section] (= type (:TYPE section))))
(def match-ptr (resource-type-matcher type:PTR))
(def match-a (resource-type-matcher type:A))
(def match-srv (resource-type-matcher type:SRV))

(defn- drain-channel-sequence [channel end]
  (lazy-seq
    (let [item (<!! channel)]
      (if (nil? item)
        (do (end) nil)
        (cons item (drain-channel-sequence channel end))))))

(defn name->ip [name]
  (let [messages (async/timeout 1000)
        receive (fn [_ _ message] (>!! messages message))
        {send :send close-socket :close} (socket "0.0.0.0" port receive :multicast address)]

    (send address port (a-query name))
    (->
      (->>
        (drain-channel-sequence messages close-socket)
        (map decode-message)
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
  (let [messages (async/chan 10)
        receive (fn [_ _ message] (>!! messages message))
        {send :send close-socket :close} (socket "0.0.0.0" port receive :multicast address)]

    (send address port (ptr-query service-path))
    ;listen a limited time for responses
    (future
      (Thread/sleep 2000)
      (async/close! messages))
    (set
      (->>
        (drain-channel-sequence messages close-socket)
        (map decode-message)
        (filter
          (fn [message]
            (and
              (some match-ptr message)
              (= service-path (-> (filter match-ptr message) (first) (:NAME))))))
        (map
          (fn [message]
            (-> (filter match-srv message) (first) (:NAME))))))))

(defn -main [& args]
  (run! println (service->names "_zzzzz._tcp.local"))
  (run! println (service->names "_googlecast._tcp.local"))
  (run! println (service->names "_octoprint._tcp.local"))
  (println (name->ip "octopi.local"))

  (shutdown-agents))