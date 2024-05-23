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

(defn- query->answers [message]
  (let [messages (async/chan 10)
        receive (fn [_ _ message] (>!! messages message))
        {send :send close-socket :close} (socket "0.0.0.0" port receive :multicast address)]

    (send address port message)
    ;listen a limited time for responses
    (future
      (Thread/sleep 2000)
      (async/close! messages))
    (->>
      (drain-channel-sequence messages close-socket)
      (map decode-message))))

(defn name->ip [name]
  (->
    (->>
      (query->answers (a-query name))
      (filter
        (fn [message]
          (= name (-> (filter match-a message) first :NAME))))
      (map
        (fn [message]
          (-> (filter match-a message) first :RDATA))))
    to-array
    first))

(defn service->names [service-path]
  (set
    (->>
      (query->answers (ptr-query service-path))
      (filter
        (fn [message]
          (and
            (some match-ptr message)
            (= service-path (-> (filter match-ptr message) first :NAME)))))
      (map
        (fn [message]
          (-> (filter match-srv message) first))))))

(defn -main [& args]
  (run! println (service->names "_zzzzz._tcp.local"))
  (run! println (service->names "_googlecast._tcp.local"))
  (run! println (service->names "_octoprint._tcp.local"))
  (println (name->ip "octopi.local"))

  (shutdown-agents))