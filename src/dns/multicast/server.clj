(ns dns.multicast.server
  (:require
    [clj-commons.byte-streams :refer [print-bytes]]
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]))

(def port 5353)
(def address "224.0.0.251")

(defn- ptr-query? [message]
  (and
    (not (:QR (first message)))
    (= type:PTR (:QTYPE (second message)))))

(defn- drain-channel-sequence [channel end]
  (lazy-seq
    (let [item (<!! channel)]
      (if (nil? item)
        (do (end) nil)
        (cons item (drain-channel-sequence channel end))))))

(defn listen []
  (let [messages (async/chan 10)
        {send         :send
         close-socket :close} (socket address port (fn [_ _ message]
                                                     (>!! messages message)))
        services (atom {})
        running (atom true)
        respond (fn [name]
                  (println "received" name)
                  (when-let [response (get @services name)]
                    (try
                      (let [message-bytes (ptr-answer name 0 0 36663)]
                        (println "sending...")
                        (print-bytes message-bytes)
                        (send address port message-bytes))
                      (catch Exception e (println e)))))
        queries (flatten
                  (->>
                    (drain-channel-sequence messages close-socket)
                    (map decode-message)
                    (filter ptr-query?)
                    (map (fn [message]
                           (let [query-count (:QDCOUNT (first message))
                                 message-queries (take query-count (rest message))]
                             (map :QNAME message-queries))))))]
    (future
      (while @running
        (Thread/sleep 300)
        (run! respond queries)))

    {:register (fn [service-type service-instance port txt]
                 (swap! services assoc service-type [service-instance port txt]))
     :shutdown (fn []
                 (swap! running not)
                 (async/close! messages))}))

(defn -main [& args]
  (let [{register :register
         shutdown :shutdown} (listen)]
    (register "_spotify-connect._tcp.local" "clojure" 36663 nil)
    (future
      (Thread/sleep 36000)
      (println "shutting down...")
      (shutdown)
      (shutdown-agents))))