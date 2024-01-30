(ns dns.multicast.server
  (:require
    [clj-commons.byte-streams :refer [print-bytes]]
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]))

(def port 5353)
(def address "224.0.0.251")

(defn encode-message [message]
  )

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

(defn serve []
  (let [messages (async/chan 10)
        services (atom {})
        enqueue (fn [_ _ message]
                  (>!! messages message))
        {send         :send
         close-socket :close} (socket address port enqueue)
        respond (fn [name]
                  (println "received" name)
                  (when-let [response (get @services name)]
                    (try
                      (let [message-bytes (ptr-answer name 0 0 5110)]
                        (println "sending...")
                        (print-bytes message-bytes)
                        (send address port message-bytes))
                      (catch Exception e (println e)))))]
    (future
      (run! respond
            (flatten
              (->>
                (drain-channel-sequence messages close-socket)
                (map decode-message)
                (filter ptr-query?)
                (map
                  (fn [message]
                    (let [header (first message)
                          sections (rest message)
                          query-count (:QDCOUNT header)
                          queries (take query-count sections)]
                      (map :QNAME queries))))))))
    {:register (fn [name]
                 (swap! services assoc name (keyword name)))
     :shutdown (fn []
                 (async/close! messages))}))

(defn -main [& args]
  (let [{register :register
         shutdown :shutdown} (serve)]
    (register "_spotify-connect._tcp.local")
    (future
      (Thread/sleep 36000)
      (println "shutting down...")
      (shutdown)
      (shutdown-agents))))