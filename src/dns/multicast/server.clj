(ns dns.multicast.server
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [socket.io.udp :refer [socket]]))

(def port 5353)
(def address "224.0.0.251")

(defn encode-message [message]
  )

(defn- ptr-query? [message]
  (and
    (not (:QR (first message)))
    (= type:PTR (:QTYPE (second message)))))

(defn- result-sequence [messages end-callback]
  (lazy-seq
    (let [message (<!! messages)]
      (if (nil? message)
        (do
          (end-callback)
          nil)
        (cons message (result-sequence messages end-callback))))))

(defn serve []
  (let [messages (async/chan 10)
        services (atom {})
        enqueue (fn [_ _ message]
                  (>!! messages message))
        {send         :send
         close-socket :close} (socket address port enqueue)
        respond (fn [name]
                  (when-let [response (get @services name)]
                    ;(send address port (encode-message message))
                    (println "sending" response)))]
    (future
      (run! respond
            (flatten
              (->>
                (result-sequence messages close-socket)
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