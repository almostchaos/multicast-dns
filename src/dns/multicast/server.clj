(ns dns.multicast.server
  (:require
    [clj-commons.byte-streams :refer [print-bytes]]
    [clojure.core.async :as async :refer [<!! >!!]]
    [clojure.string :as string]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug info warn error]]))

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
  (let [messages (async/chan 100)
        {send         :send
         close-socket :close} (socket address port (fn [_ _ message]
                                                     (>!! messages message)))
        services (atom {})
        running (atom true)
        respond (fn [name]
                  (when-let [response (get @services name)]
                    (try
                      (let [message-bytes (ptr-answer name 0 0 36663)]
                        (debug "sending response to" name)
                        (send address port message-bytes))
                      (catch Exception e
                        (error e)))))
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
      (debug "starting to listen...")

      (while @running
        (debug "...")
        (let [queried-services (set (to-array (take 20 queries)))
              matching-services (filter (partial get @services) queried-services)]
          (debug "responding to queries:" (string/join ", " matching-services))
          (run! respond matching-services))
        (Thread/sleep 500)))

    {:register (fn [service-type service-instance port txt]
                 (swap! services assoc service-type [service-instance port txt]))
     :shutdown (fn []
                 (debug "stopping...")
                 (swap! running not)
                 (close-socket)
                 (async/close! messages)
                 (debug "stopped listening"))}))

(defn -main [& args]
  (let [{register :register
         shutdown :shutdown} (listen)]
    (register "_spotify-connect._tcp.local" "clojure" 36663 nil)
    (future
      (Thread/sleep 36000)
      (println "shutting down...")
      (shutdown)
      (shutdown-agents))))