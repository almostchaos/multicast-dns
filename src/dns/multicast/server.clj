(ns dns.multicast.server
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [clojure.string :as string]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug error]]))

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
         close-socket :close} (socket address port
                                      (fn [_ _ message] (>!! messages message)))
        services (atom {})
        running (atom true)
        respond (fn [service-type]
                  (run! (fn [[instance instance-port txt]]
                          (try
                            (debug "sending response for" instance "port" instance-port)
                            (send address port (ptr-answer service-type instance 0 0 instance-port txt))
                            (catch Exception e
                              (error e))))
                        (get @services service-type)))
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

    {:advertise (fn [service-type service-instance port txt]
                  (swap! services update service-type
                         (fn [instances]
                           (if (nil? instances)
                             [[service-instance port txt]]
                             (cons [service-instance port txt] instances)))))
     :shutdown  (fn []
                  (debug "stopping...")
                  (swap! running not)
                  (close-socket)
                  (async/close! messages)
                  (debug "stopped listening"))}))

(defn -main [& args]
  (let [{advertise :advertise
         shutdown  :shutdown} (listen)]
    (advertise "_spotify-connect._tcp.local" "A" 36663 nil)
    (advertise "_spotify-connect._tcp.local" "B" 36663 nil)
    (future
      (Thread/sleep 36000)
      (println "shutting down...")
      (shutdown)
      (shutdown-agents))))