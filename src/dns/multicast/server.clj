(ns dns.multicast.server
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug error]]))

(def port 5353)
(def address "224.0.0.251")

(defn- query? [message]
  (not (:QR (first message))))

(defn- ptr-question? [section]
  (= type:PTR (:QTYPE section)))

(defn- drain-channel-sequence [channel end]
  (lazy-seq
    (let [item (<!! channel)]
      (if (nil? item)
        (do (end) nil)
        (cons item (drain-channel-sequence channel end))))))

(defn listen []
  (let [services (atom {})
        running (atom true)
        queried-services (async/chan 100)
        receive (fn [_ _ packet]
                  (let [message (decode-message packet)]
                    (when (query? message)
                      (->>
                        (rest message)
                        (filter ptr-question?)
                        (map :QNAME)
                        (run! (partial >!! queried-services))))))

        {send         :send
         close-socket :close} (socket address port receive)
        respond (fn [service-type service-instances]
                  (run! (fn [[instance instance-port txt]]
                          (try
                            (debug "sending response for" instance "port" instance-port)
                            (send address port (ptr-answer service-type instance 0 0 instance-port txt))
                            (catch Exception e
                              (error e))))
                        service-instances))]
    (future
      (debug "starting to listen...")

      (while @running
        (debug "...")
        (loop [service-type (<!! queried-services)]
          (when-let [matching-services (get @services service-type)]
            (respond service-type matching-services)
            (recur (<!! queried-services))))
        (Thread/sleep 2000)))

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
                  (async/close! queried-services)
                  (debug "stopped listening"))}))

(defn -main [& args]
  (let [{advertise :advertise
         shutdown  :shutdown} (listen)]
    (advertise "_zzzzz._tcp.local" "A" 36663 {:path "/a"})
    (advertise "_zzzzz._tcp.local" "B" 36663 {:path "/b" :q 0})
    (future
      (Thread/sleep 36000)
      (println "shutting down...")
      (shutdown)
      (shutdown-agents))))