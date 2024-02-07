(ns dns.multicast.server
  (:require
    [clojure.core.async :as async :refer [>!! alts!!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug error info]]))

(def port 5353)
(def address "224.0.0.251")

(defn- query? [message]
  (not (:QR (first message))))

(defn- ptr-question? [section]
  (= type:PTR (:QTYPE section)))

(defmacro on-term-signal [& handler]
  `(.addShutdownHook
     (Runtime/getRuntime)
     (new Thread
          (fn []
            (debug "sigterm captured")
            ~@handler))))

(defn- drain-channel-sequence [channel exit]
  (lazy-seq
    (let [[item channel] (alts!! [channel exit])]
      (when item
        (cons item (drain-channel-sequence channel exit))))))

(defn listen [bind-address]
  (let [services (atom {})
        running (atom true)
        queried-service-types (async/chan 100)
        receive (fn [_ _ packet]
                  (let [message (decode-message packet)]
                    (when (query? message)
                      (->>
                        (rest message)
                        (filter ptr-question?)
                        (map :QNAME)
                        (run! (partial >!! queried-service-types))))))

        {send         :send
         close-socket :close} (socket bind-address port receive :multicast address)
        respond (fn [service-type service-instances]
                  (run! (fn [[instance instance-port txt]]
                          (async/go
                            (let [full-instance-name (str instance "." service-type)
                                  random-delay (long (+ 20 (rand 100)))]
                              (try
                                (Thread/sleep random-delay)
                                (debug "sending response for" full-instance-name)
                                (send address port (ptr-answer service-type instance 0 0 instance-port txt))
                                (catch Exception e
                                  (error "failed to send response for" full-instance-name e))))))
                        service-instances))]
    (future
      (info "starting to listen...")
      (while @running
        ;(debug "...")
        (let [timed-exit (async/timeout 1000)
              service-types (distinct (drain-channel-sequence queried-service-types timed-exit))]
          (run!
            (fn [service-type]
              (when-let [service-instances (get @services service-type)]
                (respond service-type (vals service-instances))))
            service-types))))

    {:advertise (fn [service-type service-instance port txt]
                  (swap! services update service-type
                         (fn [instances]
                           (let [entry [service-instance port txt]]
                             (if (nil? instances)
                               {service-instance entry}
                               (assoc instances service-instance entry))))))
     :stop      (fn []
                  (debug "stopping...")
                  (swap! running not)
                  (close-socket)
                  (async/close! queried-service-types)
                  (debug "stopped listening"))}))

(defn -main [& args]
  (let [{advertise :advertise
         shutdown  :stop} (listen "0.0.0.0")]
    (advertise "_zzzzz._tcp.local" "B" 36663 {:path "/b" :q 0})
    (advertise "_airplay._tcp.local" "A" 36663 {})
    (advertise "_spotify-connect._tcp.local" "A" 36663 {})
    (advertise "_googlecast._tcp.local" "A" 36663 {})
    (advertise "_octoprint._tcp.local" "A" 36663 {})
    (on-term-signal
      (info "shutting down...")
      (shutdown)
      (shutdown-agents))))