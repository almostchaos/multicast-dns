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

(defn- known-question? [question]
  (let [type (:QTYPE question)]
    (or
      (= type:PTR type)
      (= type:SRV type)
      (= type:TXT type))))

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
  (let [service-types (atom {})
        service-instances (atom {})
        running (atom true)
        queried-resources (async/chan 100)
        receive (fn [_ _ packet]
                  (let [message (decode-message packet)]
                    (when (query? message)
                      (->>
                        (rest message)
                        (filter known-question?)
                        (map (fn [question]
                               (let [type (:QTYPE question)
                                     resource (:QNAME question)]
                                 (cond
                                   (= type:PTR type) [ptr-answer resource]
                                   (= type:SRV type) [srv-answer resource]
                                   (= type:TXT type) [txt-answer resource]))))
                        (run! (partial >!! queried-resources))))))

        {send         :send
         close-socket :close} (socket bind-address port receive :multicast address)
        respond (fn [answer parameters]
                  (async/go
                    (let [random-delay (long (+ 20 (rand 100)))]
                      (try
                        (Thread/sleep random-delay)
                        (debug "sending response for" parameters)
                        (send address port (apply answer parameters))
                        (catch Exception e
                          (error "failed to send response for" parameters e))))))]
    (future
      (info "starting to listen...")
      (while @running
        ;(debug "...")
        (let [timed-exit (async/timeout 1000)
              resources (distinct (drain-channel-sequence queried-resources timed-exit))]
          (run!
            (fn [[answer resource]]
              (if (= answer ptr-answer)
                (when-let [instances (get @service-types resource)]
                  (run! (partial respond answer) (vals instances)))
                (when-let [instance (get @service-instances resource)]
                  (respond answer instance))))
            resources))))

    {:advertise (fn [service-type service-instance port txt]
                  (let [entry [service-type service-instance port txt]]
                    (swap! service-instances assoc (str service-instance "." service-type) entry)
                    (swap! service-types update service-type
                           (fn [instances]
                             (if (nil? instances)
                               {service-instance entry}
                               (assoc instances service-instance entry))))))
     :stop      (fn []
                  (debug "stopping...")
                  (swap! running not)
                  (close-socket)
                  (async/close! queried-resources)
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