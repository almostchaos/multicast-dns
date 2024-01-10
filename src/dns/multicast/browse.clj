(ns dns.multicast.browse
  (:require
    [socket.io.udp :refer [socket]]
    [clj-commons.byte-streams :refer [to-string to-byte-array print-bytes]]
    [taoensso.timbre :refer [trace debug info warn error]]))

(defmacro on-term-signal [& handler]
  `(.addShutdownHook (Runtime/getRuntime)
                     (Thread. (fn []
                                (debug "sigterm captured")
                                ~@handler))))
(def flag {:disabled 0
           :enabled  1})

(def op-code {:query         0
              :inverse-query 1
              :status        2
              :reserved      3})

(def r-code {:no-error        0
             :format-error    1
             :server-failure  2
             :name-error      3
             :not-implemented 4
             :refused         5
             :reserved        6})

(def resource-type {:A    1
                    :PTR  12
                    :TXT  16
                    :AAAA 28
                    :SRV  33
                    :NSEC 47
                    :ANY  255})

(def q-class {:IN  1
              :CS  2
              :CH  3
              :HS  4
              :ANY 255})
(def mdns-port 5353)
(def multicast-host "224.0.0.251")

(defn- byte-array-concat [& byte-arrays]
  (byte-array (mapcat seq byte-arrays)))

(defn- service-path [protocol type subtypes]
  (let [prefix (if (nil? subtypes) [] (conj (map (partial str "_") subtypes) "sub"))
        path [(str "_" type) (str "_" protocol) "local"]]
    (concat prefix path)))

(defn- first-byte [num]
  (bit-and 255 num))

(defn- second-byte [num]
  (bit-and 255 (bit-shift-right num 8)))

(defn- encode-header [header]
  (byte-array [(second-byte (:ID header))
               (first-byte (:ID header))
               (-> 0
                   (bit-or (bit-shift-left (:QR header) 7))
                   (bit-or (bit-shift-left (:OPCODE header) 3))
                   (bit-or (bit-shift-left (:AA header) 2))
                   (bit-or (bit-shift-left (:TC header) 1))
                   (bit-or (:RD header)))
               (-> 0
                   (bit-or (bit-shift-left (:RA header) 7))
                   (bit-or (bit-shift-left (:Z header) 4))
                   (bit-or (:RCODE header)))
               ;;one question per message is assumed
               0 1 0 0 0 0 0 0]))

(defn- encode-question [path type class]
  (byte-array-concat
    (reduce
      (fn [path-bytes name]
        (let [name-bytes (to-byte-array name)
              count-bytes (byte-array [(alength name-bytes)])]
          (byte-array-concat path-bytes count-bytes name-bytes)))
      (byte-array 0) path)
    [0 (second-byte type) (first-byte type) (second-byte class) (first-byte class)]))

(defn- encode-srv-query-message [service-path]
  (byte-array-concat
    (encode-header {:ID      256                            ;;2 bytes number
                    :QR      (:disabled flag)
                    :OPCODE  (:query op-code)
                    :AA      (:disabled flag)
                    :TC      (:disabled flag)
                    :RD      (:disabled flag)
                    :RA      (:disabled flag)
                    :Z       (:disabled flag)
                    :AD      (:disabled flag)
                    :CD      (:disabled flag)
                    :RCODE   (:no-error r-code)
                    :QDCOUNT 1
                    :ANCOUNT 0
                    :NSCOUNT 0
                    :ARCOUNT 0})
    (encode-question service-path
                     (:PTR resource-type)
                     (:IN q-class))))

(defn browse [protocol type & subtypes]
  (debug "starting browser ...")
  (debug "listening for mdns response ...")

  (let [name (service-path protocol type subtypes)
        message-bytes (encode-srv-query-message name)

        {send :send close :close}
        (socket multicast-host mdns-port
                (fn [host port message]
                  (println "received [" host ":" port "] ------------")
                  (print-bytes message)))]

    (on-term-signal
      (info "stopping browser...")
      (close)
      (shutdown-agents)
      (info "stopped browser"))

    (future
      (Thread/sleep 5000)
      (debug "sending mdns request")
      (send multicast-host mdns-port message-bytes)
      (debug "sent mdns request"))))

(defn -main [& args]
  (browse "tcp" "spotify-connect"))