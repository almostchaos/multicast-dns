(ns dns.multicast.browse
  (:require
    [aleph.udp :refer [socket]]
    [clojure.string :as string]
    [clojure.pprint :refer [pprint]]
    [manifold.stream :refer [put! take! close!]]
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

(defn- encode-header [header]
  (byte-array [(bit-and 255 (bit-shift-right (:ID header) 8))
               (bit-and 255 (:ID header))
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

(defn- encode-message [header questions answers authority additional]
  (let [header-bytes
        (encode-header header)
        message-bytes
        (reduce
          (fn [result-bytes question]
            (let [path (:QNAME question)
                  question-bytes
                  (reduce
                    (fn [path-bytes name]
                      (let [name-bytes (to-byte-array name)
                            count-bytes (byte-array [(alength name-bytes)])]
                        (byte-array-concat path-bytes count-bytes name-bytes)))
                    result-bytes path)]
              (byte-array-concat result-bytes question-bytes [0])))
          [] questions)]
    (byte-array-concat header-bytes message-bytes)))

(defn- encode-srv-query-message [service-path]
  (let [header {:ID      256                                ;;2 bytes number
                :QR      (:disabled flag)
                :OPCODE  (:inverse-query op-code)
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
                :ARCOUNT 0}
        question [{:QNAME  service-path
                   :QTYPE  (:SRV resource-type)
                   :QCLASS (:IN q-class)}]
        answer []
        authority []
        additional []]
    (encode-message header question answer authority additional)))

(defn browse [protocol type & subtypes]
  (debug "starting browser ...")
  (let [name (service-path protocol type subtypes)
        message-bytes (encode-srv-query-message name)
        mdns-socket @(socket {:host "localhost" :port 10001})]

    (on-term-signal
      (info "stopping browser...")
      (close! mdns-socket)
      (info "stopped browser"))

    (future
      (let [receive-socket @(socket {:host multicast-host :port mdns-port :transport :epoll})]
        (debug "listening for mdns response ...")
        (pprint @(take! receive-socket))))

    (debug "sending mdns request")
    (put! mdns-socket {:host multicast-host :port mdns-port :message message-bytes})
    (debug "sent mdns request")
    (print-bytes message-bytes)

    message-bytes))

(defn -main [& args]
  (browse "tcp" "http"))