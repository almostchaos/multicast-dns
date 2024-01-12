(ns dns.multicast.message
  (:require
    [clj-commons.byte-streams :refer [to-byte-array]]))

(defn- byte-array-concat [& byte-arrays]
  (byte-array (mapcat seq byte-arrays)))

(defn bits-to-byte [& bits]
  (byte
    (reduce
      (fn [result [index bit]]
        (bit-or (bit-shift-left (if bit 1 0) index) result))
      0
      (map-indexed
        (fn [index bit] [index bit])
        (take 8 (flatten bits))))))

(defn- byte-to-bits [value]
  (boolean-array (reverse (map (partial bit-test value) (range 8)))))

(defn- byte-to-4-bits [value]
  (boolean-array (take 4 (byte-to-bits value))))

(defn- bytes-to-int [bytes]
  (reduce
    (fn [result b]
      (bit-or b (bit-shift-left result 8))) 0 bytes))

(def flag:disabled false)
(def flag:enabled true)
(def op-code:query (byte-to-4-bits 0))
(def op-code:inverse-query (byte-to-4-bits 1))
(def op-code:status (byte-to-4-bits 2))
(def op-code:reserved (byte-to-4-bits 3))
(def r-code:no-error (byte-to-4-bits 0))
(def r-code:format-error (byte-to-4-bits 1))
(def r-code:server-failure (byte-to-4-bits 2))
(def r-code:name-error (byte-to-4-bits 3))
(def r-code:not-implemented (byte-to-4-bits 4))
(def r-code:refused (byte-to-4-bits 5))
(def r-code:reserved (byte-to-4-bits 6))
(def resource-type:A 1)
(def resource-type:PTR 12)
(def resource-type:TXT 16)
(def resource-type:AAAA 28)
(def resource-type:SRV 33)
(def resource-type:NSEC 47)
(def resource-type:ANY 255)
(def q-class:IN 1)
(def q-class:CS 2)
(def q-class:CH 3)
(def q-class:HS 4)
(def q-class:ANY 255)

(defn service-path [protocol type subtypes]
  (let [prefix (if (nil? subtypes) [] (conj (map (partial str "_") subtypes) "sub"))
        path [(str "_" type) (str "_" protocol) "local"]]
    (concat prefix path)))

(defn encode-header [& parameters]
  (let [header (apply hash-map parameters)]
    (byte-array [;;ID
                 (rand-int 255)
                 (rand-int 255)
                 (bits-to-byte
                   (:QR header)
                   (:OPCODE header)
                   (:AA header)
                   (:TC header)
                   (:RD header))
                 (bits-to-byte
                   (:RA header)
                   ;;Z flag/reserved
                   false false false
                   (:RCODE header))
                 ;;one question per message is assumed
                 0 1 0 0 0 0 0 0])))

(defn encode-question [path type class]
  (byte-array-concat
    (reduce
      (fn [path-bytes name]
        (let [name-bytes (to-byte-array name)
              count-bytes (byte-array [(alength name-bytes)])]
          (byte-array-concat path-bytes count-bytes name-bytes)))
      (byte-array 0) path)
    [0 0 type 0 class]))

(defn encode-srv-query-message [protocol type subtypes]
  (byte-array-concat
    (encode-header :QR flag:disabled
                   :OPCODE op-code:query
                   :AA flag:disabled
                   :TC flag:disabled
                   :RD flag:disabled
                   :RA flag:disabled
                   :AD flag:disabled
                   :CD flag:disabled
                   :RCODE r-code:no-error)
    (encode-question (service-path protocol type subtypes)
                     resource-type:SRV
                     q-class:IN)))

(defn decode-header [header-bytes]
  (let [[id-ms id-ls
         flags-first flags-second
         qd-count-ms qd-count-ls
         an-count-ms an-count-ls
         ns-count-ms ns-count-ls
         ar-count-ms ar-count-ls] header-bytes
        [qp-code-1 qp-code-2 qp-code-3 qp-code-4
         aa tc rd] (byte-to-bits flags-first)
        [ra z1 z2 z3
         r-code-1 r-code-2 r-code-3 r-code-4] (byte-to-bits flags-second)]
    {:ID      (bytes-to-int [id-ms id-ls])
     :OPCODE  (bits-to-byte [qp-code-1 qp-code-2 qp-code-3 qp-code-4])
     :AA      aa
     :TC      tc
     :RD      rd
     :RA      ra
     :RCODE   (bits-to-byte [r-code-1 r-code-2 r-code-3 r-code-4])
     :QDCOUNT (bytes-to-int [qd-count-ms qd-count-ls])
     :ANCOUNT (bytes-to-int [an-count-ms an-count-ls])
     :NSCOUNT (bytes-to-int [ns-count-ms ns-count-ls])
     :ARCOUNT (bytes-to-int [ar-count-ms ar-count-ls])}))