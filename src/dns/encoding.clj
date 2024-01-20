(ns dns.encoding
  (:require
    [clj-commons.byte-streams :refer [to-byte-array to-string]]
    [clojure.string :as string]))

(defn byte-array-concat [& byte-arrays]
  (byte-array (mapcat seq byte-arrays)))

(defn bits->byte [& bits]
  (byte
    (reduce
      (fn [result [index bit]]
        (bit-or (bit-shift-left (if bit 1 0) index) result))
      0
      (map-indexed
        (fn [index bit] [index bit])
        (take 8 bits)))))

(defn- byte->bits [value]
  (boolean-array (reverse (map (partial bit-test value) (range 8)))))

(defn- byte->4-bits [value]
  (boolean-array (take 4 (byte->bits value))))

(defn- byte->long [b]
  (Byte/toUnsignedLong b))

(defn- byte-array->long [bytes]
  (reduce
    (fn [result b]
      (bit-or (byte->long b) (bit-shift-left result 8))) 0 bytes))

(defn- rand-byte []
  (byte (- (rand-int 256) 128)))

(def flag:disabled false)
(def flag:enabled true)
(def op-code:query (byte->4-bits 0))
(def op-code:inverse-query (byte->4-bits 1))
(def op-code:status (byte->4-bits 2))
(def op-code:reserved (byte->4-bits 3))
(def r-code:no-error (byte->4-bits 0))
(def r-code:format-error (byte->4-bits 1))
(def r-code:server-failure (byte->4-bits 2))
(def r-code:name-error (byte->4-bits 3))
(def r-code:not-implemented (byte->4-bits 4))
(def r-code:refused (byte->4-bits 5))
(def r-code:reserved (byte->4-bits 6))
(def type:A 1)
(def type:NS 2)
(def type:CNAME 5)
(def type:PTR 12)
(def type:TXT 16)
(def type:AAAA 28)
(def type:SRV 33)
(def type:NSEC 47)
(def type:ANY 255)
(def class:IN 1)

(defn encode-header [& parameters]
  (let [header (apply hash-map parameters)
        qp-code (:OPCODE header)
        r-code (:RCODE header)
        qr (:QR header)
        aa (:AA header)
        tc (:TC header)
        rd (:RD header)
        ra (:RA header)]
    (byte-array
      [(rand-byte)
       (rand-byte)
       (bits->byte
         qr (nth qp-code 3) (nth qp-code 2) (nth qp-code 1) (nth qp-code 0) aa tc rd)
       (bits->byte
         ra false false false (nth r-code 3) (nth r-code 2) (nth r-code 1) (nth r-code 0))
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

(defn- decode-header [header-bytes]
  (let [[id-ms id-ls
         flags-first flags-second
         qd-count-ms qd-count-ls
         an-count-ms an-count-ls
         ns-count-ms ns-count-ls
         ar-count-ms ar-count-ls] header-bytes
        [qr qp-code-1 qp-code-2 qp-code-3
         qp-code-4 aa tc rd] (byte->bits flags-first)
        [ra z1 z2 z3
         r-code-1 r-code-2 r-code-3 r-code-4] (byte->bits flags-second)]
    {:ID      (byte-array->long [id-ms id-ls])
     :QR      qr
     :OPCODE  (bits->byte [qp-code-1 qp-code-2 qp-code-3 qp-code-4])
     :AA      aa
     :TC      tc
     :RD      rd
     :RA      ra
     :RCODE   (bits->byte [r-code-1 r-code-2 r-code-3 r-code-4])
     :QDCOUNT (byte-array->long [qd-count-ms qd-count-ls])
     :ANCOUNT (byte-array->long [an-count-ms an-count-ls])
     :NSCOUNT (byte-array->long [ns-count-ms ns-count-ls])
     :ARCOUNT (byte-array->long [ar-count-ms ar-count-ls])}))

(defn- decode-name
  ([start message]
   (decode-name start message []))
  ([start message path]
   ;;stop if index is outside byte array
   (if (>= (alength message) start)
     (let [first-byte (nth message start)]
       (cond
         (= 0 first-byte) (string/join "." path)
         (>= -63 first-byte) (let [next-start-ms (bit-and 2r00111111 first-byte)
                                   next-start-ls (nth message (inc start))
                                   next-start (byte-array->long [next-start-ms next-start-ls])]
                               (decode-name next-start message path))
         :else (let [label-bytes (take first-byte (drop (inc start) message))
                     label (to-string (byte-array label-bytes))
                     next-start (+ start 1 first-byte)]
                 (decode-name next-start message (concat path [label])))))
     path)))

(defn- name-storage-size [start message]
  (loop [position start count 0]
    (let [value (nth message position)]
      (cond
        (= 0 value) (+ 1 count)
        (= -64 value) (+ 2 count)
        :else (recur (inc position) (inc count))))))

(defn- decode-data [type start length message]
  (cond
    (= type type:PTR) (decode-name start message)
    (= type type:NSEC) (decode-name start message)
    (= type type:CNAME) (decode-name start message)
    (= type type:A) (string/join "." (map byte->long (take 4 (drop start message))))
    (= type type:TXT) (to-string (byte-array (take length (drop start message))))
    :else (byte-array (take length (drop start message)))))

(defn- decode-sections [position message question-count answer-count]
  (if (> (alength message) position)
    (let [name (decode-name position message)
          name-length (name-storage-size position message)
          name-end (+ position name-length)
          bytes-after-name (drop name-end message)
          type (byte-array->long (take 2 bytes-after-name))
          bytes-after-type (drop 2 bytes-after-name)
          class (byte-array->long (take 2 bytes-after-type))]

      (if (> question-count 0)
        (let [question {:QNAME name :QTYPE type :QCCLASS class}
              next-position (+ name-end 2 2)
              remaining-question-count (dec question-count)]
          (concat [question]
                  (decode-sections next-position message remaining-question-count answer-count)))

        (if (> answer-count 0)
          (let [bytes-after-class (drop 2 bytes-after-type)
                ttl (byte-array->long (take 4 bytes-after-class))
                bytes-after-ttl (drop 4 bytes-after-class)
                data-length (byte-array->long (take 2 bytes-after-ttl))
                data-start (+ name-end 2 2 4 2)
                data (decode-data type data-start data-length message)
                answer {:NAME name :TYPE type :CLASS class :TTL ttl :RDLENGTH data-length :RDATA data}
                next-position (+ data-start data-length)
                remaining-answer-count (dec answer-count)]

            (concat [answer]
                    (decode-sections next-position message 0 remaining-answer-count)))
          [])))))

(defn decode-message [message]
  (let [header (decode-header (take 12 message))
        question-count (:QDCOUNT header)
        answer-count (:ANCOUNT header)
        body (decode-sections 12 message question-count answer-count)]
    (concat [header] body)))