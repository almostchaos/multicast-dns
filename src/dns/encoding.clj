(ns dns.encoding
  (:require
    [clj-commons.byte-streams :refer [to-byte-array to-string]]
    [clojure.string :as string]))

(defn byte-array-concat [& byte-arrays]
  (byte-array (mapcat seq byte-arrays)))

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

(defn long->byte [value]
  (byte (if (> value 127) (- value 256) value)))

(defn- bits->byte [bits]
  (long->byte
    (reduce
      (fn [result [index bit]]
        (if bit
          (bit-set result index)
          result))
      0
      (map-indexed
        (fn [index bit] [index bit])
        (reverse (drop (- (count bits) 8) bits))))))

(defn- rand-byte []
  (byte (- (rand-int 255) 128)))

(defn long->byte-array [long-value]
  (loop [index 0
         value long-value
         previous nil]
    (if (< index 8)
      (recur
        (inc index)
        (bit-shift-right value 8)
        (cons (long->byte (bit-and 0xff value)) previous))
      previous)))

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
        qr (:QR header)
        [op-code-3 op-code-2 op-code-1 op-code-0] (:OPCODE header)
        aa (:AA header)
        tc (:TC header)
        rd (:RD header)
        ra (:RA header)
        [r-code-3 r-code-2 r-code-1 r-code-0] (:RCODE header)
        [qd-count-ms qd-count-ls] (drop 6 (long->byte-array (:QDCOUNT header)))
        [an-count-ms an-count-ls] (drop 6 (long->byte-array (:ANCOUNT header)))
        [ns-count-ms ns-count-ls] (drop 6 (long->byte-array (:NSCOUNT header)))
        [ar-count-ms ar-count-ls] (drop 6 (long->byte-array (:ARCOUNT header)))]
    (byte-array
      (let [value (bits->byte [qr op-code-3 op-code-2 op-code-1 op-code-0 aa tc rd])]
        [(rand-byte)
         (rand-byte)
         value
         (bits->byte [ra false false false r-code-3 r-code-2 r-code-1 r-code-0])
         qd-count-ms qd-count-ls
         an-count-ms an-count-ls
         ns-count-ms ns-count-ls
         ar-count-ms ar-count-ls]))))

(defn encode-question [path type class]
  (byte-array-concat
    (reduce
      (fn [path-bytes name]
        (let [name-bytes (to-byte-array name)
              count-bytes (byte-array [(alength name-bytes)])]
          (byte-array-concat path-bytes count-bytes name-bytes)))
      (byte-array 0) path)
    [0 0 type 0 class]))

(defn encode-name [path]
  (byte-array-concat
    (flatten
      (map (fn [name]
             (cons
               (long->byte (count name))
               (to-byte-array name)))
           path))
    [0]))

(defn encode-address-data [ip]
  (byte-array (->> ip (map parse-long) (map long->byte))))

(defn encode-srv-data [host port priority weight]
  (let [[priority-ms priority-ls] (drop 6 (long->byte-array priority))
        [weight-ms weight-ls] (drop 6 (long->byte-array weight))
        [port-ms port-ls] (drop 6 (long->byte-array port))]
    (byte-array-concat
      [priority-ms priority-ls
       weight-ms weight-ls
       port-ms port-ls]
      (encode-name host))))

(defn encode-txt-data [properties]
  (byte-array
    (flatten
      (map (fn [[key value]]
             (let [property (str (name key) "=" value)]
               (cons
                 (long->byte (count property))
                 (to-byte-array property))))
           properties))))

(defn encode-answer [service type class ttl data]
  (let [rd-length (alength data)
        [type-ms type-ls] (drop 6 (long->byte-array type))
        [class-ms class-ls] (drop 6 (long->byte-array class))
        [ttl-3 ttl-2 ttl-1 ttl-0] (drop 4 (long->byte-array ttl))
        [rd-length-ms rd-length-ls] (drop 6 (long->byte-array rd-length))
        name (encode-name service)]
    (byte-array-concat
      name
      [type-ms type-ls
       class-ms class-ls
       ttl-3 ttl-2 ttl-1 ttl-0
       rd-length-ms rd-length-ls]
      data)))

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
   ;;stop if index is out of bounds
   (if (> (alength message) start)
     (let [first-byte (nth message start)]
       (cond
         (= 0 first-byte)
         (string/join "." path)
         (= 2r11000000 (bit-and 2r11000000 first-byte))
         (let [next-start-ms (bit-and 2r00111111 first-byte)
               next-start-ls (nth message (inc start))
               next-start (byte-array->long [next-start-ms next-start-ls])]
           (decode-name next-start message path))
         :else
         (let [label-length (byte->long first-byte)
               label-bytes (take label-length (drop (inc start) message))
               label (to-string (byte-array label-bytes))
               next-start (+ start 1 label-length)]
           (decode-name next-start message (concat path [label])))))
     path)))

(defn- decode-address-data [start message]
  (string/join "." (map byte->long (take 4 (drop start message)))))

(defn- decode-srv-data [start message]
  (let [data (drop start message)
        priority (byte-array->long (take 2 data))
        weight (byte-array->long (take 2 (drop 2 data)))
        port (byte-array->long (take 2 (drop 4 data)))
        host (decode-name (+ start 6) message)]
    {:priority priority :weight weight :port port :host host}))

(defn- kv->tuple [kv]
  (let [tuple (string/split kv #"={1}")]
    [(first tuple) (if (< (count tuple) 2)
                     ""
                     (last tuple))]))

(defn- decode-txt-data [start length message]
  (let [data (take length (drop start message))
        texts (loop [input data
                     result nil]
                (let [part-length (first input)
                      part (take part-length (drop 1 input))
                      txt (to-string (byte-array part))
                      input-rest (drop (+ 1 part-length) input)
                      result-partial (cons txt result)]
                  (if (empty? input-rest)
                    result-partial
                    (recur input-rest result-partial))))]
    (apply array-map (flatten (map kv->tuple texts)))))

(defn- name-storage-size [start message]
  (if (> (alength message) start)
    (loop [position start count 0]
      (let [value (nth message position)]
        (cond
          (= 0 value) (+ 1 count)
          (= 2r11000000 (bit-and 2r11000000 value)) (+ 2 count)
          :else (recur (inc position) (inc count)))))
    0))

(defn- decode-data [type start length message]
  (cond
    (= type type:PTR) (decode-name start message)
    (= type type:SRV) (decode-srv-data start message)
    (= type type:NSEC) (decode-name start message)
    (= type type:CNAME) (decode-name start message)
    (= type type:A) (decode-address-data start message)
    (= type type:TXT) (decode-txt-data start length message)
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
        additional-record-count (:ARCOUNT header)
        body (decode-sections 12 message question-count (+ additional-record-count answer-count))]
    (concat [header] body)))