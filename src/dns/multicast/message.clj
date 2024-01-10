(ns dns.multicast.message
  (:require
    [clj-commons.byte-streams :refer [to-string to-byte-array print-bytes]]))

(defn- byte-array-concat [& byte-arrays]
  (byte-array (mapcat seq byte-arrays)))

(defn- bits-to-byte [& bits]
  (byte
    (reduce
      (fn [result [index bit]]
        (bit-or (bit-shift-left (if bit 1 0) index) result))
      0
      (map-indexed
        (fn [index bit]
          [index bit])
        (take 8 (flatten bits))))))

(defn- byte-to-bits [value]
  (reverse (map (partial bit-test value) (range 8))))

(defn- byte-to-4-bits [value]
  (take 4 (byte-to-bits value)))

(def flag {:disabled false
           :enabled  true})

(def op-code {:query         (byte-to-4-bits 0)
              :inverse-query (byte-to-4-bits 1)
              :status        (byte-to-4-bits 2)
              :reserved      (byte-to-4-bits 3)})

(def r-code {:no-error        (byte-to-4-bits 0)
             :format-error    (byte-to-4-bits 1)
             :server-failure  (byte-to-4-bits 2)
             :name-error      (byte-to-4-bits 3)
             :not-implemented (byte-to-4-bits 4)
             :refused         (byte-to-4-bits 5)
             :reserved        (byte-to-4-bits 6)})

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
    (encode-header :QR (:disabled flag)
                   :OPCODE (:query op-code)
                   :AA (:disabled flag)
                   :TC (:disabled flag)
                   :RD (:disabled flag)
                   :RA (:disabled flag)
                   :AD (:disabled flag)
                   :CD (:disabled flag)
                   :RCODE (:no-error r-code))
    (encode-question (service-path protocol type subtypes)
                     (:PTR resource-type)
                     (:IN q-class))))