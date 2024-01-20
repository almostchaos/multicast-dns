(ns dns.message
  (:require
    [clojure.string :as string]
    [dns.encoding :refer :all]))

(defn srv-query [service-path]
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
    (encode-question (string/split service-path #"\.")
                     type:PTR
                     class:IN)))

(defn a-query [name]
  (let [service-path (string/split name #"\.")]
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
      (encode-question service-path
                       type:A
                       class:IN))))