(ns dns.message
  (:require
    [clojure.string :as string]
    [dns.encoding :refer :all]))

(defn srv-query [protocol type subtypes]
  (let [prefix (if (nil? subtypes) [] (conj (map (partial str "_") subtypes) "sub"))
        path [(str "_" type) (str "_" protocol) "local"]
        service-path (concat prefix path)]
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
                       resource-type:PTR
                       q-class:IN))))

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
                       resource-type:A
                       q-class:IN))))