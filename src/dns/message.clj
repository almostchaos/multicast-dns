(ns dns.message
  (:require
    [clojure.string :refer [split]]
    [dns.encoding :refer :all]))

(def dot #"\.")

(defn ptr-query [service-path]
  (let [path (split service-path dot)]
    (byte-array-concat
      (encode-header
        :QR flag:disabled
        :OPCODE op-code:query
        :AA flag:disabled
        :TC flag:disabled
        :RD flag:disabled
        :RA flag:disabled
        :AD flag:disabled
        :CD flag:disabled
        :RCODE r-code:no-error
        :QDCOUNT 1
        :ANCOUNT 0
        :NSCOUNT 0
        :ARCOUNT 0)
      (encode-question path type:PTR class:IN))))

(defn a-query [name]
  (let [service-path (split name dot)]
    (byte-array-concat
      (encode-header
        :QR flag:disabled
        :OPCODE op-code:query
        :AA flag:disabled
        :TC flag:disabled
        :RD flag:disabled
        :RA flag:disabled
        :AD flag:disabled
        :CD flag:disabled
        :RCODE r-code:no-error
        :QDCOUNT 1
        :ANCOUNT 0
        :NSCOUNT 0
        :ARCOUNT 0)
      (encode-question service-path type:A class:IN))))

(defn ptr-answer [service-path instance & {hostname :host port :port txt :txt time-to-live :ttl}]
  (let [priority 0
        weight 0
        service-name (split service-path dot)
        service-instance (cons instance service-name)
        host (split hostname dot)
        ttl (or time-to-live 120)]

    (byte-array-concat
      (encode-header
        :QR flag:enabled
        :OPCODE op-code:query
        :AA flag:disabled
        :TC flag:disabled
        :RD flag:disabled
        :RA flag:disabled
        :AD flag:disabled
        :CD flag:disabled
        :RCODE r-code:no-error
        :QDCOUNT 0
        :ANCOUNT 1
        :NSCOUNT 0
        :ARCOUNT (if (empty? txt) 1 2))
      (encode-answer service-name type:PTR class:IN ttl (encode-name service-instance))
      (encode-answer service-instance type:SRV class:IN ttl (encode-srv-data host port priority weight))
      (when-not (empty? txt)
        (encode-answer service-instance type:TXT class:IN ttl (encode-txt-data txt))))))

(defn srv-answer [service-path instance & {hostname :host port :port txt :txt time-to-live :ttl}]
  (let [priority 0
        weight 0
        service-name (split service-path dot)
        service-instance (cons instance service-name)
        host (split hostname dot)
        ttl (or time-to-live 120)]
    (byte-array-concat
      (encode-header
        :QR flag:enabled
        :OPCODE op-code:query
        :AA flag:disabled
        :TC flag:disabled
        :RD flag:disabled
        :RA flag:disabled
        :AD flag:disabled
        :CD flag:disabled
        :RCODE r-code:no-error
        :QDCOUNT 0
        :ANCOUNT 1
        :NSCOUNT 0
        :ARCOUNT (if (empty? txt) 0 1))
      (encode-answer service-instance type:SRV class:IN ttl (encode-srv-data host port priority weight))
      (when-not (empty? txt)
        (encode-answer service-instance type:TXT class:IN ttl (encode-txt-data txt))))))

(defn txt-answer [service-path instance & {txt :txt time-to-live :ttl}]
    (let [service-name (split service-path dot)
          service-instance (cons instance service-name)
          ttl (or time-to-live 120)]
      (byte-array-concat
        (encode-header
          :QR flag:enabled
          :OPCODE op-code:query
          :AA flag:disabled
          :TC flag:disabled
          :RD flag:disabled
          :RA flag:disabled
          :AD flag:disabled
          :CD flag:disabled
          :RCODE r-code:no-error
          :QDCOUNT 0
          :ANCOUNT 1
          :NSCOUNT 0
          :ARCOUNT 1)
        (encode-answer service-instance type:TXT class:IN ttl (encode-txt-data txt)))))