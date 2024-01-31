(ns dns.message
  (:require
    [clj-commons.byte-streams :refer [to-byte-array]]
    [clojure.string :as string]
    [dns.encoding :refer :all])
  (:import (java.net InetAddress)))

(defn- hostname []
  (.getHostName (InetAddress/getLocalHost)))

(defn- ip-address []
  (.getHostAddress (InetAddress/getLocalHost)))

(defn ptr-query [service-path]
  (let [path (string/split service-path #"\.")]
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
  (let [service-path (string/split name #"\.")]
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

(defn ptr-answer [service-path instance priority weight port txt]
  (let [service-name (string/split service-path #"\.")
        service-instance (cons instance service-name)
        host (string/split (hostname) #"\.")
        ip (string/split (ip-address) #"\.")
        ttl 120
        [priority-ms priority-ls] (drop 6 (long->byte-array priority))
        [weight-ms weight-ls] (drop 6 (long->byte-array weight))
        [port-ms port-ls] (drop 6 (long->byte-array port))]
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
        :ARCOUNT (if (empty? txt) 2 3))
      (encode-answer service-name type:PTR class:IN ttl (encode-name service-instance))
      (encode-answer service-instance type:SRV class:IN ttl
                     (byte-array-concat
                       [priority-ms priority-ls
                        weight-ms weight-ls
                        port-ms port-ls]
                       (encode-name host)))
      (encode-answer host type:A class:IN ttl (encode-address ip))
      (when-not (empty? txt)
        (encode-answer service-instance type:TXT class:IN ttl (encode-txt txt))))))