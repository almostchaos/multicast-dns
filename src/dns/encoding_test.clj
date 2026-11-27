(ns dns.encoding-test
  (:require [clojure.test :refer :all]
            [dns.encoding :refer :all]
            [clj-commons.byte-streams :refer [to-byte-array]]))

(deftest byte-array-concat-test
  (is (= [1 2 3 4] (seq (byte-array-concat (byte-array [1 2]) (byte-array [3 4])))))
  (is (= [1 2] (seq (byte-array-concat (byte-array [1 2]) (byte-array 0)))))
  (is (or (nil? (seq (byte-array-concat (byte-array 0))))
          (= [] (seq (byte-array-concat (byte-array 0)))))))

(deftest long-byte-conversion-test
  (testing "long->byte"
    (is (= (byte 0) (long->byte 0)))
    (is (= (byte 127) (long->byte 127)))
    (is (= (byte -128) (long->byte 128)))
    (is (= (byte -1) (long->byte 255))))

  (testing "long->byte-array"
    (is (= [0 0 0 0 0 0 0 0] (map int (long->byte-array 0))))
    (is (= [0 0 0 0 0 0 0 1] (map int (long->byte-array 1))))
    (is (= [0 0 0 0 0 0 1 0] (map int (long->byte-array 256))))))

(deftest name-encoding-test
  (testing "encode-name"
    (is (= [3 119 119 119 0] (seq (encode-name ["www"]))))
    (is (= [3 119 119 119 5 108 111 99 97 108 0] (seq (encode-name ["www" "local"]))))))

(deftest address-encoding-test
  (testing "encode-address-data"
    (is (= [192 168 1 1] (map #(bit-and 0xff %) (encode-address-data ["192" "168" "1" "1"]))))))

(deftest txt-encoding-test
  (testing "encode-txt-data"
    (let [props {:key "val"}
          encoded (encode-txt-data props)]
      (is (= [7 107 101 121 61 118 97 108] (map #(bit-and 0xff %) encoded)))))
  (testing "encode-txt-data with empty map or nil"
    (is (= [0] (seq (encode-txt-data {}))))
    (is (= [0] (seq (encode-txt-data nil)))))
  (testing "encode-txt-data uses UTF-8 byte lengths"
    (let [props {:city "München" :note "🎵"}
          encoded (encode-txt-data props)
          header (encode-header :ID 0 :QR flag:enabled :OPCODE op-code:query
                                :AA flag:enabled :TC flag:disabled :RD flag:disabled
                                :RA flag:disabled :RCODE r-code:no-error
                                :QDCOUNT 0 :ANCOUNT 1 :NSCOUNT 0 :ARCOUNT 0)
          answer (encode-answer ["svc" "local"] type:TXT class:IN 120 encoded)
          decoded (decode-message (byte-array-concat header answer))]
      ;; "city=München" is 12 chars / 13 UTF-8 bytes; "note=🎵" is 6 chars / 9 UTF-8 bytes
      (is (= 13 (bit-and 0xff (nth encoded 0))))
      (is (= 9 (bit-and 0xff (nth encoded 14))))
      (is (= {"city" "München" "note" "🎵"} (:RDATA (second decoded)))))))

(deftest srv-encoding-test
  (testing "encode-srv-data"
    (let [encoded (encode-srv-data ["host" "local"] 8080 10 20)]
      ;; priority (10) -> [0 10]
      ;; weight (20) -> [0 20]
      ;; port (8080) -> [31 144] (8080 = 31 * 256 + 144)
      ;; host ["host" "local"] -> [4 104 111 115 116 5 108 111 99 97 108 0]
      (is (= [0 10 0 20 31 144 4 104 111 115 116 5 108 111 99 97 108 0] 
             (map #(bit-and 0xff %) encoded))))))

(deftest header-encoding-decoding-test
  (testing "encode-header and decode-header round-trip"
    (let [header-params {:QR flag:enabled
                         :OPCODE op-code:query
                         :AA flag:enabled
                         :TC flag:disabled
                         :RD flag:disabled
                         :RA flag:disabled
                         :RCODE r-code:no-error
                         :QDCOUNT 1
                         :ANCOUNT 2
                         :NSCOUNT 0
                         :ARCOUNT 3}
          encoded (apply encode-header (mapcat identity header-params))
          ;; decode-header is private, but I can use decode-message to get it
          ;; or I can just test decode-message which uses it.
          ;; Actually, let's just use decode-message with a mock body.
          dummy-body (byte-array 0)
          full-message (byte-array-concat encoded dummy-body)
          decoded-message (decode-message full-message)
          decoded-header (first decoded-message)]
      
      (is (= (:QR header-params) (:QR decoded-header)))
      (is (= (:AA header-params) (:AA decoded-header)))
      (is (= (:TC header-params) (:TC decoded-header)))
      (is (= (:RD header-params) (:RD decoded-header)))
      (is (= (:RA header-params) (:RA decoded-header)))
      (is (= (:QDCOUNT header-params) (:QDCOUNT decoded-header)))
      (is (= (:ANCOUNT header-params) (:ANCOUNT decoded-header)))
      (is (= (:NSCOUNT header-params) (:NSCOUNT decoded-header)))
      (is (= (:ARCOUNT header-params) (:ARCOUNT decoded-header)))
      ;; OPCODE and RCODE are returned as numbers by bits->byte in decode-header
      ;; but were passed as boolean-arrays in encode-header.
      ;; op-code:query is (byte->4-bits 0) -> [false false false false]
      (is (= 0 (:OPCODE decoded-header)))
      (is (= 0 (:RCODE decoded-header))))

    (doseq [id [0 0x1234]]
      (let [encoded (encode-header :ID id :QR flag:enabled :OPCODE op-code:query
                                   :AA flag:enabled :TC flag:disabled :RD flag:disabled
                                   :RA flag:disabled :RCODE r-code:no-error
                                   :QDCOUNT 0 :ANCOUNT 0 :NSCOUNT 0 :ARCOUNT 0)
            decoded-header (first (decode-message encoded))]
        (is (= id (:ID decoded-header)))))))

(deftest message-round-trip-test
  (testing "Full message round-trip"
    (let [header (encode-header :QR flag:enabled
                                :OPCODE op-code:query
                                :AA flag:enabled
                                :TC flag:disabled
                                :RD flag:disabled
                                :RA flag:disabled
                                :RCODE r-code:no-error
                                :QDCOUNT 1
                                :ANCOUNT 1
                                :NSCOUNT 0
                                :ARCOUNT 0)
          question (encode-question ["test" "local"] type:A class:IN)
          answer (encode-answer ["test" "local"] type:A class:IN 3600 (encode-address-data ["127" "0" "0" "1"]))
          message (byte-array-concat header question answer)
          decoded (decode-message message)
          decoded-header (first decoded)
          decoded-question (second decoded)
          decoded-answer (nth decoded 2)]
      
      (is (= 1 (:QDCOUNT decoded-header)))
      (is (= 1 (:ANCOUNT decoded-header)))
      
      (is (= "test.local" (:QNAME decoded-question)))
      (is (= type:A (:QTYPE decoded-question)))
      
      (is (= "test.local" (:NAME decoded-answer)))
      (is (= type:A (:TYPE decoded-answer)))
      (is (= "127.0.0.1" (:RDATA decoded-answer))))))

(deftest compression-test
  (testing "Name compression"
    (let [header (encode-header :QDCOUNT 2 :ANCOUNT 0 :NSCOUNT 0 :ARCOUNT 0)
          ;; First name: test.local
          name1 ["test" "local"]
          q1 (encode-question name1 type:A class:IN)
          
          ;; Header (12) + q1 (11 bytes for name + 5 bytes for 0, type, class) = 28 bytes.
          ;; q1 name starts at 12.
          ;; q1 ends at 28.
          
          compressed-name (byte-array [(long->byte 0xc0) (long->byte 0x0c)])
          ;; q2-tail for question should be [0 type 0 class] - wait, NO.
          ;; If decode-sections handles question:
          ;; 267:        (let [question {:QNAME name :QTYPE type :QCCLASS class}
          ;; 268:              next-position (+ name-end 2 2)
          ;; It expects 2 bytes for type and 2 bytes for class.
          q2-tail (byte-array [0 type:A 0 class:IN])
          q2 (byte-array-concat compressed-name q2-tail)
          message (byte-array-concat header q1 q2)
          decoded (decode-message message)
          decoded-q1 (second decoded)
          decoded-q2 (nth decoded 2)]
      
      (is (= "test.local" (:QNAME decoded-q1)))
      (is (= "test.local" (:QNAME decoded-q2))))))

(deftest large-long-conversion-test
  (testing "large long conversion"
    (let [val 0x1234567890ABCDEF
          bytes (long->byte-array val)
          expected [0x12 0x34 0x56 0x78 0x90 0xAB 0xCD 0xEF]]
      (is (= expected (map #(bit-and 0xff %) bytes))))))

(deftest name-storage-size-test
  (testing "skips label-length bytes per label even when UTF-8 bytes >= 0xC0"
    (let [q1 (encode-question ["Café" "_http" "_tcp" "local"] type:SRV class:IN)
          ;; "Café" is 5 UTF-8 bytes (0xC3 0xA9 for é), "_http" 5, "_tcp" 4, "local" 5
          ;; Name wire size: (1+5) + (1+5) + (1+4) + (1+5) + 1 = 24 bytes
          name-size (#'dns.encoding/name-storage-size 0 q1)]
      (is (= 24 name-size))))

  (testing "handles compression pointer after a UTF-8 label"
    (let [utf8-prefix (byte-array [5 67 97 102 (long->byte 0xc3) (long->byte 0xa9)
                                   (long->byte 0xc0) (long->byte 0x0c)])]
      (is (= 8 (#'dns.encoding/name-storage-size 0 utf8-prefix)))))

  (testing "decodes multiple questions when first contains UTF-8 bytes >= 0xC0"
    (let [header (encode-header :ID 0 :QR flag:disabled :OPCODE op-code:query
                                :AA flag:disabled :TC flag:disabled :RD flag:disabled
                                :RA flag:disabled :RCODE r-code:no-error
                                :QDCOUNT 2 :ANCOUNT 0 :NSCOUNT 0 :ARCOUNT 0)
          q1 (encode-question ["Café" "_http" "_tcp" "local"] type:SRV class:IN)
          q2 (encode-question ["test" "local"] type:A class:IN)
          decoded (decode-message (byte-array-concat header q1 q2))
          decoded-q1 (second decoded)
          decoded-q2 (nth decoded 2)]
      (is (= "Café._http._tcp.local" (:QNAME decoded-q1)))
      (is (= type:SRV (:QTYPE decoded-q1)))
      (is (= "test.local" (:QNAME decoded-q2)))
      (is (= type:A (:QTYPE decoded-q2))))))
