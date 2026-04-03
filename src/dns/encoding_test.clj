(ns dns.encoding-test
  (:require [clojure.test :refer :all]
            [dns.encoding :as encoding]))

(deftest byte-array-concat-test
  (let [ba1 (byte-array [1 2])
        ba2 (byte-array [3 4])
        result (encoding/byte-array-concat ba1 ba2)]
    (is (java.util.Arrays/equals ^bytes (byte-array [1 2 3 4]) ^bytes result))))

(deftest byte->bits-test
  (is (= [false false false false false false false true] (seq (#'encoding/byte->bits 1))))
  (is (= [true false false false false false false false] (seq (#'encoding/byte->bits 128))))
  (is (= [true true true true true true true true] (seq (#'encoding/byte->bits 255)))))

(deftest byte->4-bits-test
  (is (= [false false false false] (seq (#'encoding/byte->4-bits 0))))
  (is (= [true false false false] (seq (#'encoding/byte->4-bits 128)))))

(deftest byte->long-test
  (is (= 0 (#'encoding/byte->long (byte 0))))
  (is (= 127 (#'encoding/byte->long (byte 127))))
  (is (= 128 (#'encoding/byte->long (encoding/long->byte 128))))
  (is (= 255 (#'encoding/byte->long (encoding/long->byte 255)))))

(deftest byte-array->long-test
  (is (= 0 (#'encoding/byte-array->long (byte-array [0 0]))))
  (is (= 1 (#'encoding/byte-array->long (byte-array [0 1]))))
  (is (= 256 (#'encoding/byte-array->long (byte-array [1 0]))))
  (is (= 65535 (#'encoding/byte-array->long (byte-array [-1 -1])))))

(deftest long->byte-test
  (is (= (byte 0) (encoding/long->byte 0)))
  (is (= (byte 127) (encoding/long->byte 127)))
  (is (= (byte -128) (encoding/long->byte 128)))
  (is (= (byte -1) (encoding/long->byte 255))))

(deftest bits->byte-test
  (is (= (byte 0) (#'encoding/bits->byte [false false false false false false false false])))
  (is (= (byte 1) (#'encoding/bits->byte [false false false false false false false true])))
  (is (= (byte -128) (#'encoding/bits->byte [true false false false false false false false])))
  (is (= (byte -1) (#'encoding/bits->byte [true true true true true true true true]))))

(deftest long->byte-array-test
  (is (= [0 0 0 0 0 0 0 0] (map #(if (< % 0) (+ % 256) %) (encoding/long->byte-array 0))))
  (is (= [0 0 0 0 0 0 0 1] (map #(if (< % 0) (+ % 256) %) (encoding/long->byte-array 1))))
  (is (= [0 0 0 0 0 0 1 0] (map #(if (< % 0) (+ % 256) %) (encoding/long->byte-array 256))))
  (is (= [255 255 255 255 255 255 255 255] (map #(if (< % 0) (+ % 256) %) (encoding/long->byte-array -1)))))

(deftest encode-header-test
  (let [header (encoding/encode-header
                :QR encoding/flag:enabled
                :OPCODE encoding/op-code:query
                :AA encoding/flag:disabled
                :TC encoding/flag:disabled
                :RD encoding/flag:disabled
                :RA encoding/flag:disabled
                :RCODE encoding/r-code:no-error
                :QDCOUNT 1
                :ANCOUNT 0
                :NSCOUNT 0
                :ARCOUNT 0)]
    (is (= 12 (alength header)))
    (is (= (byte -128) (nth header 2))) ;; QR=1, others 0
    (is (= (byte 0) (nth header 3)))
    (is (= (byte 0) (nth header 4)))
    (is (= (byte 1) (nth header 5)))))

(deftest encode-question-test
  (let [result (encoding/encode-question ["host" "local"] 1 1)
        expected (byte-array [4 (byte \h) (byte \o) (byte \s) (byte \t)
                              5 (byte \l) (byte \o) (byte \c) (byte \a) (byte \l)
                              0 0 1 0 1])]
    (is (java.util.Arrays/equals ^bytes expected ^bytes result))))

(deftest encode-name-test
  (let [result (encoding/encode-name ["host" "local"])
        expected (byte-array [4 (byte \h) (byte \o) (byte \s) (byte \t)
                              5 (byte \l) (byte \o) (byte \c) (byte \a) (byte \l)
                              0])]
    (is (java.util.Arrays/equals ^bytes expected ^bytes result))))

(deftest encode-address-data-test
  (let [result (encoding/encode-address-data ["192" "168" "1" "1"])
        expected (byte-array [(encoding/long->byte 192) (encoding/long->byte 168) (byte 1) (byte 1)])]
    (is (java.util.Arrays/equals ^bytes expected ^bytes result))))

(deftest encode-txt-data-test
  (let [result (encoding/encode-txt-data {:key "val"})
        expected (byte-array [7 (byte \k) (byte \e) (byte \y) (byte \=) (byte \v) (byte \a) (byte \l)])]
    (is (java.util.Arrays/equals ^bytes expected ^bytes result))))

(deftest encode-answer-test
  (let [data (byte-array [1 2 3 4])
        result (encoding/encode-answer ["host"] 1 1 3600 data)
        ;; name: 4 host 0 (6 bytes)
        ;; type: 0 1 (2 bytes)
        ;; class: 0 1 (2 bytes)
        ;; ttl: 0 0 14 16 (4 bytes) (3600 = 0x0E10)
        ;; rdlength: 0 4 (2 bytes)
        ;; data: 1 2 3 4 (4 bytes)
        expected (byte-array [4 (byte \h) (byte \o) (byte \s) (byte \t) 0
                              0 1 0 1
                              0 0 14 16
                              0 4
                              1 2 3 4])]
    (is (java.util.Arrays/equals ^bytes expected ^bytes result))))

(deftest decode-header-test
  (let [header-bytes (byte-array [0 1 128 0 0 1 0 0 0 0 0 0])
        result (#'encoding/decode-header header-bytes)]
    (is (= 1 (:ID result)))
    (is (= true (:QR result)))
    (is (= 0 (:OPCODE result)))
    (is (= 1 (:QDCOUNT result)))))

(deftest decode-name-test
  (let [message (byte-array [4 (byte \h) (byte \o) (byte \s) (byte \t) 0])]
    (is (= "host" (#'encoding/decode-name 0 message))))
  (let [message (byte-array [4 (byte \h) (byte \o) (byte \s) (byte \t)
                              5 (byte \l) (byte \o) (byte \c) (byte \a) (byte \l) 0])]
    (is (= "host.local" (#'encoding/decode-name 0 message)))))

(deftest decode-address-data-test
  (let [message (byte-array [(encoding/long->byte 192) (encoding/long->byte 168) (byte 1) (byte 1)])]
    (is (= "192.168.1.1" (#'encoding/decode-address-data 0 message)))))

(deftest decode-txt-data-test
  (let [message (byte-array [7 (byte \k) (byte \e) (byte \y) (byte \=) (byte \v) (byte \a) (byte \l)])]
    (is (= {"key" "val"} (#'encoding/decode-txt-data 0 8 message)))))

(deftest name-storage-size-test
  (let [message (byte-array [4 (byte \h) (byte \o) (byte \s) (byte \t) 0])]
    (is (= 6 (#'encoding/name-storage-size 0 message)))))

(deftest decode-data-test
  (let [message (byte-array [(encoding/long->byte 192) (encoding/long->byte 168) (byte 1) (byte 1)])]
    (is (= "192.168.1.1" (#'encoding/decode-data encoding/type:A 0 4 message)))))

(deftest decode-message-test
  (let [header (encoding/encode-header
                :QR encoding/flag:enabled
                :OPCODE encoding/op-code:query
                :AA encoding/flag:disabled
                :TC encoding/flag:disabled
                :RD encoding/flag:disabled
                :RA encoding/flag:disabled
                :RCODE encoding/r-code:no-error
                :QDCOUNT 1
                :ANCOUNT 1
                :NSCOUNT 0
                :ARCOUNT 0)
        question (encoding/encode-question ["host"] 1 1)
        answer (encoding/encode-answer ["host"] 1 1 3600 (byte-array [(encoding/long->byte 192) (encoding/long->byte 168) 1 1]))
        message (encoding/byte-array-concat header question answer)
        result (encoding/decode-message message)
        decoded-header (first result)
        decoded-sections (rest result)]
    (is (= true (:QR decoded-header)))
    (is (= 1 (:QDCOUNT decoded-header)))
    (is (= 1 (:ANCOUNT decoded-header)))
    (is (= 2 (count decoded-sections)))
    (let [q (first decoded-sections)
          a (second decoded-sections)]
      (is (= "host" (:QNAME q)))
      (is (= "host" (:NAME a)))
      (is (= "192.168.1.1" (:RDATA a))))))

(deftest decode-srv-data-test
  (let [message (byte-array [0 0 0 0 0 0 0 0 0 0 0 0 ;; Header
                             0 10 ;; Priority
                             0 20 ;; Weight
                             0 30 ;; Port
                             4 (byte \h) (byte \o) (byte \s) (byte \t) 0]) ;; host. (encoded name)
        start 12
        result (#'encoding/decode-srv-data start message)]
    (is (= 10 (:priority result)))
    (is (= 20 (:weight result)))
    (is (= 30 (:port result)))
    (is (= "host" (:host result)))))

(deftest encode-srv-data-test
  (let [host ["host"]
        port 30
        priority 10
        weight 20
        result (encoding/encode-srv-data host port priority weight)
        expected (byte-array [0 10 0 20 0 30 4 (byte \h) (byte \o) (byte \s) (byte \t) 0])]
    (is (java.util.Arrays/equals ^bytes expected ^bytes result))))

(deftest kv->tuple-test
  (is (= ["key" "value"] (#'encoding/kv->tuple "key=value")))
  (is (= ["key" ""] (#'encoding/kv->tuple "key=")))
  (is (= ["key" ""] (#'encoding/kv->tuple "key"))))
