(ns dns.multicast.service.config-server-test
  (:require
    [clojure.data.json :as json]
    [clojure.test :refer :all]
    [dns.encoding :as encoding]
    [dns.multicast.service.config-server :refer [app]]
    [dns.multicast.service.registry :refer [create-ip-registry create-service-registry]])
  (:import
    (java.io ByteArrayInputStream)))

(defn- json-request [uri payload]
  {:request-method :post
   :uri            uri
   :body           (ByteArrayInputStream. (.getBytes (json/write-str payload) "UTF-8"))})

(defn- parse-body [response]
  (json/read-str (:body response) :key-fn keyword))

(deftest post-service-test
  (testing "advertises service with info map into service registry"
    (let [service-registry (create-service-registry)
          ip-registry (create-ip-registry)]
      (service-registry
        (fn [lookup-service advertise-service]
          (ip-registry
            (fn [_ advertise-ip]
              (let [handler (app advertise-service advertise-ip)
                    resp (handler (json-request "/service"
                                                {:type "_http._tcp.local"
                                                 :name "web"
                                                 :host "web.local"
                                                 :port 8080}))]
                (is (= 200 (:status resp)))
                (is (= {:status  "advertised"
                        :service "web._http._tcp.local"
                        :host    "web.local"
                        :port    8080}
                       (parse-body resp)))
                (is (= {"web._http._tcp.local"
                        ["_http._tcp.local" "web" {:host "web.local" :port 8080}]}
                       (into {} (lookup-service "web._http._tcp.local")))))))))))

  (testing "advertises service with txt, ttl, and alternate keys"
    (let [service-registry (create-service-registry)
          ip-registry (create-ip-registry)]
      (service-registry
        (fn [lookup-service advertise-service]
          (ip-registry
            (fn [_ advertise-ip]
              (let [handler (app advertise-service advertise-ip)
                    resp (handler (json-request "/service"
                                                {:service-type     "_http._tcp.local"
                                                 :service-instance "api"
                                                 :host             "api.local"
                                                 :port             9000
                                                 :txt              {:version "1.0"}
                                                 :ttl              300}))]
                (is (= 200 (:status resp)))
                (is (= {"api._http._tcp.local"
                        ["_http._tcp.local" "api" {:host "api.local"
                                                   :port 9000
                                                   :txt  {:version "1.0"}
                                                   :ttl  300}]}
                       (into {} (lookup-service "api._http._tcp.local")))))))))))

  (testing "returns 400 when body or required fields are missing"
    (let [handler (app (fn [_ _ _]) (fn [_ _]))]
      (is (= 400 (:status (handler {:request-method :post :uri "/service"}))))
      (is (= 400 (:status (handler (json-request "/service" {:type "_http._tcp.local"}))))))))

(deftest post-ip-test
  (testing "normalizes dotted IPv4 string for ip-registry and encode-address-data"
    (let [ip-registry (create-ip-registry)]
      (ip-registry
        (fn [lookup-ip advertise-ip]
          (let [handler (app (fn [_ _ _]) advertise-ip)
                resp (handler (json-request "/ip" {:hostname "test.local"
                                                   :ip       "192.168.0.244"}))]
            (is (= 200 (:status resp)))
            (is (= {:status "advertised"
                    :host   "test.local"
                    :ip     ["192" "168" "0" "244"]}
                   (parse-body resp)))
            (is (= ["192" "168" "0" "244"] (lookup-ip "test.local")))
            (is (= [192 168 0 244]
                   (map #(bit-and 0xff %) (encoding/encode-address-data (lookup-ip "test.local"))))))))))

  (testing "accepts octet arrays of strings or numbers and :host alias"
    (let [ip-registry (create-ip-registry)]
      (ip-registry
        (fn [lookup-ip advertise-ip]
          (let [handler (app (fn [_ _ _]) advertise-ip)]
            (is (= 200 (:status (handler (json-request "/ip" {:host "a.local" :ip ["10" "0" "0" "1"]})))))
            (is (= ["10" "0" "0" "1"] (lookup-ip "a.local")))
            (is (= 200 (:status (handler (json-request "/ip" {:hostname "b.local" :ip [10 0 0 2]})))))
            (is (= ["10" "0" "0" "2"] (lookup-ip "b.local"))))))))

  (testing "rejects invalid IPv4 addresses and missing fields with 400"
    (let [handler (app (fn [_ _ _]) (fn [_ _]))]
      (is (= 400 (:status (handler {:request-method :post :uri "/ip"}))))
      (is (= 400 (:status (handler (json-request "/ip" {:hostname "test.local"})))))
      (doseq [bad-ip ["not-an-ip" "192.168.0" "192.168.0.256" "192.168.0.1." "-1.0.0.1" [192 168 1]]]
        (is (= 400 (:status (handler (json-request "/ip" {:hostname "test.local" :ip bad-ip})))))))))
