(ns dns.multicast.service.config-server
  (:require
    [clojure.data.json :as json]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [compojure.core :refer [POST routes]]
    [compojure.route :as route]))

(def application-json {"Content-Type" "application/json"})

(defn- normalize-ip [ip]
  (let [parts (cond
                (string? ip) (string/split ip #"\." -1)
                (sequential? ip) (mapv str ip)
                :else nil)]
    (when (= 4 (count parts))
      (let [octets (mapv parse-long parts)]
        (when (every? #(and (some? %) (<= 0 % 255)) octets)
          (mapv str octets))))))

(defn app [service-advertise ip-advertise]
  (routes
    (POST "/service" {:keys [body]}
      (if-not body
        {:status  400
         :headers application-json
         :body    (json/write-str {:error "Missing request body"})}
        (try
          (let [payload (json/read (io/reader body) :key-fn keyword)
                service-type (or (:type payload) (:service-type payload))
                service-instance (or (:name payload) (:service-instance payload))
                host (:host payload)
                port (:port payload)
                txt (:txt payload)
                ttl (:ttl payload)]
            (if (and service-type service-instance host port)
              (let [info (cond-> {:host host
                                  :port (long port)}
                                 txt (assoc :txt txt)
                                 ttl (assoc :ttl (long ttl)))]
                (service-advertise service-type service-instance info)
                {:status  200
                 :headers application-json
                 :body    (json/write-str {:status  "advertised"
                                           :service (str service-instance "." service-type)
                                           :host    host
                                           :port    port})})
              {:status  400
               :headers application-json
               :body    (json/write-str {:error "Missing required fields: type, name, host, port"})}))
          (catch Exception e
            {:status  400
             :headers application-json
             :body    (json/write-str {:error (.getMessage e)})}))))

    (POST "/ip" {:keys [body]}
      (if-not body
        {:status  400
         :headers application-json
         :body    (json/write-str {:error "Missing request body"})}
        (try
          (let [payload (json/read (io/reader body) :key-fn keyword)
                hostname (or (:host payload) (:hostname payload))
                ip (:ip payload)]
            (if (and hostname ip)
              (if-let [normalized-ip (normalize-ip ip)]
                (do
                  (ip-advertise hostname normalized-ip)
                  {:status  200
                   :headers application-json
                   :body    (json/write-str {:status "advertised"
                                             :host   hostname
                                             :ip     normalized-ip})})
                {:status  400
                 :headers application-json
                 :body    (json/write-str {:error "Invalid IPv4 address"})})
              {:status  400
               :headers application-json
               :body    (json/write-str {:error "Missing required fields: hostname, ip"})}))
          (catch Exception e
            {:status  400
             :headers application-json
             :body    (json/write-str {:error (.getMessage e)})}))))

    (route/not-found "Not found")))
