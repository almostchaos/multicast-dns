(ns dns.multicast.service.config-server
  (:require
    [clojure.data.json :as json]
    [clojure.java.io :as io]
    [compojure.core :refer [POST routes]]
    [compojure.route :as route]))

(defn app [service-advertise ip-advertise]
  (routes
    (POST "/service" {:keys [body]}
      (if-not body
        {:status  400
         :headers {"Content-Type" "application/json"}
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
              (do
                (apply service-advertise (cond-> [service-type service-instance host (long port)]
                                   txt (conj :txt txt)
                                   ttl (conj :ttl (long ttl))))
                {:status  200
                 :headers {"Content-Type" "application/json"}
                 :body    (json/write-str {:status  "advertised"
                                           :service (str service-instance "." service-type)
                                           :host    host
                                           :port    port})})
              {:status  400
               :headers {"Content-Type" "application/json"}
               :body    (json/write-str {:error "Missing required fields: type, name, host, port"})}))
          (catch Exception e
            {:status  400
             :headers {"Content-Type" "application/json"}
             :body    (json/write-str {:error (.getMessage e)})}))))

    (POST "/ip" {:keys [body]}
      (if-not body
        {:status  400
         :headers {"Content-Type" "application/json"}
         :body    (json/write-str {:error "Missing request body"})}
        (try
          (let [payload (json/read (io/reader body) :key-fn keyword)
                hostname (or (:host payload) (:hostname payload))
                ip (:ip payload)]
            (if (and hostname ip)
              (do
                (ip-advertise hostname ip)
                {:status  200
                 :headers {"Content-Type" "application/json"}
                 :body    (json/write-str {:status  "advertised"
                                           :host    hostname
                                           :ip    ip})})
              {:status  400
               :headers {"Content-Type" "application/json"}
               :body    (json/write-str {:error "Missing required fields: hostname, ip"})}))
          (catch Exception e
            {:status  400
             :headers {"Content-Type" "application/json"}
             :body    (json/write-str {:error (.getMessage e)})}))))

    (route/not-found "Not found")))
