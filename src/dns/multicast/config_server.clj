(ns dns.multicast.config-server
  (:require
    [clojure.data.json :as json]
    [clojure.java.io :as io]
    [compojure.core :refer [POST routes]]
    [compojure.route :as route])
  (:import (java.net InetAddress)))

(defn app [advertise]
  (routes
    (POST "*" {:keys [body]}
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
                (apply advertise (cond-> [service-type service-instance host (long port)]
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
    (route/not-found "Not found")))
