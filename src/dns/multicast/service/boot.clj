(ns dns.multicast.service.boot
  (:require
    [dns.multicast.service.config-server :as config-server]
    [dns.multicast.service.registry :refer [create-ip-registry create-service-registry]]
    [dns.multicast.service.server :as server]
    [org.httpkit.server :as http]
    [taoensso.timbre :refer [debug info warn]])
  (:gen-class))

(def http-port 3000)

(defn- on-term-signal [handler]
  (.addShutdownHook
    (Runtime/getRuntime)
    (Thread.
      (fn []
        (debug "sigterm captured")
        (try
          (handler)
          (catch Exception e
            (warn "failed to stop server cleanly," (.getMessage e)))
          (finally
            (shutdown-agents)))))))

(defn -main [& args]
  (let [name "test"
        host "test.local"
        ip-registry (create-ip-registry)
        service-registry (create-service-registry)

        [service-lookup service-advertise]
        (service-registry
          (fn [lookup advertise]
            (advertise "_ssh._tcp.local" name {:host host :port 22})
            (advertise "_airplay._tcp.local" name {:host host :port 5000})
            (advertise "_spotify-connect._tcp.local" name {:host host :port 4070 :txt {:CPath "/zc" :VERSION "1.0" :Stack "SP"}})
            (advertise "_googlecast._tcp.local" name {:host host :port 8009})
            (advertise "_octoprint._tcp.local" name {:host host :port 80 :txt {:bla 100}})
            [lookup advertise]))

        [ip-lookup ip-advertise]
        (ip-registry
          (fn [lookup advertise]
            (advertise host ["192" "168" "0" "244"])
            [lookup advertise]))

        stop-config-server
        (http/run-server
          (config-server/app service-advertise ip-advertise)
          {:port http-port})

        stop-mdns-server
        (server/listen service-lookup ip-lookup "0.0.0.0")]

    (on-term-signal
      (fn []
        (info "shutting down...")
        (stop-mdns-server)
        (stop-config-server :timeout 100)))))