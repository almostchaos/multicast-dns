(ns dns.multicast.service.boot
  (:require
    [dns.multicast.service.config-server :as config-server]
    [dns.multicast.service.registry :refer [create-service-registry]]
    [dns.multicast.service.server :as server]
    [org.httpkit.server :as http]
    [taoensso.timbre :refer [debug info]])
  (:gen-class))

(def http-port 3000)

(defn- on-term-signal [handler]
  (.addShutdownHook
    (Runtime/getRuntime)
    (Thread.
      (fn []
        (debug "sigterm captured")
        (handler)))))

(defn -main [& args]
  (let [name "test"
        host "test.local"
        ip ["192" "168" "0" "244"]
        service-registry (create-service-registry)
        ip-registry nil
        app (service-registry
              (fn [_ advertise]
                (advertise "_ssh._tcp.local" name :host host :ip ip :port 22)
                (advertise "_airplay._tcp.local" name :host host :ip ip :port 36663)
                (advertise "_spotify-connect._tcp.local" name :host host :ip ip :port 36663 :txt {:CPath "/zc" :VERSION "1.0" :Stack "SP"})
                (advertise "_googlecast._tcp.local" name :host host :ip ip :port 10000)
                (advertise "_octoprint._tcp.local" name :host host :ip ip :port 80 :txt {:bla 100})

                (config-server/app advertise)))
        stop-config-server (http/run-server app {:port http-port})
        stop-mdns-server (server/listen service-registry ip-registry "0.0.0.0")]

    (on-term-signal
      (fn []
        (info "shutting down...")
        (stop-mdns-server)
        (stop-config-server :timeout 100)
        (shutdown-agents)))))