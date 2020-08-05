(ns reagent-shadow-sandbox.handler
  (:require
   [clojure.java.io]
   [clojure.edn]
   [reitit.ring :as reitit-ring]
   [reagent-shadow-sandbox.middleware :refer [middleware]]
   [hiccup.page :refer [include-js include-css html5]]
   [config.core :refer [env]]
   [net.fiendishplatypus.file.index :as index]))

(def mount-target
  [:div#app
   [:h2 "Welcome to reagent-shadow-sandbox"]
   [:p "please wait while Figwheel/shadow-cljs is waking up ..."]
   [:p "(Check the js console for hints if nothing exciting happens.)"]])

(defn head []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   (include-css (if (env :dev) "/css/site.css" "/css/site.min.css"))])

(defn loading-page []
  (html5
   (head)
   [:body {:class "body-container"}
    mount-target
    (include-js "/js/app.js")
    [:script "reagent_shadow_sandbox.core.init_BANG_()"]]))


(defn index-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (loading-page)})


(defn load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (clojure.java.io/reader source)]
      (clojure.edn/read (java.io.PushbackReader. r)))

    (catch java.io.IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))

(def state
  (load-edn "resources/public/data.edn"))

(defn state-storage
  []
  state)

(defn substance-storage
  []
  (load-edn "resources/public/substances.edn"))

(defn substance-db-storage
  []
  (load-edn "resources/public/substance-db.edn"))

(defn apps-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body (str (state-storage))})

(defn substances-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body (str (substance-storage))})

(defn substance-db-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body (str (substance-db-storage))})

(def app
  (reitit-ring/ring-handler
   (reitit-ring/router
    [["/" {:get {:handler index-handler}}]
     ["/atlas/apps" {:get {:handler apps-handler}}]
     ["/atlas/substances" {:get {:handler substances-handler}}]
     ["/atlas/substance-db" {:get {:handler substance-db-handler}}]
     ["/sandbox/atlas/page/mock" {:get {:handler index-handler}}]
     ["/items"
      ["" {:get {:handler index-handler}}]
      ["/:item-id" {:get {:handler index-handler
                          :parameters {:path {:item-id int?}}}}]]
     ["/about" {:get {:handler index-handler}}]])
   (reitit-ring/routes
    (reitit-ring/create-resource-handler {:path "/" :root "/public"})
    (reitit-ring/create-default-handler))
   {:middleware middleware}))
