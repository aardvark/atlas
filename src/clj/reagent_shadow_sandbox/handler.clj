(ns reagent-shadow-sandbox.handler
  (:require
   [reitit.ring :as reitit-ring]
   [reagent-shadow-sandbox.middleware :refer [middleware]]
   [hiccup.page :refer [include-js include-css html5]]
   [config.core :refer [env]]))

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

(defn state-storage
  []
  [{:name "Ferrite Dust" 
    :info "Metalic Mineral Extract" 
    :stack 250
    :recipes [{:name "Pure Ferite" :ratio "1:1" :time-per-stack "1m 15s"}
              {:name "Metal Plating" :ration "50:1" :time-per-stack "Instant"}]}
                                                                   
   {:name "Metal Plating" :info "Crafted Technology Component" :stack 10}
   {:name "Sodium"
    :info "TODO"
    :stack 250
    :recipes [{:name "Sodium Nitrate" :ratio "2:1" :time-per-stack "TODO"}]}
   {:name "item4" :info nil}])
  

(defn apps-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body (str (state-storage))})

(def app
  (reitit-ring/ring-handler
   (reitit-ring/router
    [["/" {:get {:handler index-handler}}]
     ["/atlas/apps" {:get {:handler apps-handler}}]
     ["/items"
      ["" {:get {:handler index-handler}}]
      ["/:item-id" {:get {:handler index-handler
                          :parameters {:path {:item-id int?}}}}]]
     ["/about" {:get {:handler index-handler}}]])
   (reitit-ring/routes
    (reitit-ring/create-resource-handler {:path "/" :root "/public"})
    (reitit-ring/create-default-handler))
   {:middleware middleware}))
