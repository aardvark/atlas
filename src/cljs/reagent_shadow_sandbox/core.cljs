(ns reagent-shadow-sandbox.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]
   [ajax.core :as ajax]
   [cljs.reader]))

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :atlas]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components

(def selected-app (atom ""))

(defn app-info-component []
   [:div @selected-app])


(defn item-component [app]
  [:div.app 
   {:on-click #(reset! selected-app (:info app))}
   [:span.appname (:name app)]])
     

(def apps (atom nil))

(defn items-component []
  (let [query (fn [] 
                (ajax/GET "/atlas/apps"
                  :handler (fn [response]
                             (reset! apps (cljs.reader/read-string response)))))]
    (query)
    (fn []
      (into [:div.applist]
        (for [app @apps]
          ^{:key (:name app)} (item-component app))))))
   

(defn atlas-page []
  (fn []
    [:h1 "Atlas"]
    [:div.main 
      [items-component]
     [:div.appinfo-cnt
       [:div "App info:"]
       [app-info-component "Not selected"]]]))


;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :atlas #'atlas-page))


;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header
        [:p [:a {:href (path-for :index)} "Home"] " | "
         [:a {:href (path-for :about)} "About reagent-shadow-sandbox"]]]
       [page]
       [:footer
        [:p "Add footer"]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (rdom/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)))
        
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))
