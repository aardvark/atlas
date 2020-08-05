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
   [["/" :atlas]
    ["/sandbox/atlas/page/mock" :atlas-mock]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components

(def selected-app (atom ""))

(defn unwrap-ingredients
  [ingredients]
  (str (reduce
        (fn [a b]
          (let [amount (:amount b)
                amount (if (> amount 1) (str amount " of ") "")]
            (str a (if (= a "") "" " + ") amount (:name b))))
        ""
        ingredients)))

(defn unwrap-result
  [result]
  (str (:name result) "(" (:amount result) ")"))

(defn recipe-component [recipes section-name]
  (into [:div.recipes section-name]
        (for [recipe recipes]
          [:div.recipe (unwrap-ingredients (:ingredients recipe))
           " = "
           (unwrap-result (:result recipe))
           " | " (:name recipe)
           [:br] "DEBUG:" [:br] recipe])))

(defn app-info-component []
  [:<>
   [:div (:name @selected-app)]
   [recipe-component (:as-result @selected-app) "Created by: "]
   [recipe-component (:as-ingredient @selected-app) "Used in: "]])


(defn item-component [app]
  [:div.app
   {:on-click #(reset! selected-app app)}
   [:span.appname (:namelower app)]])


(def apps (atom nil))

(defn items-component []
  (let [query (fn []
                (ajax/GET "/atlas/apps"
                  :handler (fn [response]
                             (println response)
                             (reset! apps (cljs.reader/read-string response))
                             (reset! selected-app (first @apps)))))]
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
      [app-info-component "Not selected"]]]))


(defn selected-option
  [elementId]
  (let [el (.getElementById js/document elementId)
        el-idx (aget el "selectedIndex")
        selected (aget (aget el "options") el-idx)
        text (aget selected "text")
        id (aget selected "id")]
    {:id id :text text}))

(defn checkbox-value
  [elementId]
  (let [el (.getElementById js/document elementId)
        v (aget el "checked")]
    v))

(def substances (atom []))

(defn query-substances
  []
  (ajax/GET "/atlas/substances"
    :handler (fn [response]
               (reset! substances (cljs.reader/read-string response)))))


(def substance-db (atom {}))

(defn query-substance-db
  []
  (ajax/GET "/atlas/substance-db"
    :handler (fn [response]
               (reset! substance-db (cljs.reader/read-string response)))))


(defn make-substance-options
  []
  (for [s @substances]
    ^{:key (:id s)} [:option {:id (:id s)} (:namelower s)]))

;; Substance db query
;; 
(defn ingredient->row
  [{name :name amount :amount}]
  [[:span name] [:span amount]])

(defn recipe->row
  "Take a recipe map and produce a 'hiccup' row for drawing on ui"
  [{name :name result :result ingredients :ingredients}]
  (let [start [[:span name]]]
    (concat start
            (mapcat ingredient->row ingredients)
            (ingredient->row result))))


(defn with-ingredients-by-id-first
  [id m]
  (let [key (fn [a] [(:id a) (:amount a)])
        id-first (fn [[x xx] [y yy]]
                   (cond
                     (= x y id) (compare xx yy)
                     (= x id) -1
                     (= y id) 1
                     :else (compare x y)))
        sorted-ingredients (sort-by key id-first (:ingredients m))]
    (assoc m :ingredients sorted-ingredients)))

(defn substance-by-number-of-ingredients
  [substance]
  (let [id (:id substance)
        cooking? (:cooking substance)
        group-rule (fn [m] (count (:ingredients m)))
        recipes (:as-ingredient (get @substance-db id))
        recipes (filter #(if cooking? true (= (:cooking %) false)) recipes)
        sorted-recipes (map (partial with-ingredients-by-id-first id)
                            recipes)
        sorted-recipes (sort-by (fn [x] (:amount (first (:ingredients x))))
                                compare
                                sorted-recipes)]
    (group-by group-rule sorted-recipes)))


(defn substance->ingredient-recipes
  [substance n]
  (into []
        (cons (keyword (str "div.grid-" n))
              (concat
               (case n
                 1
                 [[:span>strong "Recipe Name"]
                  [:span>strong "Ingredient"]
                  [:span>strong "#"]
                  [:span>strong "Result"]
                  [:span>strong "#"]]
                 2
                 [[:span>strong "Recipe Name"]
                  [:span>strong "Ingredient A"]
                  [:span>strong "#"]
                  [:span>strong "Ingredient B"]
                  [:span>strong "#"]
                  [:span>strong "Result"]
                  [:span>strong "#"]]
                 3
                 [[:span>strong "Recipe Name"]
                  [:span>strong "Ingredient A"]
                  [:span>strong "#"]
                  [:span>strong "Ingredient B"]
                  [:span>strong "#"]
                  [:span>strong "Ingredient C"]
                  [:span>strong "#"]
                  [:span>strong "Result"]
                  [:span>strong "#"]])

               (mapcat recipe->row
                       (get
                        (substance-by-number-of-ingredients substance)
                        n))))))

;; new mock page

(defn atlas-mock-page []
  (let [search-for (atom {:ingredient "Nothing" :product "Nothing"})
        query query-substances
        query2 query-substance-db]
    (query)
    (query2)
    (fn []
      [:div
       [:h3 "Find recipes"]
       [:div
        "By ingredient: "
        [:select#ingredients-opt {:name "ingredients"}
         [:option ""]
         (make-substance-options)]


        "By product: "
        [:select#products-opt {:name "products"}
         [:option ""]
         [:option "Ion Battery"]]
        "Include cooking: "
        [:input {:type "checkbox" :id "includeCooking"}]]
       [:button
        {:on-click (fn [e]
                     (reset! search-for {:ingredient (selected-option "ingredients-opt")
                                         :product (selected-option "products-opt")
                                         :cooking (checkbox-value "includeCooking")}))}
        "Search"]
       [:div "Found:" @search-for]
       (substance->ingredient-recipes
        {:id (:id (:ingredient @search-for)) :cooking (:cooking @search-for)}
        1)
       (substance->ingredient-recipes
        {:id (:id (:ingredient @search-for)) :cooking (:cooking @search-for)}
        2)
       (substance->ingredient-recipes
        {:id (:id (:ingredient @search-for)) :cooking (:cooking @search-for)}
        3)])))

;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :atlas #'atlas-page
    :atlas-mock #'atlas-mock-page))


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
