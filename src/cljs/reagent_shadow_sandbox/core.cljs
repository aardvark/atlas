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

(defn get-value
  [elId]
  (aget (.getElementById js/document elId) "value"))

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
  (for [{id :id namelower :namelower} @substances]
    ^{:key id} [:option {:id id :value namelower} namelower]))




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



;; data db
(def recipe-db
  (atom {:susbtances   {}
         :substance-db {}
         :products     {}
         :lookup       {}}))

(defn substance-by-number-of-ingredients
  [query]
  (let [id (:ingredient @query)
        substance-db (get @recipe-db :substance-db)
        substance (get substance-db id)
        recipes (:as-ingredient substance)
        cooking? (:cooking @query)
        product (:product @query)
        xf (comp
            (filter #(if cooking? true (= (:cooking %) false)))
            (filter #(if (nil? product) true 
                         (= product (get-in % [:result :id]))))
            (map (partial with-ingredients-by-id-first id)))
        
        recipes (into [] xf recipes)
        
        sorted-recipes (sort-by (fn [x] (:amount (first (:ingredients x))))
                                compare
                                recipes)
        number-of-ingredients (fn [m] (count (:ingredients m)))]
    (group-by number-of-ingredients sorted-recipes)))


(defn recipe->row2
  "Take a recipe map and produce a 'hiccup' row for drawing on ui"
  [{name :name result :result ingredients :ingredients}]
  (let [start [[:span name]]
        n (count ingredients)
        empty-ingredients (repeat (* (- 3 n) 2) [:span])]

     (concat start
              (mapcat ingredient->row ingredients)
              empty-ingredients
              (ingredient->row result))))

(defn substance->ingredient-recipes
  [search-for]
  (let [recipes (substance-by-number-of-ingredients search-for)]
    (into []
          (cons (keyword (str "div.grid-3"))
                (concat
                   [[:span>strong "Recipe Name"]
                    [:span>strong "Ingredient A"]
                    [:span>strong "#"]
                    [:span>strong "Ingredient B"]
                    [:span>strong "#"]
                    [:span>strong "Ingredient C"]
                    [:span>strong "#"]
                    [:span>strong "Result"]
                    [:span>strong "#"]]
                 (mapcat recipe->row2
                         (concat (get recipes 1)
                                 (get recipes 2)
                                 (get recipes 3))))))))

;; products query 
;; 
(def products (atom []))

(defn query-products
  []
  (ajax/GET "/atlas/products"
    :handler (fn [response]
               (reset! products (cljs.reader/read-string response)))))

(def lookup (atom {}))

(defn query-lookup
  []
  (ajax/GET "/atlas/lookup"
    :handler (fn [response]
               (reset! lookup (cljs.reader/read-string response)))))

(defn make-products-options
  []
  (for [{id :id namelower :namelower} @products]
    ^{:key id} [:option {:id id} namelower]))


(defn query-recipe-db
  []
  (ajax/GET "/atlas/recipe-db"
    :handler (fn [response]
               (let [edn-payload (cljs.reader/read-string response)]
                 (reset! recipe-db edn-payload)))))

(defn make-options
  [xs]
  (for [{id :id namelower :namelower} xs]
    ^{:key id} [:option {:id id :value namelower} namelower]))

;; new mock page
;; 
;; 
(defn atlas-mock-page []
  (let [search-for (atom {:ingredient "Nothing" :product "Nothing" :cooking false})
        query query-recipe-db]
    (query)
    (fn []
      [:div
       [:h3 "Find recipes"]
       [:div
        [:label {:for "ingredients-inp"} "By ingredient:"]
        [:input#ingredients-inp {:type "text" :list "ingredients-list" :name "ingredients"}]
        [:datalist#ingredients-list
         (make-options (:substances @recipe-db))]

        [:label {:for "products-inp"} "By result:"]
        [:input#products-inp {:type "text" :list "products-list" :name "products"}]
        [:datalist#products-list
         (make-options (:products @recipe-db))]

        "Include cooking: "
        [:input {:type "checkbox" :id "includeCooking"}]]

       [:button
        {:on-click (fn [_]
                     (reset! search-for {:ingredient (get (:lookup @recipe-db) (get-value "ingredients-inp"))
                                         :product (get (:lookup @recipe-db) (get-value "products-inp"))
                                         :cooking (checkbox-value "includeCooking")}))}
        "Search"]
       [:div "Found:" @search-for]
       (substance->ingredient-recipes search-for)])))

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
