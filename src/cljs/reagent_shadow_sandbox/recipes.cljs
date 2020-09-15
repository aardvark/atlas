(ns reagent-shadow-sandbox.recipes
  (:require
   [reagent-shadow-sandbox.checkbox :as checkbox]
   [reagent-shadow-sandbox.element :as element]

   [ajax.core :as ajax]
   [cljs.reader]
   
   [reagent.core :as reagent :refer [atom]]))

;; Substance db query
;; 
(defn ingredient->row
  [{name :name amount :amount}]
  [[:span name] [:span amount]])

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
  (let [selected-ingredient (:ingredient @query)
        substance-db (get @recipe-db :substance-db)
        substance (get substance-db selected-ingredient)
        selected-product (:product @query)
        product (get substance-db selected-product)
        recipes (concat (:as-ingredient substance)
                        (:as-result product))
        cooking? (:cooking @query)
        
        xf (comp
            (filter #(if cooking? true (= (:cooking %) false)))
            (filter #(if (nil? selected-ingredient) true
                         (contains? (set (map :id (:ingredients %))) selected-ingredient)))
            (filter #(if (nil? selected-product) true
                         (= selected-product (get-in % [:result :id]))))
            (map (partial with-ingredients-by-id-first selected-ingredient)))

        recipes (into [] xf recipes)

        sorted-recipes (sort-by (fn [x] (:amount (first (:ingredients x))))
                                compare
                                recipes)
        number-of-ingredients (fn [m] (count (:ingredients m)))]
    (group-by number-of-ingredients sorted-recipes)))


(defn recipe->row
  "Take a recipe map and produce a 'hiccup' row for drawing on ui"
  [{name :name result :result ingredients :ingredients}]
  (let [start [[:span name]]
        n (count ingredients)
        empty-ingredients (repeat (* (- 3 n) 2) [:span])]

    (concat start
            (mapcat ingredient->row ingredients)
            empty-ingredients
            (ingredient->row result))))

(defn make-options
  [xs]
  (for [{id :id namelower :namelower} xs]
    ^{:key id} [:option {:id id :value namelower} namelower]))

;;TODO make this adapt to the recipes longer than 3 ingredient
(defn substance->ingredient-recipes
  "Create table for given input output ingredients"
  [search-for]
  (let [recipes (substance-by-number-of-ingredients search-for)]
    (into []
          (cons (keyword (str "div.grid-3"))
                (concat
                 [[:span#recipe-header.t-header
                   [:strong "Recipe Name"]]
                  [:span#ingredients-header.t-header
                   [:strong "Ingredients"]]

                  [:span#result-header.t-header [:strong "Result"]]
                  [:span#result-amnts.t-header [:strong "#"]]
                  [:span.t-header [:strong "A"]]
                  [:span.t-header [:strong "#"]]
                  [:span.t-header [:strong "B"]]
                  [:span.t-header [:strong "#"]]
                  [:span.t-header [:strong "C"]]
                  [:span.t-header [:strong "#"]]]
                 (mapcat recipe->row
                         (concat (get recipes 1)
                                 (get recipes 2)
                                 (get recipes 3))))))))

;; data query
;; 

(defn query-recipe-db
  []
  (ajax/GET "/atlas/recipe-db"
    :handler (fn [response]
               (let [edn-payload (cljs.reader/read-string response)]
                 (reset! recipe-db edn-payload)))))


(defn clear-btn
  [state name]
  [:button 
    {:id (str name "-clear-btn")
     :on-click (fn [_]
                 ;; reset state
                 (swap! state (fn [current] (assoc current (keyword name) nil)))
                 ;; reset input field
                 (aset (.getElementById js/document (str name "-inp")) "value" ""))}
    "X"])
  

;; new mock page
;; 
;; 
(defn recipes-page []
  (let [search-for (atom {:ingredient "Nothing" :product "Nothing" :cooking false})
        query query-recipe-db]
    (query)
    (fn []
      [:div
       [:h3 "Find recipes"]
       [:div
        
        [:label {:for "ingredient-inp"} "By ingredient:"]
        [:input#ingredient-inp {:type "text" :list "ingredients-list" :name "ingredients"}]
        [clear-btn search-for "ingredient"]
        [:datalist#ingredients-list
         (make-options (:substances @recipe-db))]

        [:label {:for "product-inp"} "By result:"]
        [:input#product-inp {:type "text" :list "products-list" :name "products"}]
        [clear-btn search-for "product"]
        [:datalist#products-list
         (make-options (:products @recipe-db))]

        "Include cooking: "
        [:input {:type "checkbox" :id "includeCooking"}]]

       [:button
        {:on-click (fn [_]
                     (reset! search-for {:ingredient (get (:lookup @recipe-db) (element/value "ingredient-inp"))
                                         :product (get (:lookup @recipe-db) (element/value "product-inp"))
                                         :cooking (checkbox/value "includeCooking")}))}
        "Search"]
       [:div "Found:" @search-for]
       [substance->ingredient-recipes search-for]])))
       
