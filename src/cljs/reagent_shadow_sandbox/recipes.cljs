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
  (println xs)
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
                     (reset! search-for {:ingredient (get (:lookup @recipe-db) (element/value "ingredients-inp"))
                                         :product (get (:lookup @recipe-db) (element/value "products-inp"))
                                         :cooking (checkbox/value "includeCooking")}))}
        "Search"]
       [:div "Found:" @search-for]
       [substance->ingredient-recipes search-for]])))
       
