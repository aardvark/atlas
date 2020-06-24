(ns net.fiendishplatypus.nms.recipe
  (:require [clojure.data.xml :as xml]
            [clojure.java.io]
            [net.fiendishplatypus.file.index :as index]))

(def ingredient-example
  "     <Property value=\"GcRefinerRecipeElement.xml\">
          <Property name=\"Id\" value=\"LAUNCHSUB\" />
          <Property name=\"Type\" value=\"GcInventoryType.xml\">
            <Property name=\"InventoryType\" value=\"Substance\" />
          </Property>
          <Property name=\"Amount\" value=\"30\" />
        </Property>")

(defn parse-ingredient [m]
  (let [name  (get-in m [:attrs :name])
        value (get-in m [:attrs :value])]
    (case name
      "Table"      {:table (map parse-ingredient (:content m))}
      nil          (case value
                     "GcRefinerRecipeElement.xml" (into {} (map parse-ingredient (:content m)))
                     "GcRefinerRecipe.xml" {(:value (:attrs (first (:content m)))) (into {} (map parse-ingredient (:content m)))})
      "Id"         {:id value}
      "Amount"     {:amount value}
      "Type"       {:type (:value (:attrs (first (:content m))))}
      "Name"       {:name value}
      "TimeToMake" {:time-to-make value}
      "Cooking"    {:cooking (Boolean/parseBoolean value)}
      "Result"     {:result (into {} (map parse-ingredient (:content m)))}
      "Ingredients" {:ingredients (map parse-ingredient (:content m))})))


(comment (into {} (map parse-ingredient (:content (xml/parse-str ingredient-example)))))
         ;; => {:id "LAUNCHSUB", :type "Substance", :amount "30"}


(def ingredients-example
  "<Property name=\"Ingredients\">
        <Property value=\"GcRefinerRecipeElement.xml\">
          <Property name=\"Id\" value=\"LAUNCHSUB\" />
          <Property name=\"Type\" value=\"GcInventoryType.xml\">
            <Property name=\"InventoryType\" value=\"Substance\" />
          </Property>
          <Property name=\"Amount\" value=\"30\" />
        </Property>
      </Property>")

(defn to-ingredients
  [xml]
  (let [all (:content xml)]
    {:ingredients
     (into []
           (map #(into {} (map
                           parse-ingredient
                           (:content %)))
                all))}))

(comment (to-ingredients (xml/parse-str ingredients-example)))
;; => {:ingredients [{:id "LAUNCHSUB", :type "Substance", :amount "30"}]}


(def result-example
  "<Property name=\"Result\" value=\"GcRefinerRecipeElement.xml\">
        <Property name=\"Id\" value=\"FUEL1\" />
        <Property name=\"Type\" value=\"GcInventoryType.xml\">
          <Property name=\"InventoryType\" value=\"Substance\" />
        </Property>
        <Property name=\"Amount\" value=\"1\" />
      </Property>")


(defn to-result 
  [xml]
  {:result (into {} (map parse-ingredient (:content xml)))})


(def recipe-example
   "<Property value=\"GcRefinerRecipe.xml\">
      <Property name=\"Id\" value=\"REFINERECIPE_14\" />
      <Property name=\"Name\" value=\"RECIPE_OXYGEN\" />
      <Property name=\"TimeToMake\" value=\"20\" />
      <Property name=\"Cooking\" value=\"False\" />
      <Property name=\"Result\" value=\"GcRefinerRecipeElement.xml\">
        <Property name=\"Id\" value=\"FUEL1\" />
        <Property name=\"Type\" value=\"GcInventoryType.xml\">
          <Property name=\"InventoryType\" value=\"Substance\" />
        </Property>
        <Property name=\"Amount\" value=\"1\" />
      </Property>
      <Property name=\"Ingredients\">
        <Property value=\"GcRefinerRecipeElement.xml\">
          <Property name=\"Id\" value=\"OXYGEN\" />
          <Property name=\"Type\" value=\"GcInventoryType.xml\">
            <Property name=\"InventoryType\" value=\"Substance\" />
          </Property>
          <Property name=\"Amount\" value=\"1\" />
        </Property>
      </Property>
    </Property>")


(comment (into {} (map parse-ingredient (:content (xml/parse-str recipe-example)))))
;; => {:id "REFINERECIPE_14",
;;     :name "RECIPE_OXYGEN",
;;     :time-to-make "20",
;;     :cooking false,
;;     :result {:id "FUEL1", :type "Substance", :amount "1"},
;;     :ingredients ({:id "OXYGEN", :type "Substance", :amount "1"})}

(defn recipe-table 
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    {:table "GcRecipeTable" 
     :recipes (into {} (map parse-ingredient (:content (first (:content (xml/parse rdr))))))}))

(comment 
  (recipe-table "resources/nms/recipes-example.xml"))
  ;; => {:table "GcRecipeTable",
  ;;     :recipes
  ;;     {"REFINERECIPE_14"
  ;;      {:id "REFINERECIPE_14",
  ;;       :name "RECIPE_OXYGEN",
  ;;       :time-to-make "20",
  ;;       :cooking false,
  ;;       :result {:id "FUEL1", :type "Substance", :amount "1"},
  ;;       :ingredients ({:id "OXYGEN", :type "Substance", :amount "1"})},
  ;;      "REFINERECIPE_15"
  ;;      {:id "REFINERECIPE_15",
  ;;       :name "RECIPE_LAUNCHFUEL",
  ;;       :time-to-make "120",
  ;;       :cooking false,
  ;;       :result {:id "JELLY", :type "Product", :amount "1"},
  ;;       :ingredients ({:id "LAUNCHSUB", :type "Substance", :amount "30"})}}}

(defn recipe 
  [xml]
  (into {} 
        (map parse-ingredient 
             (:content (xml/parse-str xml)))))
;; => #'net.fiendishplatypus.nms.recipe.parser/recipe


(comment 
  (recipe
   (index/load-record "resources/nms/recipes-example.xml" 
                      (first
                       (index/index (fn [s] (.contains s "GcRefinerRecipe.xml"))
                                    (fn [s] (= s "    </Property>"))
                                    "resources/nms/recipes-example.xml")))))
;; => {:id "REFINERECIPE_14",
;;     :name "RECIPE_OXYGEN",
;;     :time-to-make "20",
;;     :cooking false,
;;     :result {:id "FUEL1", :type "Substance", :amount "1"},
;;     :ingredients ({:id "OXYGEN", :type "Substance", :amount "1"})}


