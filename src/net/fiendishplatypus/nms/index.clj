(ns net.fiendishplatypus.nms.index
  (:require [clojure.java.io]
            [clojure.set]
            [clojure.string]
            [clojure.data.xml]
            [clojure.zip]
            [net.fiendishplatypus.nms.language :as language]
            [net.fiendishplatypus.nms.substance :as substance]
            [net.fiendishplatypus.nms.recipe :as recipe]
            [net.fiendishplatypus.nms.product :as product]
            [net.fiendishplatypus.nms.setup :as setup]
            [taoensso.timbre :as timbre
             :refer [info]]
            [taoensso.tufte :as tufte :refer (defnp p profile)])

  (:import [java.io RandomAccessFile]
           [java.nio ByteBuffer]))


(defn matcher
  [re ^String s]
  (let [match (re-find re s)]
    (if (coll? match) (second match) nil)))

(defn reducer
  [id? acc b]
  (p :index-reducer
     (let [a (nth acc 0)
           x (cond
          ;; found start marker. need to merge id and setup start
               (:start? b)
               {:start  (:size a)
                :size   (+ (:size a)
                           (:size b))
                :start? true
                :end?   false
                :id     (:id b)}

          ;; found recipe id after to update
               (and (not (nil? (:id b))) (id? (:id a)))
               (assoc a :id (:id b) :size (+ (:size a) (:size b)))

          ;; found end marker, need to generate additional record for next entry
               (:end? b)
               (let [m (assoc a :end? true)
                     m (update m :size + (:size b))
                     m (assoc m :end (+ (:size a) (:size b)))]
                 [{:size (:size m)} m])

               :else
               (update a :size + (:size b)))]
       (if (map? x)
         (cons x (rest acc))
         (concat x (rest acc))))))

(def id-matcher
  "Regex to match xml Id or ID attribute values"
  (re-pattern #"name=\"(?:ID|Id)\" value=\"(.+)\""))

(defn index
  "Create index for file `f` based on records. Expect files encoded in UTF-8"
  [start-mark end? f]
  (with-open [rdr (clojure.java.io/reader f)]
    (let [start? (fn [s] (.contains s start-mark))
          id (fn [s] (or (if (start? s) start-mark nil) (matcher id-matcher s)))
          xf (map (fn [s]
                    {:size   (+ 2 (count (.getBytes s "UTF-8")))
                     :start? (start? s)
                     :end?   (end? s)
                     :id     (id s)}))
          reducer (completing (partial reducer start?))
          records (sort-by (fn [a] (:start a)) <
                           (filter #(not (nil? (:id %)))
                                   (transduce xf reducer [{:size 0}] (line-seq rdr))))]
      records)))


(defn language-indexer
  [file]
  ((partial index
            "TkLocalisationEntry.xml"
            (fn [s] (= s "    </Property>"))) file))


(defn load-record
  "Given file `f` and index `record` returns full entry"
  [^String f record]
  (let [start-pos (:start record)
        buffer (ByteBuffer/allocate (- (:end record) start-pos))]
    (with-open [file (RandomAccessFile. f "r")]
      (.seek file start-pos)
      (.read (.getChannel file) buffer)
      (.flip buffer)
      (String. (.array buffer)
               (java.nio.charset.Charset/forName "UTF-8")))))


(defn translate
  "Accepts 3 parameter:
     - `dict` -- dictionary map for translation key id -> value translation
     - `input` -- data map for translation input keys are entity ids values are entity map
                  in which we need to translate some keys
     - `keys-to-translate` -- keys in input value that require translation
   
   Return `input` data map with translated keys"
  [dict input keys-to-tranlate]
  (let [lookup-key (fn [m k] {k (get dict (get m k))})]
    (into {}
          (map (fn [e]
                 {(first e)
                  (apply merge
                         (second e)
                         (map (partial lookup-key (second e))
                              keys-to-tranlate))})
               input))))


(def substance-input-example
  {"WAR_STANDING_UP" {:name      "UI_STANDING_WAR"
                      :id        "WAR_STANDING_UP"
                      :namelower "UI_STANDING_WAR"}
   "ASTEROID3"       {:name      "UI_ASTEROID3_NAME"
                      :id        "ASTEROID3"
                      :namelower "UI_ASTEROID3_NAME_L"}})

(def lang-files
  (map #(.getPath %) (setup/language-files!)))


(def dictionary-cache
  (atom {}))

(comment
  (reset! dictionary-cache {}))


(defn search-for
  "Collect strings for enrichment"
  [entity-map keys-to-collect]
  (let [to-enrich entity-map
        f (fn [entry] (-> entry
                          (second)
                          (select-keys keys-to-collect)
                          (vals)))
        xf (mapcat f)]
    (into #{} xf to-enrich)))

(defn id
  [^String s]
  (if
   (or (clojure.string/includes? s "\"Id\"") (clojure.string/includes? s "\"ID\""))
   (.substring s (+ 7 (clojure.string/index-of s "value=\""))
               (clojure.string/index-of s "\"" (+ 7 (clojure.string/index-of s "value=\""))))
   nil))

(comment 
  ;; => "SCAN_BROKEN"
  (id "      <Property name=\"Id\" value=\"SCAN_BROKEN\" />"))

(defn file->records
  [file search-for]
  (let [start-mark "TkLocalisationEntry.xml"
        start?     (fn [s] (and (not (nil? s))
                                (clojure.string/includes? s start-mark)))
        end?       (fn [s] (= s "    </Property>"))

        id         (fn [start? ^String s] (or (if start? start-mark nil)
                                              (id s)))
        xf         (map (fn [^String s]
                          (let [start? (start? s)]
                            {:size (+ 2 (alength (.getBytes s "UTF-8")))
                             :start? start?
                             :end?   (if start? false (end? s))
                             :id     (id start? s)})))
        red        (completing (partial reducer start?))]
    (with-open [rdr (clojure.java.io/reader file)]
      (sort-by (fn [a] (:start a)) <
               (filter (fn [x]
                         (and (not (nil? (:id x)))
                              (search-for (:id x))))
                       (transduce xf red '({:size 0})
                                  (line-seq rdr)))))))


(defn load-lang-record
  [file record]
  (try
    (language/language
     (load-record file record)
     (:id record))
    (catch Exception _ (println "Error on loading file: " file
                                ". Record: " record
                                ". Xml: " (load-record file record)))))

;; dictionary prototype
(defn make-dictionary
  "Prepare dictionary for translation `input` words.
   `input` should be set of keys we are looking translation for.
   Words lookup done in language `files`. "
  [input files]
  (let [search-for (apply disj input (keys @dictionary-cache))]
    (info "looking up for" (count search-for) "keys to translate")
    (loop [acc @dictionary-cache
           search-for search-for
           files files]
      (if (or (nil? (first files)) (empty? search-for))
        ;; we either found all keys or we exhausted all files to search
        ;; so we update dictionary cache with found lines and return
        (swap! dictionary-cache merge acc)
        (let [dict              (let [file (first files)]
                                  (info "looking up keys in file" file)
                                  (let [records (file->records file search-for)
                                        _ (info "loaded" (count records) "records")
                                        lang (into {}
                                                   (map (partial load-lang-record file))
                                                   records)
                                        _ (info "loaded" (count lang) "language maps")]
                                    lang))
              search-for (apply disj search-for (keys dict))
              files (rest files)]
          (recur (merge acc dict) search-for files))))))

(comment
  (time
   (make-dictionary substance-input-example
                    ["NMS_LOC4_ENGLISH.EXML" "NMS_LOC5_ENGLISH.EXML" "NMS_LOC6_ENGLISH.EXML"])))
;; => {"UI_ASTEROID3_NAME" "PLATINUM", "UI_ASTEROID3_NAME_L" "Platinum", "UI_STANDING_WAR" "VY'KEEN"}

(comment (substance/from-file index load-record))


(defn search-for-all-substances
  "Helper to include all substances to the dictionary search"
  []
  (search-for (substance/from-file index load-record) [:name :namelower]))

(defn substance-dictionary
  "Helper for full substance dictionary"
  []
  (make-dictionary (search-for-all-substances) lang-files))

(comment
  (time (substance-dictionary)))
  ;; "Elapsed time: 75559.7906 msecs" first run
  ;; "Elapsed time: 150.034 msecs" secondary run

(comment
  (time
   (translate (substance-dictionary) (substance/from-file index load-record) [:name :namelower])))
   ;; => {"WAR_STANDING_UP" {:name "VY'KEEN", :id "WAR_STANDING_UP", :namelower "VY'KEEN"},

(comment
  (recipe/from-file index load-record))
;; => {"RECIPE_1"
;;     {:id "RECIPE_1",
;;      :name "UI_YEAST_PROCESS",
;;      :time-to-make "5",
;;      :cooking true,
;;      :result {:id "FOOD_P_POOP", :type "Product", :amount "1"},
;;      :ingredients ({:id "FOOD_P_POOP", :type "Product", :amount "1"})},
;;     "RECIPE_2"
;;     {:id "RECIPE_2",
;;      :name "UI_EGG_PROCESS",
;;      :time-to-make "5",
;;      :cooking true,
;;      :result {:id "FOOD_P_STELLAR", :type "Product", :amount "1"},
;;      :ingredients ({:id "STELLAR2", :type "Substance", :amount "1"})}}

(comment
  (time
   (make-dictionary
    (search-for (recipe/from-file index load-record) [:name])
    lang-files)))

;; linking substance with recipe
(comment
  (time
   (get (translate @dictionary-cache (recipe/from-file index load-record) [:name])
        "RECIPE_2")))
  ;; => {:id "RECIPE_2",
  ;;     :name "Processor Setting: Chromatic Yolk Formation",
  ;;     :time-to-make "5",
  ;;     :cooking true,
  ;;     :result {:id "FOOD_P_STELLAR", :type "Product", :amount "1"},
  ;;     :ingredients ({:id "STELLAR2", :type "Substance", :amount "1"})}

(defnp recipe->substance-link
  [recipe]
  (let [{:keys [id result ingredients]} recipe]
    (into {}
          [{(:id result) {:as-result [id]}}
           (into {} (map (fn [m] {(:id m) {:as-ingredient [id]}}) ingredients))])))

(comment
  (time
   (recipe->substance-link
    (get (translate @dictionary-cache (recipe/from-file index load-record) [:name])
         "RECIPE_2")))
  ;; => {"FOOD_P_STELLAR" {:as-result ["RECIPE_2"]}, "STELLAR2" {:as-ingredient ["RECIPE_2"]}}
  ((second (recipe/from-file index load-record))))

(defnp substance-recipes-table
  [recipes]
  (apply merge-with (fn [a b]
                      {:as-result (concat (:as-result a) (:as-result b))
                       :as-ingredient (concat (:as-ingredient a) (:as-ingredient b))})
         (map recipe->substance-link recipes)))

(comment
  (substance-recipes-table
   [{:id "RECIPE_2"
     :result {:id "FOOD_P_STELLAR", :type "Product", :amount "1"}
     :ingredients '({:id "STELLAR2", :type "Substance", :amount "1"})}
    {:id "RECIPE_3"
     :result {:id "FOOD_P_STELLAR", :type "Product", :amount "1"}
     :ingredients '({:id "STELLAR2", :type "Substance", :amount "1"})}]))
;; => {"FOOD_P_STELLAR" {:as-result ("RECIPE_2" "RECIPE_3"), :as-ingredient ()},
;;     "STELLAR2" {:as-result (), :as-ingredient ("RECIPE_2" "RECIPE_3")}}


(defnp add-recipes-to-substance
  [substance lookup-table]
  (merge substance (get lookup-table (:id substance))))

(comment
  (add-recipes-to-substance
   {:name "CHROMATIC METAL", :id "STELLAR2", :namelower "Chromatic Metal"}
   {"FOOD_P_STELLAR" {:as-result '("RECIPE_2" "RECIPE_3"), :as-ingredient ()}
    "STELLAR2" {:as-result (), :as-ingredient '("RECIPE_2" "RECIPE_3")}}))
  ;; => {:name "CHROMATIC METAL",
  ;;     :id "STELLAR2",
  ;;     :namelower "Chromatic Metal",
  ;;     :as-result (),
  ;;     :as-ingredient ("RECIPE_2" "RECIPE_3")}


(comment
  (get (translate @dictionary-cache (substance/from-file index load-record) [:name :namelower])
       "STELLAR2"))
;; => {:name "CHROMATIC METAL", :id "STELLAR2", :namelower "Chromatic Metal"}


(defn recipe-dictionary
  []
  (make-dictionary (search-for (recipe/from-file index load-record) [:name]) lang-files))


(defn product-dictionary
  []
  (make-dictionary (search-for (product/from-file index load-record) [:name :namelower]) lang-files))


(defn- preload-dictionary
  []
  (make-dictionary
   (clojure.set/union (search-for (recipe/from-file index load-record) [:name])
                      (search-for (substance/from-file index load-record) [:name :namelower])
                      (search-for (product/from-file index load-record) [:name :namelower]))
   lang-files)
  nil)

(defn- not-found
  []
  (apply disj (clojure.set/union (search-for (recipe/from-file index load-record) [:name])
                                 (search-for (substance/from-file index load-record) [:name :namelower])
                                 (search-for (product/from-file index load-record) [:name :namelower]))
         (keys @dictionary-cache)))


(comment
  (recipe-dictionary)
  (first (product-dictionary))
  (reset! dictionary-cache {})
  (tufte/add-basic-println-handler! {})
  (profile {}
           (dotimes [_ 1]
             (preload-dictionary)))
  (profile {}
           (make-dictionary (search-for-all-substances) [(first lang-files)]))
  (not-found))


(defonce substance-cache (atom {}))


(defnp substances
  []
  (if (empty? @substance-cache)
    (reset! substance-cache
            (translate @dictionary-cache (substance/from-file index load-record) [:name :namelower]))
    @substance-cache))

(defonce recipes-cache (atom {}))

(defnp recipes
  []
  (if (empty? @recipes-cache)
    (reset! recipes-cache
            (translate @dictionary-cache (recipe/from-file index load-record) [:name]))
    @recipes-cache))

(defonce products-cache (atom {}))

(defnp products
  []
  (if (empty? @products-cache)
    (reset! products-cache
            (translate @dictionary-cache (product/from-file index load-record) [:name :namelower]))
    @products-cache))

(defn reset-caches
  []
  (reset! recipes-cache {})
  (reset! substance-cache {})
  (reset! products-cache {}))

(comment (reset-caches))


(defnp get-substance
  [substance]
  (let [substances                        (substances)
        recipes                           (recipes)
        products                          (products)

        substance-recipes-table           (substance-recipes-table (vals recipes))
        substance-with-recipe-ids         (add-recipes-to-substance substance substance-recipes-table)

        {:keys [as-ingredient as-result]} substance-with-recipe-ids
        find-name                         (fn [{:keys [type id]}]
                                            (:namelower
                                             (case type
                                               "Substance" (get substances id)
                                               "Product" (get products id))))


        recipe-with-substance-name        (fn [recipe]
                                            (let [{:keys [result ingredients]} recipe
                                                  recipe-name                  (find-name result)
                                                  result-with-name             (assoc result :name recipe-name)
                                                  ingredient-transform         (fn [ingredient]
                                                                                 (assoc ingredient :name (find-name ingredient)))
                                                  ingredients-with-name        (map ingredient-transform ingredients)]

                                              (assoc recipe :result result-with-name
                                                     :ingredients ingredients-with-name)))

        lookup-recipes                    (fn [recipe-list] (map recipe-with-substance-name
                                                                 (vals (select-keys recipes recipe-list))))]

    (assoc substance-with-recipe-ids
           :as-ingredient (lookup-recipes as-ingredient)
           :as-result (lookup-recipes as-result))))


(comment
  (time
   (take 2
         (filter (fn [x] (not (and (empty? (:as-ingredient x))
                                   (empty? (:as-result x)))))
                 (map get-substance
                      (vals (substances))))))
  
  (get-substance
       (nth (vals (substances)) 2))
  
  (get-substance (first (substances))))


(defn by-key
  [key]
  (fn [x] (key x)))

(defn substances->ui
  []
  (sort-by (by-key :namelower) compare
           (filter (fn [x] (not (and (empty? (:as-ingredient x))
                                     (empty? (:as-result x)))))
                   (map get-substance
                        (vals (merge (substances) (products)))))))

(comment
  (reset! dictionary-cache {})
  (preload-dictionary)
  (tufte/add-basic-println-handler! {})
  (profile {}
           (dotimes [_ 5]
             (substances->ui))))


(defn list-for-ui
  [k]
  (let [filter-key k
        input (vals (merge (substances) (products)))
        xf (comp
            (map get-substance)
            (filter (fn [x] (seq (get x filter-key))))
            (map #(select-keys % [:id :namelower])))]
    (sort-by (by-key :namelower)
             compare
             (into [] xf input))))

(defn usable-substances []
  (list-for-ui :as-ingredient))


;;(time (usable-substances))
;;"Elapsed time: 5677.3168 msecs"

;;(time (usable-substances2))
;;"Elapsed time: 5396.4526 msecs"

;;TODO: rename to the ingredients
;;(spit "resources/public/substances.edn" (pr-str (usable-substances)))
;;

(defn usable-products []
  (list-for-ui :as-result))

;;(spit "resources/public/products.edn" (pr-str (usable-products)))

(defn name->id
  []
  (into {} (map (fn [m] {(:namelower m) (:id m)})
                (concat
                 (list-for-ui :as-result)
                 (list-for-ui :as-ingredient)))))
(comment
  (get substance-db (get (name->id) "Salty Juice"))
  (spit "resources/public/nameToIdLookup.edn" (pr-str (name->id))))

(defn substance-db
  []
  (into {} (mapcat (fn [x] {(:id x) x}) (substances->ui))))

;;(spit "resources/public/substance-db.edn" (pr-str (substance-db)))

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


(defn with-ingredints-by-id-first
  [id m]
  (let [by-id (fn [a] (:id a))
        id-first (fn [x y] (if (= x id)
                             -1
                             (compare x y)))
        sorted-ingredients (sort-by by-id id-first (:ingredients m))]
    (assoc m :ingredients sorted-ingredients)))


(defn substance-by-number-of-ingredients
  [substance]
  (let [group-rule (fn [m] (count (:ingredients m)))
        recipes (:as-ingredient (get-substance substance))
        id (:id substance)
        recipes (map (partial with-ingredints-by-id-first id)
                     recipes)]
    (group-by group-rule recipes)))


(defn substance->ingredient-recipes
  [substance n]
  (into []
        (cons :div.grid
              (concat [[:span>strong "Recipe Name"]
                       [:span>strong "Ingredient"]
                       [:span>strong "#"]
                       [:span>strong "Result"]
                       [:span>strong "#"]]

                      (mapcat recipe->row
                              (get
                               (substance-by-number-of-ingredients substance)
                               n))))))

(comment
  (get
   (substance-by-number-of-ingredients {:id "FUEL1"})
   2))

(comment
  (reset! dictionary-cache {})
  (preload-dictionary)
  (substances->ui)
  (get (substance-db) "FOOD_J_SALT")
  (get (substance-db) (get (name->id) "Gold"))
  (:as-ingredient (get (substance-db) (get (name->id) "Platinum")))
  ;; Paraffinium -> Nitrogen -> Star Bulb
  ;; Paraffinium -> Oxygen -> Star Bulb
  ;; Oxygen -> Paraffinium -> Star Bulb
  ;; Nitrogen -> Paraffinium -> Star Bulb
  
  (get (substance-dictionary) "FOOD_ICE_NAME_L")
  (search-for {"PLANT_CAVE" {:name "UI_PLANTSUB_CAVE_NAME", :id "PLANT_CAVE", :namelower "UI_PLANTSUB_CAVE_NAME_L"}}
              [:name :namelower]))
