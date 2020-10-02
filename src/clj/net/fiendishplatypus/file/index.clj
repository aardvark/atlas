(ns net.fiendishplatypus.file.index
  (:require [clojure.java.io]
            [clojure.set]
            [clojure.string]
            [net.fiendishplatypus.nms.language :as language]
            [net.fiendishplatypus.nms.substance :as substance]
            [net.fiendishplatypus.nms.recipe :as recipe]
            [net.fiendishplatypus.nms.product :as product])
  (:import [java.io RandomAccessFile]
           [java.nio ByteBuffer]))


(defn matcher
  [re s]
  (let [match (re-find re s)]
    (if (coll? match) (second match) nil)))

(defn reducer
  [id? acc b]
  (let [a (first acc)
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
      (concat x (rest acc)))))

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
  [f record]
  (let [buffer (ByteBuffer/allocate (- (:end record) (:start record)))]
    (with-open [file (RandomAccessFile. f "r")]
      (.seek file (:start record))
      (.read (.getChannel file) buffer)
      (.flip buffer)
      (String. (.array buffer)))))


(defn translate
  [dict input keys-to-tranlate]
  (let [lookup-key (fn [m k] {k (get dict (get m k))})]
    (into {}
          (map (fn [e]
                 {(first e) (apply merge
                                   (second e)
                                   (map (partial lookup-key (second e)) keys-to-tranlate))})
               input))))


(def substance-input-example
  {"WAR_STANDING_UP" {:name      "UI_STANDING_WAR"
                      :id        "WAR_STANDING_UP"
                      :namelower "UI_STANDING_WAR"}
   "ASTEROID3"       {:name      "UI_ASTEROID3_NAME"
                      :id        "ASTEROID3"
                      :namelower "UI_ASTEROID3_NAME_L"}})

(def lang-files
  (list
   "NMS_LOC6_ENGLISH.EXML"
   "NMS_LOC5_ENGLISH.EXML"
   "NMS_LOC4_ENGLISH.EXML"
   "NMS_LOC1_ENGLISH.EXML"
   "NMS_UPDATE3_ENGLISH.EXML"))


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



;; dictionary prototype
(defn make-dictionary
  "Prepare dictionary for translation `input` words.
   `input` should be set of keys we are looking translation for.
   Words lookup done in language `files`. "
  [input files]
  (let [search-for (apply disj input (keys @dictionary-cache))]
    (loop [acc @dictionary-cache
           search-for search-for
           files files]
      (if (or (nil? (first files)) (empty? search-for))
        ;; we either found all keys or we exhausted all files to search
        ;; so we update dictionary cache with found lines and return
        (swap! dictionary-cache merge acc)
        (let [dict
              (let [file       (str "D:\\NMSProjects\\Notepad\\LANGUAGE\\" (first files))
                    ;;TODO: move file location to props
                    start-mark (:start-mark language/index-meta)
                    start?     (fn [s] (clojure.string/includes? s start-mark))
                    end?       (:end? language/index-meta)
                    id         (fn [s] (or (if (start? s) start-mark nil)
                                           (matcher id-matcher s)))
                    xf         (map (fn [s]
                                      {:size   (+ 2 (count (.getBytes s "UTF-8")))
                                       :start? (start? s)
                                       :end?   (end? s)
                                       :id     (id s)}))
                    red        (completing (partial reducer start?))]

                (with-open [rdr (clojure.java.io/reader file)]
                  (let [records (sort-by (fn [a] (:start a)) <
                                         (filter #(not (nil? (:id %)))
                                                 (transduce xf red '({:size 0}) (line-seq rdr))))
                        lang    (map
                                 (fn [record]
                                   (try
                                     (language/language
                                      (load-record file record)
                                      (:id record))
                                     (catch Exception e (println "Error on loading file: " file 
                                                                 ". Record: " record 
                                                                 ". Xml: " (load-record file record)))))
                                 records)
                        ;; here we are filtering full language dictionary for only 
                        ;; keys we are searching for, ideally we may move this step 
                        ;; to the record loading step and this may shave us some time
                        dict    (into {} (filter (fn [x] (search-for (first (first  x)))))
                                      lang)]
                    dict)))
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
  ;; => {"UI_LAND3_NAME_L" "Magnetised Ferrite",
  ;;     "UI_SUNGOLD_NAME_L" "Liquid Sun",
  ;;     "UI_SAND1_NAME_L" "Silicate Powder",
  ;;     "UI_HOT1_NAME" "PHOSPHORUS",
  ;;     "UI_PLANTSUB_RADIO_NAME_L" "Gamma Root",
  ;;     "UI_AIR1_NAME_L" "Oxygen",
  ;;     "UI_STELLAR2_NAME_L" "Chromatic Metal",
  ;;     "UI_GREEN2_NAME_L" "Emeril",
  ;;     "UI_EX_GREEN_NAME_L" "Activated Emeril",
  ;;     "UI_DUSTY1_NAME" "PYRITE",
  ;;     "UI_DUSTY1_NAME_L" "Pyrite",
  ;;     "UI_PLANTSUB_DEADCREATURE_NAME" "MORDITE",
  ;;     "UI_TOXIC1_NAME_L" "Ammonia",
  ;;     "UI_PLANTSUB_LUSH_NAME_L" "Star Bulb",
  ;;     "UI_WATERPLANT_NAME_L" "Cyto-Phosphate",
  ;;     "UI_EX_YELLOW_NAME" "ACTIVATED COPPER",
  ;;     "UI_FUEL_1_NAME" "CARBON",
  ;;     "UI_EX_RED_NAME_L" "Activated Cadmium",
  ;;     "SUB_DEADDRONE_NAME_L" "Pugneum",
  ;;     "UI_MAINTAIN_SUB1_NAME" "RESIDUAL GOOP",
  ;;     "UI_NEWWATER2_NAME" "CHLORINE",
  ;;     "UI_WATER2_NAME_L" "Sodium",
  ;;     "UI_BLUE2_NAME" "INDIUM",
  ;;     "UI_STELLAR2_NAME" "CHROMATIC METAL",
  ;;     "UI_MAINTAIN_SUB1_NAME_L" "Residual Goop",
  ;;     "UI_MAINTAIN_SUB4_NAME_L" "Living Slime",
  ;;     "UI_LAUNCHSUB2_NAME_L" "Deuterium",
  ;;     "UI_FUEL_1_NAME_L" "Carbon",
  ;;     "UI_STANDING_TRA" "GEK",
  ;;     "UI_MAINTAIN_SUB4_NAME" "LIVING SLIME",
  ;;     "UI_PLANTSUB_CAVE_NAME" "MARROW BULB",
  ;;     "UI_STANDING_G_TRA" "MERCHANTS",
  ;;     "UI_BLUE2_NAME_L" "Indium",
  ;;     "UI_UNITS_REWARD_NAME" "UNITS",
  ;;     "UI_CAVE2_NAME" "IONISED COBALT",
  ;;     "UI_FUEL_2_NAME_L" "Condensed Carbon",
  ;;     "UI_LUSH1_NAME" "PARAFFINIUM",
  ;;     "UI_EX_RED_NAME" "ACTIVATED CADMIUM",
  ;;     "UI_ASTEROID2_NAME" "GOLD",
  ;;     "UI_TOXIC1_NAME" "AMMONIA",
  ;;     "UI_ROCKETFUEL_NAME" "TRITIUM",
  ;;     "UI_WATERPLANT_NAME" "CYTO-PHOSPHATE",
  ;;     "UI_MAINTAIN_SUB5_NAME_L" "Viscous Fluids",
  ;;     "UI_GAS_2_NAME_L" "Radon",
  ;;     "UI_NEWCATA2_NAME_L" "Sodium Nitrate",
  ;;     "UI_HEXITE_NAME_L" "Hexite",
  ;;     "UI_COLD1_NAME_L" "Dioxite",
  ;;     "UI_PLANTSUB_CREATUREPOOP_NAME" "FAECIUM",
  ;;     "UI_SAND1_NAME" "SILICATE POWDER",
  ;;     "UI_LAND1_NAME" "FERRITE DUST",
  ;;     "UI_WATER1_NAME_L" "Salt",
  ;;     "UI_PLANTSUB_CREATUREPOOP_NAME_L" "Faecium",
  ;;     "TECH_FRAGMENT_NAME" "NANITE CLUSTER",
  ;;     "UI_LAND2_NAME_L" "Pure Ferrite",
  ;;     "UI_RED2_NAME" "CADMIUM",
  ;;     "UI_LAND2_NAME" "PURE FERRITE",
  ;;     "UI_EX_GREEN_NAME" "ACTIVATED EMERIL",
  ;;     "UI_PLANTSUB_BARREN_NAME" "CACTUS FLESH",
  ;;     "UI_ASTEROID1_NAME_L" "Silver",
  ;;     "UI_PLANTSUB_LUSH_NAME" "STAR BULB",
  ;;     "UI_NEWWATER2_NAME_L" "Chlorine",
  ;;     "UI_QUICKSILVER_REWARD_NAME" "QUICKSILVER",
  ;;     "UI_LAND1_NAME_L" "Ferrite Dust",
  ;;     "UI_LAUNCHSUB2_NAME" "DEUTERIUM",
  ;;     "UI_YELLOW2_NAME" "COPPER",
  ;;     "UI_SUNGOLD_NAME" "LIQUID SUN",
  ;;     "UI_PLANTSUB_SCORCHED_NAME" "SOLANIUM",
  ;;     "UI_PLANTSUB_TOXIC_NAME" "FUNGAL MOULD",
  ;;     "UI_PLANTSUB_WATER_NAME" "KELP SAC",
  ;;     "UI_GAS_2_NAME" "RADON",
  ;;     "UI_MAINTAIN_SUB5_NAME" "VISCOUS FLUIDS",
  ;;     "TECH_FRAGMENT_NAME_L" "Nanite Cluster",
  ;;     "UI_PLANTSUB_SCORCHED_NAME_L" "Solanium",
  ;;     "UI_AIR1_NAME" "OXYGEN",
  ;;     "UI_MAINTAIN_SUB3_NAME_L" "Rusted Metal",
  ;;     "UI_STANDING_EXP" "KORVAX",
  ;;     "UI_EX_BLUE_NAME" "ACTIVATED INDIUM",
  ;;     "UI_NEWCATA2_NAME" "SODIUM NITRATE",
  ;;     "UI_ROCKETFUEL_NAME_L" "Tritium",
  ;;     "UI_PLANTSUB_SNOW_NAME" "FROST CRYSTAL",
  ;;     "UI_SOULFRAG_NAME_L" "Fragmented Qualia",
  ;;     "UI_STANDING_G_EXP" "EXPLORERS",
  ;;     "UI_LAUNCHSUB_NAME" "DI-HYDROGEN",
  ;;     "UI_EX_YELLOW_NAME_L" "Activated Copper",
  ;;     "UI_SOULFRAG_NAME" "FRAGMENTED QUALIA",
  ;;     "UI_GAS_3_NAME" "NITROGEN",
  ;;     "UI_RADIO1_NAME" "URANIUM",
  ;;     "SUB_DEADDRONE_NAME" "PUGNEUM",
  ;;     "UI_COLD1_NAME" "DIOXITE",
  ;;     "UI_CAVE1_NAME" "COBALT",
  ;;     "UI_ASTEROID1_NAME" "SILVER",
  ;;     "UI_LUSH1_NAME_L" "Paraffinium",
  ;;     "UI_PLANTSUB_DEADCREATURE_NAME_L" "Mordite",
  ;;     "UI_LAND3_NAME" "MAGNETISED FERRITE",
  ;;     "UI_PLANTSUB_SNOW_NAME_L" "Frost Crystal",
  ;;     "UI_PLANTSUB_BARREN_NAME_L" "Cactus Flesh",
  ;;     "UI_MAINTAIN_SUB2_NAME_L" "Runaway Mould",
  ;;     "UI_PLANTSUB_RADIO_NAME" "GAMMA ROOT",
  ;;     "UI_GAS_1_NAME_L" "Sulphurine",
  ;;     "UI_NANITES_REWARD_NAME" "NANITES",
  ;;     "UI_STANDING_G_WAR" "MERCENARIES",
  ;;     "UI_ASTEROID3_NAME" "PLATINUM",
  ;;     "UI_CAVE2_NAME_L" "Ionised Cobalt",
  ;;     "UI_HOT1_NAME_L" "Phosphorus",
  ;;     "UI_FUEL_2_NAME" "CONDENSED CARBON",
  ;;     "UI_RADIO1_NAME_L" "Uranium",
  ;;     "UI_PLANTSUB_CAVE_NAME_L" "Marrow Bulb",
  ;;     "UI_WATER2_NAME" "SODIUM",
  ;;     "UI_ASTEROID2_NAME_L" "Gold",
  ;;     "UI_GAS_3_NAME_L" "Nitrogen",
  ;;     "UI_MAINTAIN_SUB2_NAME" "RUNAWAY MOULD",
  ;;     "UI_GREEN2_NAME" "EMERIL",
  ;;     "UI_MAINTAIN_SUB3_NAME" "RUSTED METAL",
  ;;     "UI_RED2_NAME_L" "Cadmium",
  ;;     "UI_ASTEROID3_NAME_L" "Platinum",
  ;;     "UI_WATER1_NAME" "SALT",
  ;;     "UI_LAUNCHSUB_NAME_L" "Di-hydrogen",
  ;;     "UI_STANDING_WAR" "VY'KEEN",
  ;;     "UI_PLANTSUB_WATER_NAME_L" "Kelp Sac",
  ;;     "UI_GAS_1_NAME" "SULPHURINE",
  ;;     "UI_YELLOW2_NAME_L" "Copper",
  ;;     "UI_EX_BLUE_NAME_L" "Activated Indium",
  ;;     "UI_CAVE1_NAME_L" "Cobalt",
  ;;     "UI_HEXITE_NAME" "HEXITE",
  ;;     "UI_PLANTSUB_TOXIC_NAME_L" "Fungal Mould"}


(comment
  (time
   (translate (substance-dictionary) (substance/from-file index load-record) [:name :namelower])))
   ;; => {"WAR_STANDING_UP" {:name "VY'KEEN", :id "WAR_STANDING_UP", :namelower "VY'KEEN"},
   ;;     "ASTEROID3" {:name "PLATINUM", :id "ASTEROID3", :namelower "Platinum"},
   ;;     "OXYGEN" {:name "OXYGEN", :id "OXYGEN", :namelower "Oxygen"},
   ;;     "CATALYST2" {:name "SODIUM NITRATE", :id "CATALYST2", :namelower "Sodium Nitrate"},
   ;;     "PLANT_SNOW" {:name "FROST CRYSTAL", :id "PLANT_SNOW", :namelower "Frost Crystal"},
   ;;     "HOT1" {:name "PHOSPHORUS", :id "HOT1", :namelower "Phosphorus"},
   ;;     "PLANT_TOXIC" {:name "FUNGAL MOULD", :id "PLANT_TOXIC", :namelower "Fungal Mould"},
   ;;     "SPACEGUNK2" {:name "RUNAWAY MOULD", :id "SPACEGUNK2", :namelower "Runaway Mould"},
   ;;     "ROBOT1" {:name "PUGNEUM", :id "ROBOT1", :namelower "Pugneum"},
   ;;     "TGUILD_STAND_DN" {:name "MERCHANTS", :id "TGUILD_STAND_DN", :namelower "MERCHANTS"},
   ;;     "CREATURE1" {:name "MORDITE", :id "CREATURE1", :namelower "Mordite"},
   ;;     "WATER2" {:name "CHLORINE", :id "WATER2", :namelower "Chlorine"},
   ;;     "EX_YELLOW" {:name "ACTIVATED COPPER", :id "EX_YELLOW", :namelower "Activated Copper"},
   ;;     "ROCKETSUB" {:name "TRITIUM", :id "ROCKETSUB", :namelower "Tritium"},
   ;;     "TECHFRAG_R" {:name "NANITES", :id "TECHFRAG_R", :namelower "NANITES"},
   ;;     "SPACEGUNK3" {:name "RUSTED METAL", :id "SPACEGUNK3", :namelower "Rusted Metal"},
   ;;     "STELLAR2" {:name "CHROMATIC METAL", :id "STELLAR2", :namelower "Chromatic Metal"},
   ;;     "WGUILD_STAND_DN" {:name "MERCENARIES", :id "WGUILD_STAND_DN", :namelower "MERCENARIES"},
   ;;     "CAVE2" {:name "IONISED COBALT", :id "CAVE2", :namelower "Ionised Cobalt"},
   ;;     "PLANT_WATER" {:name "KELP SAC", :id "PLANT_WATER", :namelower "Kelp Sac"},
   ;;     "EX_RED" {:name "ACTIVATED CADMIUM", :id "EX_RED", :namelower "Activated Cadmium"},
   ;;     "TRA_STANDING_UP" {:name "GEK", :id "TRA_STANDING_UP", :namelower "GEK"},
   ;;     "WAR_STANDING_DN" {:name "VY'KEEN", :id "WAR_STANDING_DN", :namelower "VY'KEEN"},
   ;;     "QUICKSILVER" {:name "QUICKSILVER", :id "QUICKSILVER", :namelower "QUICKSILVER"},
   ;;     "SAND1" {:name "SILICATE POWDER", :id "SAND1", :namelower "Silicate Powder"},
   ;;     "CAVE1" {:name "COBALT", :id "CAVE1", :namelower "Cobalt"},
   ;;     "LAUNCHSUB" {:name "DI-HYDROGEN", :id "LAUNCHSUB", :namelower "Di-hydrogen"},
   ;;     "UNITS" {:name "UNITS", :id "UNITS", :namelower "UNITS"},
   ;;     "CATALYST1" {:name "SODIUM", :id "CATALYST1", :namelower "Sodium"},
   ;;     "DUSTY1" {:name "PYRITE", :id "DUSTY1", :namelower "Pyrite"},
   ;;     "TRA_STANDING_DN" {:name "GEK", :id "TRA_STANDING_DN", :namelower "GEK"},
   ;;     "LUSH1" {:name "PARAFFINIUM", :id "LUSH1", :namelower "Paraffinium"},
   ;;     "PLANT_RADIO" {:name "GAMMA ROOT", :id "PLANT_RADIO", :namelower "Gamma Root"},
   ;;     "WATER1" {:name "SALT", :id "WATER1", :namelower "Salt"},
   ;;     "GAS2" {:name "RADON", :id "GAS2", :namelower "Radon"},
   ;;     "PLANT_HOT" {:name "SOLANIUM", :id "PLANT_HOT", :namelower "Solanium"},
   ;;     "FUEL1" {:name "CARBON", :id "FUEL1", :namelower "Carbon"},
   ;;     "GAS3" {:name "NITROGEN", :id "GAS3", :namelower "Nitrogen"},
   ;;     "GREEN2" {:name "EMERIL", :id "GREEN2", :namelower "Emeril"},
   ;;     "WGUILD_STAND_UP" {:name "MERCENARIES", :id "WGUILD_STAND_UP", :namelower "MERCENARIES"},
   ;;     "TOXIC1" {:name "AMMONIA", :id "TOXIC1", :namelower "Ammonia"},
   ;;     "RADIO1" {:name "URANIUM", :id "RADIO1", :namelower "Uranium"},
   ;;     "BLUE2" {:name "INDIUM", :id "BLUE2", :namelower "Indium"},
   ;;     "EX_BLUE" {:name "ACTIVATED INDIUM", :id "EX_BLUE", :namelower "Activated Indium"},
   ;;     "EXP_STANDING_DN" {:name "KORVAX", :id "EXP_STANDING_DN", :namelower "KORVAX"},
   ;;     "EGUILD_STAND_DN" {:name "EXPLORERS", :id "EGUILD_STAND_DN", :namelower "EXPLORERS"},
   ;;     "SPECIAL_POOP" {:name "HEXITE", :id "SPECIAL_POOP", :namelower "Hexite"},
   ;;     "RED2" {:name "CADMIUM", :id "RED2", :namelower "Cadmium"},
   ;;     "SUNGOLD" {:name "LIQUID SUN", :id "SUNGOLD", :namelower "Liquid Sun"},
   ;;     "LAND3" {:name "MAGNETISED FERRITE", :id "LAND3", :namelower "Magnetised Ferrite"},
   ;;     "ASTEROID1" {:name "SILVER", :id "ASTEROID1", :namelower "Silver"},
   ;;     "FUEL2" {:name "CONDENSED CARBON", :id "FUEL2", :namelower "Condensed Carbon"},
   ;;     "TECHFRAG" {:name "NANITE CLUSTER", :id "TECHFRAG", :namelower "Nanite Cluster"},
   ;;     "SPACEGUNK4" {:name "LIVING SLIME", :id "SPACEGUNK4", :namelower "Living Slime"},
   ;;     "WATERPLANT" {:name "CYTO-PHOSPHATE", :id "WATERPLANT", :namelower "Cyto-Phosphate"},
   ;;     "PLANT_POOP" {:name "FAECIUM", :id "PLANT_POOP", :namelower "Faecium"},
   ;;     "LAND1" {:name "FERRITE DUST", :id "LAND1", :namelower "Ferrite Dust"},
   ;;     "EX_GREEN" {:name "ACTIVATED EMERIL", :id "EX_GREEN", :namelower "Activated Emeril"},
   ;;     "TGUILD_STAND_UP" {:name "MERCHANTS", :id "TGUILD_STAND_UP", :namelower "MERCHANTS"},
   ;;     "LAUNCHSUB2" {:name "DEUTERIUM", :id "LAUNCHSUB2", :namelower "Deuterium"},
   ;;     "PLANT_DUST" {:name "CACTUS FLESH", :id "PLANT_DUST", :namelower "Cactus Flesh"},
   ;;     "YELLOW2" {:name "COPPER", :id "YELLOW2", :namelower "Copper"},
   ;;     "SOULFRAG" {:name "FRAGMENTED QUALIA", :id "SOULFRAG", :namelower "Fragmented Qualia"},
   ;;     "SPACEGUNK1" {:name "RESIDUAL GOOP", :id "SPACEGUNK1", :namelower "Residual Goop"},
   ;;     "EXP_STANDING_UP" {:name "KORVAX", :id "EXP_STANDING_UP", :namelower "KORVAX"},
   ;;     "LAND2" {:name "PURE FERRITE", :id "LAND2", :namelower "Pure Ferrite"},
   ;;     "SPACEGUNK5" {:name "VISCOUS FLUIDS", :id "SPACEGUNK5", :namelower "Viscous Fluids"},
   ;;     "PLANT_CAVE" {:name "MARROW BULB", :id "PLANT_CAVE", :namelower "Marrow Bulb"},
   ;;     "ASTEROID2" {:name "GOLD", :id "ASTEROID2", :namelower "Gold"},
   ;;     "PLANT_LUSH" {:name "STAR BULB", :id "PLANT_LUSH", :namelower "Star Bulb"},
   ;;     "EGUILD_STAND_UP" {:name "EXPLORERS", :id "EGUILD_STAND_UP", :namelower "EXPLORERS"},
   ;;     "GAS1" {:name "SULPHURINE", :id "GAS1", :namelower "Sulphurine"},
   ;;     "COLD1" {:name "DIOXITE", :id "COLD1", :namelower "Dioxite"}}



(recipe/from-file index load-record)
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
  ;; => {"UI_LAND3_NAME_L" "Magnetised Ferrite",
  ;;     "UI_SUNGOLD_NAME_L" "Liquid Sun",
  ;;     "UI_SAND1_NAME_L" "Silicate Powder",
  ;;     "UI_HOT1_NAME" "PHOSPHORUS",
  ;;     "UI_PLANTSUB_RADIO_NAME_L" "Gamma Root",
  ;;     "UI_AIR1_NAME_L" "Oxygen",
  ;;     "UI_STELLAR2_NAME_L" "Chromatic Metal",
  ;;     "UI_GREEN2_NAME_L" "Emeril",
  ;;     "UI_EX_GREEN_NAME_L" "Activated Emeril",
  ;;     "UI_DUSTY1_NAME" "PYRITE",
  ;;     "UI_DUSTY1_NAME_L" "Pyrite",
  ;;     "UI_PLANTSUB_DEADCREATURE_NAME" "MORDITE",
  ;;     "UI_TOXIC1_NAME_L" "Ammonia",
  ;;     "UI_PLANTSUB_LUSH_NAME_L" "Star Bulb",
  ;;     "UI_WATERPLANT_NAME_L" "Cyto-Phosphate",
  ;;     "UI_EX_YELLOW_NAME" "ACTIVATED COPPER",
  ;;     "UI_FUEL_1_NAME" "CARBON",
  ;;     "UI_EX_RED_NAME_L" "Activated Cadmium",
  ;;     "SUB_DEADDRONE_NAME_L" "Pugneum",
  ;;     "UI_MAINTAIN_SUB1_NAME" "RESIDUAL GOOP",
  ;;     "UI_NEWWATER2_NAME" "CHLORINE",
  ;;     "UI_WATER2_NAME_L" "Sodium",
  ;;     "UI_BLUE2_NAME" "INDIUM",
  ;;     "UI_STELLAR2_NAME" "CHROMATIC METAL",
  ;;     "UI_MAINTAIN_SUB1_NAME_L" "Residual Goop",
  ;;     "UI_MAINTAIN_SUB4_NAME_L" "Living Slime",
  ;;     "UI_LAUNCHSUB2_NAME_L" "Deuterium",
  ;;     "UI_FUEL_1_NAME_L" "Carbon",
  ;;     "UI_STANDING_TRA" "GEK",
  ;;     "UI_MAINTAIN_SUB4_NAME" "LIVING SLIME",
  ;;     "UI_PLANTSUB_CAVE_NAME" "MARROW BULB",
  ;;     "UI_STANDING_G_TRA" "MERCHANTS",
  ;;     "UI_BLUE2_NAME_L" "Indium",
  ;;     "UI_UNITS_REWARD_NAME" "UNITS",
  ;;     "UI_CAVE2_NAME" "IONISED COBALT",
  ;;     "UI_FUEL_2_NAME_L" "Condensed Carbon",
  ;;     "UI_LUSH1_NAME" "PARAFFINIUM",
  ;;     "UI_EX_RED_NAME" "ACTIVATED CADMIUM",
  ;;     "UI_ASTEROID2_NAME" "GOLD",
  ;;     "UI_TOXIC1_NAME" "AMMONIA",
  ;;     "UI_ROCKETFUEL_NAME" "TRITIUM",
  ;;     "UI_WATERPLANT_NAME" "CYTO-PHOSPHATE",
  ;;     "UI_MAINTAIN_SUB5_NAME_L" "Viscous Fluids",
  ;;     "UI_EGG_PROCESS" "Processor Setting: Chromatic Yolk Formation",
  ;;     "UI_GAS_2_NAME_L" "Radon",
  ;;     "UI_NEWCATA2_NAME_L" "Sodium Nitrate",
  ;;     "UI_HEXITE_NAME_L" "Hexite",
  ;;     "UI_COLD1_NAME_L" "Dioxite",
  ;;     "UI_PLANTSUB_CREATUREPOOP_NAME" "FAECIUM",
  ;;     "UI_SAND1_NAME" "SILICATE POWDER",
  ;;     "UI_LAND1_NAME" "FERRITE DUST",
  ;;     "UI_WATER1_NAME_L" "Salt",
  ;;     "UI_PLANTSUB_CREATUREPOOP_NAME_L" "Faecium",
  ;;     "TECH_FRAGMENT_NAME" "NANITE CLUSTER",
  ;;     "UI_LAND2_NAME_L" "Pure Ferrite",
  ;;     "UI_RED2_NAME" "CADMIUM",
  ;;     "UI_LAND2_NAME" "PURE FERRITE",
  ;;     "UI_EX_GREEN_NAME" "ACTIVATED EMERIL",
  ;;     "UI_PLANTSUB_BARREN_NAME" "CACTUS FLESH",
  ;;     "UI_ASTEROID1_NAME_L" "Silver",
  ;;     "UI_PLANTSUB_LUSH_NAME" "STAR BULB",
  ;;     "UI_NEWWATER2_NAME_L" "Chlorine",
  ;;     "UI_QUICKSILVER_REWARD_NAME" "QUICKSILVER",
  ;;     "UI_LAND1_NAME_L" "Ferrite Dust",
  ;;     "UI_LAUNCHSUB2_NAME" "DEUTERIUM",
  ;;     "UI_YELLOW2_NAME" "COPPER",
  ;;     "UI_SUNGOLD_NAME" "LIQUID SUN",
  ;;     "UI_PLANTSUB_SCORCHED_NAME" "SOLANIUM",
  ;;     "UI_PLANTSUB_TOXIC_NAME" "FUNGAL MOULD",
  ;;     "UI_PLANTSUB_WATER_NAME" "KELP SAC",
  ;;     "UI_GAS_2_NAME" "RADON",
  ;;     "UI_MAINTAIN_SUB5_NAME" "VISCOUS FLUIDS",
  ;;     "TECH_FRAGMENT_NAME_L" "Nanite Cluster",
  ;;     "UI_PLANTSUB_SCORCHED_NAME_L" "Solanium",
  ;;     "UI_AIR1_NAME" "OXYGEN",
  ;;     "UI_MAINTAIN_SUB3_NAME_L" "Rusted Metal",
  ;;     "UI_STANDING_EXP" "KORVAX",
  ;;     "UI_EX_BLUE_NAME" "ACTIVATED INDIUM",
  ;;     "UI_NEWCATA2_NAME" "SODIUM NITRATE",
  ;;     "UI_ROCKETFUEL_NAME_L" "Tritium",
  ;;     "UI_PLANTSUB_SNOW_NAME" "FROST CRYSTAL",
  ;;     "UI_SOULFRAG_NAME_L" "Fragmented Qualia",
  ;;     "UI_STANDING_G_EXP" "EXPLORERS",
  ;;     "UI_LAUNCHSUB_NAME" "DI-HYDROGEN",
  ;;     "UI_EX_YELLOW_NAME_L" "Activated Copper",
  ;;     "UI_SOULFRAG_NAME" "FRAGMENTED QUALIA",
  ;;     "UI_GAS_3_NAME" "NITROGEN",
  ;;     "UI_RADIO1_NAME" "URANIUM",
  ;;     "SUB_DEADDRONE_NAME" "PUGNEUM",
  ;;     "UI_COLD1_NAME" "DIOXITE",
  ;;     "UI_CAVE1_NAME" "COBALT",
  ;;     "UI_ASTEROID1_NAME" "SILVER",
  ;;     "UI_LUSH1_NAME_L" "Paraffinium",
  ;;     "UI_PLANTSUB_DEADCREATURE_NAME_L" "Mordite",
  ;;     "UI_LAND3_NAME" "MAGNETISED FERRITE",
  ;;     "UI_YEAST_PROCESS" "Processor Setting: Fermentation",
  ;;     "UI_PLANTSUB_SNOW_NAME_L" "Frost Crystal",
  ;;     "UI_PLANTSUB_BARREN_NAME_L" "Cactus Flesh",
  ;;     "UI_MAINTAIN_SUB2_NAME_L" "Runaway Mould",
  ;;     "UI_PLANTSUB_RADIO_NAME" "GAMMA ROOT",
  ;;     "UI_GAS_1_NAME_L" "Sulphurine",
  ;;     "UI_NANITES_REWARD_NAME" "NANITES",
  ;;     "UI_STANDING_G_WAR" "MERCENARIES",
  ;;     "UI_ASTEROID3_NAME" "PLATINUM",
  ;;     "UI_CAVE2_NAME_L" "Ionised Cobalt",
  ;;     "UI_HOT1_NAME_L" "Phosphorus",
  ;;     "UI_FUEL_2_NAME" "CONDENSED CARBON",
  ;;     "UI_RADIO1_NAME_L" "Uranium",
  ;;     "UI_PLANTSUB_CAVE_NAME_L" "Marrow Bulb",
  ;;     "UI_WATER2_NAME" "SODIUM",
  ;;     "UI_ASTEROID2_NAME_L" "Gold",
  ;;     "UI_GAS_3_NAME_L" "Nitrogen",
  ;;     "UI_MAINTAIN_SUB2_NAME" "RUNAWAY MOULD",
  ;;     "UI_GREEN2_NAME" "EMERIL",
  ;;     "UI_MAINTAIN_SUB3_NAME" "RUSTED METAL",
  ;;     "UI_RED2_NAME_L" "Cadmium",
  ;;     "UI_ASTEROID3_NAME_L" "Platinum",
  ;;     "UI_WATER1_NAME" "SALT",
  ;;     "UI_LAUNCHSUB_NAME_L" "Di-hydrogen",
  ;;     "UI_STANDING_WAR" "VY'KEEN",
  ;;     "UI_PLANTSUB_WATER_NAME_L" "Kelp Sac",
  ;;     "UI_GAS_1_NAME" "SULPHURINE",
  ;;     "UI_YELLOW2_NAME_L" "Copper",
  ;;     "UI_EX_BLUE_NAME_L" "Activated Indium",
  ;;     "UI_CAVE1_NAME_L" "Cobalt",
  ;;     "UI_HEXITE_NAME" "HEXITE",
  ;;     "UI_PLANTSUB_TOXIC_NAME_L" "Fungal Mould"}



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

(defn recipe->substance-link
  [recipe]
  (let [{:keys [id result ingredients]} recipe]
    (into {}
          [{(:id result) {:as-result [id]}}
           (into {} (map (fn [m] {(:id m) {:as-ingredient [id]}}) ingredients))])))

(comment
  (time
   (recipe->substance-link
    (get (translate @dictionary-cache (recipe/from-file index load-record) [:name])
         "RECIPE_2"))))
  ;; => {"FOOD_P_STELLAR" {:as-result ["RECIPE_2"]}, "STELLAR2" {:as-ingredient ["RECIPE_2"]}}


(defn substance-recipes-table
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


(defn add-recipes-to-substance
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
;; 

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

(comment
  (preload-dictionary))


(defonce substance-cache (atom {}))

(defn substances
  []
  (if (empty? @substance-cache)
    (reset! substance-cache
            (translate @dictionary-cache (substance/from-file index load-record) [:name :namelower]))
    @substance-cache))

(defonce recipes-cache (atom {}))

(defn recipes
  []
  (if (empty? @recipes-cache)
    (reset! recipes-cache
            (translate @dictionary-cache (recipe/from-file index load-record) [:name]))
    @recipes-cache))

(defonce products-cache (atom {}))

(defn products
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
;; => #'net.fiendishplatypus.file.index/reset-caches
(comment (reset-caches))

(defn get-substance
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
                                                  recipe-id                    (:id result)
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
   (take 56
         (filter (fn [x] (not (and (empty? (:as-ingredient x))
                                   (empty? (:as-result x)))))
                 (map get-substance
                      (vals (substances)))))))
;; => 56
;; 
;; (sort-by (fn [a] (:name a)) <)

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

;;(spit "resources/public/data.edn" (pr-str (into [] (substances->ui))))
;;


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

;;(get (name->id) "Wriggling Jam")
;;
;;(spit "resources/public/nameToIdLookup.edn" (pr-str (name->id)))

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
  (substances)
  (product-dictionary)
  (substance-dictionary))

(get substance-dictionary "PLANT_CAVE")


(search-for {"PLANT_CAVE" {:name "UI_PLANTSUB_CAVE_NAME", :id "PLANT_CAVE", :namelower "UI_PLANTSUB_CAVE_NAME_L"}}
            [:name :namelower])
