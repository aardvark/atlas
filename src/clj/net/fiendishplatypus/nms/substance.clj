(ns net.fiendishplatypus.nms.substance
  (:require [clojure.data.xml :as xml]
            [clojure.java.io]
            [clojure.string]))
            
            


(defn parse-substance 
  [entity]
  (let [res
        (into {}
              (map
                (fn [{:keys [name value]}]
                  {(keyword (clojure.string/lower-case name)) value})
                (map :attrs (:content entity))))]
   {(:id res) (select-keys res [:name :id :namelower])}))


(comment 
  (parse-substance substance))
;; => {"FUEL1" {:name "UI_FUEL_1_NAME", :id "FUEL1", :namelower "UI_FUEL_1_NAME_L"}}

(defn value->key [m k]
  (keyword (clojure.string/lower-case (k m))))

(comment (value->key {:name "Table"} :name))
         ;; => :table

(defn substances 
  [file]
  (into {}
        (with-open [rdr (clojure.java.io/reader file)]
          (let [xml (xml/parse rdr)
                inner
                (for [x (:content xml)]
                  {(value->key (:attrs x) :name) 
                   (into {} (map parse-substance (:content x)))})]
           (cons (:attrs xml) inner)))))

(comment 
  (substances "resources/nms/substance-example.xml"))
  ;; => {:template "GcSubstanceTable",
  ;;     :table
  ;;     {"FUEL1" {:name "UI_FUEL_1_NAME", :id "FUEL1", :namelower "UI_FUEL_1_NAME_L"},
  ;;      "DIfferent" {:name "OTHER", :id "DIfferent", :namelower "SOMETHING"}},
  ;;     :crafting {}}


;; expected parsed table
(def expected-transformation-result
  {:template "GcSubstanceTable"
   :table {"FUEL1" {:ID "FUEL1" :name "UI_FUEL_1_NAME" :name-lower "UI_FUEL_1_NAME_L"}}
   :crafting nil})


(defn substance
  [record]
  (->> record (xml/parse-str)
              (parse-substance)))


(def index-meta {:start-mark "GcRealitySubstanceData.xml"
                 :end? (fn [s] (= s "    </Property>"))})

(defn substanse-index
  [indexer file]
  (indexer (:start-mark index-meta) (:end? index-meta) file))
  

(defn file->substance 
  [file]
  (let [index (substanse-indexer file)]
    (into {}
          (map 
           (fn [entry] (parse-substance 
                        (xml/parse-str 
                         (index/load-record file entry))))
           index))))


(comment 
  (file->substance
   "D:\\NMSProjects\\Notepad\\METADATA\\REALITY\\TABLES\\NMS_REALITY_GCSUBSTANCETABLE.EXML"))
  ;; => {"WAR_STANDING_UP" {:name "UI_STANDING_WAR", :id "WAR_STANDING_UP", :namelower "UI_STANDING_WAR"},
  ;;     "ASTEROID3" {:name "UI_ASTEROID3_NAME", :id "ASTEROID3", :namelower "UI_ASTEROID3_NAME_L"},
  ;;     "OXYGEN" {:name "UI_AIR1_NAME", :id "OXYGEN", :namelower "UI_AIR1_NAME_L"},
  ;;     "CATALYST2" {:name "UI_NEWCATA2_NAME", :id "CATALYST2", :namelower "UI_NEWCATA2_NAME_L"},
  ;;     "PLANT_SNOW" {:name "UI_PLANTSUB_SNOW_NAME", :id "PLANT_SNOW", :namelower "UI_PLANTSUB_SNOW_NAME_L"},
  ;;     "HOT1" {:name "UI_HOT1_NAME", :id "HOT1", :namelower "UI_HOT1_NAME_L"},
  ;;     "PLANT_TOXIC" {:name "UI_PLANTSUB_TOXIC_NAME", :id "PLANT_TOXIC", :namelower "UI_PLANTSUB_TOXIC_NAME_L"},
  ;;     "SPACEGUNK2" {:name "UI_MAINTAIN_SUB2_NAME", :id "SPACEGUNK2", :namelower "UI_MAINTAIN_SUB2_NAME_L"},
  ;;     "ROBOT1" {:name "SUB_DEADDRONE_NAME", :id "ROBOT1", :namelower "SUB_DEADDRONE_NAME_L"},
  ;;     "TGUILD_STAND_DN" {:name "UI_STANDING_G_TRA", :id "TGUILD_STAND_DN", :namelower "UI_STANDING_G_TRA"},
  ;;     "CREATURE1" {:name "UI_PLANTSUB_DEADCREATURE_NAME", :id "CREATURE1", :namelower "UI_PLANTSUB_DEADCREATURE_NAME_L"},
  ;;     "WATER2" {:name "UI_NEWWATER2_NAME", :id "WATER2", :namelower "UI_NEWWATER2_NAME_L"},
  ;;     "EX_YELLOW" {:name "UI_EX_YELLOW_NAME", :id "EX_YELLOW", :namelower "UI_EX_YELLOW_NAME_L"},
  ;;     "ROCKETSUB" {:name "UI_ROCKETFUEL_NAME", :id "ROCKETSUB", :namelower "UI_ROCKETFUEL_NAME_L"},
  ;;     "TECHFRAG_R" {:name "UI_NANITES_REWARD_NAME", :id "TECHFRAG_R", :namelower "UI_NANITES_REWARD_NAME"},
  ;;     "SPACEGUNK3" {:name "UI_MAINTAIN_SUB3_NAME", :id "SPACEGUNK3", :namelower "UI_MAINTAIN_SUB3_NAME_L"},
  ;;     "STELLAR2" {:name "UI_STELLAR2_NAME", :id "STELLAR2", :namelower "UI_STELLAR2_NAME_L"},
  ;;     "WGUILD_STAND_DN" {:name "UI_STANDING_G_WAR", :id "WGUILD_STAND_DN", :namelower "UI_STANDING_G_WAR"},
  ;;     "CAVE2" {:name "UI_CAVE2_NAME", :id "CAVE2", :namelower "UI_CAVE2_NAME_L"},
  ;;     "PLANT_WATER" {:name "UI_PLANTSUB_WATER_NAME", :id "PLANT_WATER", :namelower "UI_PLANTSUB_WATER_NAME_L"},
  ;;     "EX_RED" {:name "UI_EX_RED_NAME", :id "EX_RED", :namelower "UI_EX_RED_NAME_L"},
  ;;     "TRA_STANDING_UP" {:name "UI_STANDING_TRA", :id "TRA_STANDING_UP", :namelower "UI_STANDING_TRA"},
  ;;     "WAR_STANDING_DN" {:name "UI_STANDING_WAR", :id "WAR_STANDING_DN", :namelower "UI_STANDING_WAR"},
  ;;     "QUICKSILVER" {:name "UI_QUICKSILVER_REWARD_NAME", :id "QUICKSILVER", :namelower "UI_QUICKSILVER_REWARD_NAME"},
  ;;     "SAND1" {:name "UI_SAND1_NAME", :id "SAND1", :namelower "UI_SAND1_NAME_L"},
  ;;     "CAVE1" {:name "UI_CAVE1_NAME", :id "CAVE1", :namelower "UI_CAVE1_NAME_L"},
  ;;     "LAUNCHSUB" {:name "UI_LAUNCHSUB_NAME", :id "LAUNCHSUB", :namelower "UI_LAUNCHSUB_NAME_L"},
  ;;     "UNITS" {:name "UI_UNITS_REWARD_NAME", :id "UNITS", :namelower "UI_UNITS_REWARD_NAME"},
  ;;     "CATALYST1" {:name "UI_WATER2_NAME", :id "CATALYST1", :namelower "UI_WATER2_NAME_L"},
  ;;     "DUSTY1" {:name "UI_DUSTY1_NAME", :id "DUSTY1", :namelower "UI_DUSTY1_NAME_L"},
  ;;     "TRA_STANDING_DN" {:name "UI_STANDING_TRA", :id "TRA_STANDING_DN", :namelower "UI_STANDING_TRA"},
  ;;     "LUSH1" {:name "UI_LUSH1_NAME", :id "LUSH1", :namelower "UI_LUSH1_NAME_L"},
  ;;     "PLANT_RADIO" {:name "UI_PLANTSUB_RADIO_NAME", :id "PLANT_RADIO", :namelower "UI_PLANTSUB_RADIO_NAME_L"},
  ;;     "WATER1" {:name "UI_WATER1_NAME", :id "WATER1", :namelower "UI_WATER1_NAME_L"},
  ;;     "GAS2" {:name "UI_GAS_2_NAME", :id "GAS2", :namelower "UI_GAS_2_NAME_L"},
  ;;     "PLANT_HOT" {:name "UI_PLANTSUB_SCORCHED_NAME", :id "PLANT_HOT", :namelower "UI_PLANTSUB_SCORCHED_NAME_L"},
  ;;     "FUEL1" {:name "UI_FUEL_1_NAME", :id "FUEL1", :namelower "UI_FUEL_1_NAME_L"},
  ;;     "GAS3" {:name "UI_GAS_3_NAME", :id "GAS3", :namelower "UI_GAS_3_NAME_L"},
  ;;     "GREEN2" {:name "UI_GREEN2_NAME", :id "GREEN2", :namelower "UI_GREEN2_NAME_L"},
  ;;     "WGUILD_STAND_UP" {:name "UI_STANDING_G_WAR", :id "WGUILD_STAND_UP", :namelower "UI_STANDING_G_WAR"},
  ;;     "TOXIC1" {:name "UI_TOXIC1_NAME", :id "TOXIC1", :namelower "UI_TOXIC1_NAME_L"},
  ;;     "RADIO1" {:name "UI_RADIO1_NAME", :id "RADIO1", :namelower "UI_RADIO1_NAME_L"},
  ;;     "BLUE2" {:name "UI_BLUE2_NAME", :id "BLUE2", :namelower "UI_BLUE2_NAME_L"},
  ;;     "EX_BLUE" {:name "UI_EX_BLUE_NAME", :id "EX_BLUE", :namelower "UI_EX_BLUE_NAME_L"},
  ;;     "EXP_STANDING_DN" {:name "UI_STANDING_EXP", :id "EXP_STANDING_DN", :namelower "UI_STANDING_EXP"},
  ;;     "EGUILD_STAND_DN" {:name "UI_STANDING_G_EXP", :id "EGUILD_STAND_DN", :namelower "UI_STANDING_G_EXP"},
  ;;     "SPECIAL_POOP" {:name "UI_HEXITE_NAME", :id "SPECIAL_POOP", :namelower "UI_HEXITE_NAME_L"},
  ;;     "RED2" {:name "UI_RED2_NAME", :id "RED2", :namelower "UI_RED2_NAME_L"},
  ;;     "SUNGOLD" {:name "UI_SUNGOLD_NAME", :id "SUNGOLD", :namelower "UI_SUNGOLD_NAME_L"},
  ;;     "LAND3" {:name "UI_LAND3_NAME", :id "LAND3", :namelower "UI_LAND3_NAME_L"},
  ;;     "ASTEROID1" {:name "UI_ASTEROID1_NAME", :id "ASTEROID1", :namelower "UI_ASTEROID1_NAME_L"},
  ;;     "FUEL2" {:name "UI_FUEL_2_NAME", :id "FUEL2", :namelower "UI_FUEL_2_NAME_L"},
  ;;     "TECHFRAG" {:name "TECH_FRAGMENT_NAME", :id "TECHFRAG", :namelower "TECH_FRAGMENT_NAME_L"},
  ;;     "SPACEGUNK4" {:name "UI_MAINTAIN_SUB4_NAME", :id "SPACEGUNK4", :namelower "UI_MAINTAIN_SUB4_NAME_L"},
  ;;     "WATERPLANT" {:name "UI_WATERPLANT_NAME", :id "WATERPLANT", :namelower "UI_WATERPLANT_NAME_L"},
  ;;     "PLANT_POOP" {:name "UI_PLANTSUB_CREATUREPOOP_NAME", :id "PLANT_POOP", :namelower "UI_PLANTSUB_CREATUREPOOP_NAME_L"},
  ;;     "LAND1" {:name "UI_LAND1_NAME", :id "LAND1", :namelower "UI_LAND1_NAME_L"},
  ;;     "EX_GREEN" {:name "UI_EX_GREEN_NAME", :id "EX_GREEN", :namelower "UI_EX_GREEN_NAME_L"},
  ;;     "TGUILD_STAND_UP" {:name "UI_STANDING_G_TRA", :id "TGUILD_STAND_UP", :namelower "UI_STANDING_G_TRA"},
  ;;     "LAUNCHSUB2" {:name "UI_LAUNCHSUB2_NAME", :id "LAUNCHSUB2", :namelower "UI_LAUNCHSUB2_NAME_L"},
  ;;     "PLANT_DUST" {:name "UI_PLANTSUB_BARREN_NAME", :id "PLANT_DUST", :namelower "UI_PLANTSUB_BARREN_NAME_L"},
  ;;     "YELLOW2" {:name "UI_YELLOW2_NAME", :id "YELLOW2", :namelower "UI_YELLOW2_NAME_L"},
  ;;     "SOULFRAG" {:name "UI_SOULFRAG_NAME", :id "SOULFRAG", :namelower "UI_SOULFRAG_NAME_L"},
  ;;     "SPACEGUNK1" {:name "UI_MAINTAIN_SUB1_NAME", :id "SPACEGUNK1", :namelower "UI_MAINTAIN_SUB1_NAME_L"},
  ;;     "EXP_STANDING_UP" {:name "UI_STANDING_EXP", :id "EXP_STANDING_UP", :namelower "UI_STANDING_EXP"},
  ;;     "LAND2" {:name "UI_LAND2_NAME", :id "LAND2", :namelower "UI_LAND2_NAME_L"},
  ;;     "SPACEGUNK5" {:name "UI_MAINTAIN_SUB5_NAME", :id "SPACEGUNK5", :namelower "UI_MAINTAIN_SUB5_NAME_L"},
  ;;     "PLANT_CAVE" {:name "UI_PLANTSUB_CAVE_NAME", :id "PLANT_CAVE", :namelower "UI_PLANTSUB_CAVE_NAME_L"},
  ;;     "ASTEROID2" {:name "UI_ASTEROID2_NAME", :id "ASTEROID2", :namelower "UI_ASTEROID2_NAME_L"},
  ;;     "PLANT_LUSH" {:name "UI_PLANTSUB_LUSH_NAME", :id "PLANT_LUSH", :namelower "UI_PLANTSUB_LUSH_NAME_L"},
  ;;     "EGUILD_STAND_UP" {:name "UI_STANDING_G_EXP", :id "EGUILD_STAND_UP", :namelower "UI_STANDING_G_EXP"},
  ;;     "GAS1" {:name "UI_GAS_1_NAME", :id "GAS1", :namelower "UI_GAS_1_NAME_L"},
  ;;     "COLD1" {:name "UI_COLD1_NAME", :id "COLD1", :namelower "UI_COLD1_NAME_L"}}

