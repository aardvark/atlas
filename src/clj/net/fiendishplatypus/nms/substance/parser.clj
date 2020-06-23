(ns net.fiendishplatypus.nms.substance.parser
  (:require [clojure.data.xml :as xml]
            [clojure.java.io]
            [clojure.string]
            [net.fiendishplatypus.file.index :as index]))


(def substance
  {:tag :Property
   :attrs {:value "GcRealitySubstanceData.xml"}
   :content
   '({:tag :Property
      :attrs {:name "Name", :value "UI_FUEL_1_NAME"}
      :content '()}
     {:tag :Property
      :attrs {:name "ID", :value "FUEL1"}
      :content '()}
     {:tag :Property
      :attrs {:name "NameLower", :value "UI_FUEL_1_NAME_L"}
      :content '()})})

(defn parse-substance 
  [substance]
  (let [res
        (into {}
              (map
                (fn [{:keys [name value]}]
                  {(keyword (clojure.string/lower-case name)) value})
                (map :attrs (:content substance))))]
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

(comment
  (parse-substance
   (xml/parse-str
    (index/load-record "resources/nms/substance-example.xml"
                       (second
                        (index/index "GcRealitySubstanceData.xml"
                                     (fn [s] (= s "    </Property>"))
                                     "resources/nms/substance-example.xml"))))))
