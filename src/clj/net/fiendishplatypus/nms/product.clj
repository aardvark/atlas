(ns net.fiendishplatypus.nms.product
  (:require [clojure.string]
            [clojure.java.io]
            [clojure.data.xml :as xml]))


(def index-meta {:start-mark "GcProductData.xml"
                 :end? (fn [s] (= s "    </Property>"))
                 :file "D:\\NMSProjects\\Notepad\\METADATA\\REALITY\\TABLES\\NMS_REALITY_GCPRODUCTTABLE.EXML"})


(defn parse [m]
  (let [name  (get-in m [:attrs :name])
        value (get-in m [:attrs :value])]
    (case name
      "Id"         {:id value}
      "NameLower"  {:namelower value}
      "Name"       {:name value}
      nil)))


(defn product
  [xml-str]
  (into {} (map parse (:content (xml/parse-str xml-str)))))
  
  
(comment 
  (with-open [rdr (clojure.java.io/reader "resources/nms/product-example.xml")]
    (let [lines (clojure.string/join (line-seq rdr))]
      (product lines))))
;; => {"CASING" {:id "CASING", :name "CASING_NAME", :namelower "CASING_NAME_L"}}

(defn index
  [indexer]
  (indexer (:start-mark index-meta)
           (:end? index-meta)
           (:file index-meta)))


(defn from-file
  [indexer loader]
  (into {}
        (map (fn [record]
               (let [entity (product (loader (:file index-meta) record))]
                 {(:id entity) entity}))
             (index indexer))))
