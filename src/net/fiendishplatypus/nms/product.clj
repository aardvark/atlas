(ns net.fiendishplatypus.nms.product
  (:require [clojure.string]
            [clojure.java.io]
            [clojure.data.xml :as xml]
            [net.fiendishplatypus.nms.setup :as setup]))

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
  (indexer "GcProductData.xml"
           (fn [s] (= s "    </Property>"))
           (.getPath (setup/product-file!))))


(defn from-file
  [indexer loader]
  (into {}
        (map (fn [record]
               (let [entity (product (loader (.getPath (setup/product-file!)) record))]
                 {(:id entity) entity}))
             (index indexer))))
