(ns net.fiendishplatypus.nms.language
  (:require [clojure.string]
            [clojure.java.io]
            [clj-xpath.core :as xpath]))


;; xml parsing thorugh xpath
(comment 
  (with-open [rdr (clojure.java.io/reader "resources/nms/language-example.xml")]
    (let [xml (line-seq rdr)
          xml (clojure.string/join xml)]
      (into {}
            (map 
             (fn [id] 
               {id 
                (:value 
                 (xpath/$x:attrs 
                  (str "//Property[@value='TkLocalisationEntry.xml']/Property[@name='Id' and @value='" id "']/../Property[@name='English']/Property") xml))})
             (map :value (xpath/$x:attrs* "//Property[@value='TkLocalisationEntry.xml']/Property[@name='Id']" xml)))))))


(defn language
  [xml id]
  {id (:value 
       (try 
         (xpath/$x:attrs 
          (str "//Property[@value='TkLocalisationEntry.xml']/Property[@name='Id' and @value='" id "']/../Property[@name='English']/Property") xml)
         (catch Exception e (println (str "Exception on id: " id)))))})


(def index-meta {:start-mark "TkLocalisationEntry.xml"
                 :end? (fn [s] (= s "    </Property>"))})

(defn file->language
  [file indexer record-loader]
  (let [index (indexer file)]
       [record-loader (partial file)]
    (into {} (for [x index]
               (language (record-loader x) (:id x))))))
