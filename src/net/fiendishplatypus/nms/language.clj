(ns net.fiendishplatypus.nms.language
  (:require [clojure.string]
            [clojure.java.io]
            [clojure.zip :as zip]
            [clojure.data.xml :as xml]
            [net.fiendishplatypus.zip :as z]))


(defn extract
  [xs]
  (apply hash-map
         (for [x xs
               :let [id (get-in x [:attrs :name])]
               :when (or (=  id "Id") (= id "English"))]
           (if (= id "Id")
             (get-in x [:attrs :value])
             (:value (:attrs (first (:content x))))))))


(defn language-node-matcher 
  "Given a zip/node `n` return true if node match language node"
  [n]
  (and (= :Property (:tag n))
       (= "TkLocalisationEntry.xml" (get-in n [:attrs :value] ""))))

(defn language
  [xml _]
  (z/parse-zip
   language-node-matcher
   (fn [n] (extract (:content n)))
   (zip/xml-zip
    (xml/parse-str xml))))


(def index-meta {:start-mark "TkLocalisationEntry.xml"
                 :end? (fn [s] (= s "    </Property>"))})

(defn file->language
  [file indexer record-loader]
  (let [index (indexer file)]
       [record-loader (partial file)]
    (into {} (for [x index]
               (language (record-loader x) (:id x))))))
