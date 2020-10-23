(ns net.fiendishplatypus.nms.language
  (:require [clojure.string]
            [clojure.java.io]
            [clojure.zip :as zip]
            [clojure.data.xml :as xml]))


(defn extract
  [xs]
  (apply hash-map
         (for [x xs
               :let [id (get-in x [:attrs :name])]
               :when (or (=  id "Id") (= id "English"))]
           (if (= id "Id")
             (get-in x [:attrs :value])
             (:value (:attrs (first (:content x))))))))


(defn parse-zip
  [z acc]
  (if (nil? z) acc
      (let [n (zip/node z)]
        (if (= (zip/next z) z)
          acc
          (if (and (= :Property (:tag n))
                   (= "TkLocalisationEntry.xml" (get-in n [:attrs :value] "")))
            (recur (clojure.zip/right z) (merge (extract (:content n)) acc))
            (recur (clojure.zip/next z) acc))))))

(defn language
  [xml _]
  (parse-zip
   (zip/xml-zip
    (xml/parse-str xml))
   {}))


(def index-meta {:start-mark "TkLocalisationEntry.xml"
                 :end? (fn [s] (= s "    </Property>"))})

(defn file->language
  [file indexer record-loader]
  (let [index (indexer file)]
       [record-loader (partial file)]
    (into {} (for [x index]
               (language (record-loader x) (:id x))))))
