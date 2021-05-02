(ns net.fiendishplatypus.zip
  (:require [clojure.zip :as zip]))


(defn parse-zip
  "Given node matcher predicate `match?`,
   node to map trasform function `tform`,
   and xml zipper `z`
   will traverse zipper collect all matching nodes and 
   return transfromed value in a map
  "
  [match? tform z]
  (loop [z z
         acc {}]
    (if (nil? z) acc
        (let [n (zip/node z)]
          (if (= (zip/next z) z)
            acc
            (if (match? n)
              (recur (clojure.zip/right z) (merge (tform n) acc))
              (recur (clojure.zip/next z) acc)))))))