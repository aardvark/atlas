(ns net.fiendishplatypus.nms.file
  (:require [clojure.string]))

(defn name
  [file]
  (let [[name _ext] (clojure.string/split file #"\.")]
    name))
