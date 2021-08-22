(ns net.fiendishplatypus.nms.parser.main
  (:require [net.fiendishplatypus.nms.setup :as setup]
            [net.fiendishplatypus.nms.index :as index]
            [net.fiendishplatypus.nms.cache :as cache])
  (:gen-class))

(defn -main
  [args]
  (setup/init-config args)
  (cache/init-cache)
  (setup/create-files)
  (cache/persist-cache)
  (index/preload-dictionary)
  (index/substances)
  (index/recipes)
  (index/products))

(comment (-main []))

