(ns net.fiendishplatypus.nms.cache
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [taoensso.timbre :as timbre])
  (:import (java.io PushbackReader IOException File)))


;; Cache operations
(def cache-file-spec  {:file :file :digest :string})

(def cache-line-spec
  {:name :string
   :mbin {:file :file :digest :string}
   :exml {:file :file :digest :string}})

(defn- load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (clojure.java.io/reader source)]
      (edn/read (PushbackReader. r)))

    (catch IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))

(defn cache-file
  ([]
   (cache-file (System/getenv "LOCALAPPDATA")))
  ([dir-path]
   (io/file dir-path "nms-atlas-converter.cache")))

(defn- preload-cache
  "tries to preload cache from file"
  []
  (let [cache (cache-file)]
    (if (.exists cache)
      (do (timbre/info "Found existing cache file:" cache)
          (into {}
                (map (fn [[k {:keys [mbin exml]}]]
                       {k
                        {:mbin (update mbin :file io/as-file)
                         :exml (update exml :file io/as-file)}}))
                (load-edn cache)))
      {})))

(defn- persist-cache
  "Take existing cache map `cache` and save it to the file `cache-file`"
  [cache-file cache]
  (let [cache (letfn [(to-path [x [k v]]
                        [k (update-in v
                                      [x :file]
                                      (fn [x]
                                        (if (= File (class x))
                                          (.getPath x)
                                          x)))])
                      (strip-mbin []
                        (partial to-path :mbin))
                      (strip-exml []
                        (partial to-path :exml))]
                (into {} (map
                           (comp (strip-exml) (strip-mbin)))
                      cache))]

    (try
      (with-open [w (clojure.java.io/writer cache-file)]
        (spit w cache))
      (catch RuntimeException e
        (timbre/error e "Unable to persist cache" cache-file)))))

(defn- update-cache
  "Take `cache-atom` and add new filename and entry to cache line"
  [cache-atom filename entry]
  (swap! cache-atom (fn [c k v]
                      (assoc c k v))
         filename entry))


(def run-cache
  (atom (preload-cache)))

(comment
  "Cache operations"

  @run-cache

  (persist-cache (cache-file) @run-cache)

  (load-edn
    (clojure.java.io/file (System/getenv "LOCALAPPDATA")
                          "nms-atlas-converter.cache")))
