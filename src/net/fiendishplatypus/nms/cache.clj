(ns net.fiendishplatypus.nms.cache
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [taoensso.timbre :as timbre]
            [net.fiendishplatypus.nms.file :as file])
  (:import (java.io PushbackReader IOException File)))


;; Cache operations
(def cache-file-spec  {:file :file :digest :string})


(def cache-line-spec
  {:name :string
   :mbin {:file :file :digest :string}
   :exml {:file :file :digest :string}})


(def run-cache (atom {}))


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


(defn init-cache []
  (reset! run-cache (preload-cache)))


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


(defn update-cache
  "Take `cache-atom` and add new filename and entry to cache line"
  ([cache filename entry]
   (swap! cache (fn [c k v]
                       (assoc c k v))
          filename entry))
  ([filename entry]
   (update-cache run-cache filename entry)))


(defn entry
  ([cache file]
   (get cache (file/name (.getName file))))
  ([file]
   (entry @run-cache file)))


(defn- exml-cached?
  [cache-entry]
  (let [cached-exml-digest (get-in cache-entry [:exml :digest] "")
        current-exml-digest (:digest (file/file-and-digest (get-in cache-entry [:exml :file])))]
    (= cached-exml-digest current-exml-digest)))


(defn cached?
  "get a map of mbin :file and :digest.
   Return true if both mbin and exml entries exist in a converter cache and
   their digests are matching"
  [new-file]
  (let [cache-entry (entry @run-cache (:file new-file))
        new-mbin-digest (:digest new-file)
        cached-mbin-digest (get-in cache-entry [:mbin :digest] "")
        mbin-match (= cached-mbin-digest new-mbin-digest)]
    (timbre/info "MBIN cache check for" (:file new-file) "(" mbin-match ")")
    (if mbin-match
      (do (timbre/debug "MBIN" (:file new-file) "cached.")
          (if (exml-cached? cache-entry)
            (do (timbre/debug "EXML" (:file new-file) "cached.")
                true)
            false))
      false)))


(comment
  (init-cache)
  (cached? (file/file-and-digest (first (net.fiendishplatypus.nms.setup/list-lang-files)))))


(comment
  "Cache operations"

  @run-cache

  (persist-cache (cache-file) @run-cache)

  (load-edn
    (clojure.java.io/file (System/getenv "LOCALAPPDATA")
                          "nms-atlas-converter.cache")))
