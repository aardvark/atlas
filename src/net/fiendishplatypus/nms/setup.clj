(ns net.fiendishplatypus.nms.setup
  "Contain accessor functions for EXML files and for calling
   MBINCompiler process.
   Should be mostly used to rerun dictonaries creation when new
   version of NMS is released and recipes data can be changed.
   Invoking EXML files will run a full MBIN -> EXML transformation steps.
   
   Input (MBIN) output (EXML) file pairs are checked by hash to skip 
   MBINComplier calls when not needed."
  (:require [clojure.java.io]
            [clojure.java.shell]
            [clojure.string]
            [clojure.edn]
            [omniconf.core :as cfg]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

;; Configuration 

(cfg/define
  {:lang-mbin-dir {:description "Directory where MBIN LANGUAGE files are located"
                   :type :string}
   :mbin-compiler-dir {:description "Directory where MBINCompiler.exe can be found"
                       :type :string}
   :substance-mbin-dir {:description "Directory where NMS_REALITY_GCSUBSTANCETABLE.MBIN can be found"
                        :type :string}
   :product-mbin-dir {:description "Directory where NMS_REALITY_GCPRODUCTTABLE.MBIN can be found"
                      :type :string}})


(defn init-config []
  (let [f (clojure.java.io/file (System/getenv "LOCALAPPDATA")
                                "nms-atlas-converter.config.edn")]
    (if (.exists f) (cfg/populate-from-file f)
        (cfg/populate-from-map
        ;;  TODO: Pass root from parameters "net.fiendishplatypus.nms.setup.unpacked.root"
         {:nms-unpacked-root "C:\\Projects\\NMSUnpacked"
          :lang-mbin-dir "C:\\Projects\\NMSUnpacked\\LANGUAGE"
          :mbin-compiler-dir "C:\\Projects\\NoManSkyMods"
          :substance-filename "NMS_REALITY_GCSUBSTANCETABLE.MBIN"
          :substance-mbin-dir "C:\\Projects\\NMSUnpacked\\METADATA\\REALITY\\TABLES"
          :product-mbin-dir "C:\\Projects\\NMSUnpacked\\METADATA\\REALITY\\TABLES"
          :product-filename "NMS_REALITY_GCPRODUCTTABLE.MBIN"
          :recipe-mbin-dir "C:\\Projects\\NMSUnpacked\\METADATA\\REALITY\\TABLES"
          :recipe-filename "NMS_REALITY_GCRECIPETABLE.MBIN"}))))
 



;; MBIN file lookup operation

(defn product-mbin-file
  []
  (clojure.java.io/file (cfg/get :product-mbin-dir)
                        (cfg/get :product-filename)))

(defn substance-mbin-file
  []
  (clojure.java.io/file (cfg/get :substance-mbin-dir) 
                        (cfg/get :substance-filename)))

(defn recipe-mbin-file
  []
  (clojure.java.io/file (cfg/get :recipe-mbin-dir)
                        (cfg/get :recipe-filename)))

(defn engilsh-lang-file?
  [name]
  (not (nil? (re-matches #"NMS_\w+_ENGLISH.MBIN" name))))


(def engilsh-lang-file
  (reify
    java.io.FilenameFilter
    (accept [this _ name]
      (engilsh-lang-file? name))))


(defn list-lang-files
  ([path-to-lang-dir]
   (.listFiles (java.io.File. path-to-lang-dir)
               engilsh-lang-file))
  ([]
   (list-lang-files (cfg/get :lang-mbin-dir))))

;; File checksum operations

(defn file-to-a-digest
  [file]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        digest-arr (with-open [s (clojure.java.io/make-input-stream file {})
                               dis (new java.security.DigestInputStream s md)]
                     (while (not= (.read dis) -1))
                     (.digest (.getMessageDigest dis)))]
    digest-arr))

(defn file-and-digest
  [file]
  {:file file
   :digest (clojure.string/join (map #(format "%02x" %)
                                     (seq (file-to-a-digest file))))})

(comment
  (map file-and-digest (list-lang-files)))


;; `file-and-digest` function can be used to do a smart conversion.
;; Meaning we can only run conversion to XML if new file is different
;; from previous one.
;; This raise a separate question of storing 'conversion-run'
;; results somewhere and loading them for checking on the next run
;; saving map of :filename and :sha256sum should be enough
;; :mbin-file and :exml-file parameters can be always added as they
;; appear in the processing step
;; 
;; How this should work?
;; 
;; * Check if env LOCALAPPDATA dir contains existing snapshot run info.
;; * for each file:
;; ** check in and out check checksums, if one of them not match, redo file
;;    conversion
;; ** if both match simply return existing file


;; Cache operations 
(def run-info-spec
  {:name :string
   :mbin {:file :file :digest :string}
   :exml {:file :file :digest :string}})

(def cache-file
  (clojure.java.io/file (System/getenv "LOCALAPPDATA") "nms-atlas-converter.cache"))

(defn- load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (clojure.java.io/reader source)]
      (clojure.edn/read (java.io.PushbackReader. r)))

    (catch java.io.IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))


(defn- preload-cache
  []
  (if (.exists cache-file)
    (do (info "Found existing cache file:" cache-file)
        (load-edn cache-file))
    {}))


(defn persist-cache
  [run-cache]
  (let [run-cache (letfn [(to-path [x [k v]]
                            [k (update-in v
                                          [x :file]
                                          (fn [x]
                                            (if (= java.io.File (class x))
                                              (.getPath x)
                                              x)))])
                          (strip-mbin []
                            (partial to-path :mbin))
                          (strip-exml []
                            (partial to-path :exml))]
                    (into {} (map
                              (comp (strip-exml) (strip-mbin)))
                          run-cache))]

    (try
      (with-open [w (clojure.java.io/writer cache-file)]
        (spit w run-cache))
      (catch RuntimeException e
        (error e "Unable to persist cache t " cache-file)))))


(def run-cache
  (atom (preload-cache)))

(comment
  "Cache operations"
  @run-cache
  (letfn [(to-path [x [k v]]
            [k (update-in v [x :file] (fn [^java.io.File x] (.getPath x)))])
          (strip-mbin []
            (partial to-path :mbin))
          (strip-exml []
            (partial to-path :exml))]
    (into {} (map
              (comp (strip-exml) (strip-mbin)))
          @run-cache))
  (persist-cache @run-cache)
  (load-edn
   (clojure.java.io/file (System/getenv "LOCALAPPDATA")
                         "nms-atlas-converter.cache")))


(defn file-to-name-ext-pair
  [filename]
  (let [[name ext] (clojure.string/split filename #"\.")]
    {:name name :ext ext}))


(defn cache-entry
  [file]
  (let [name (:name (file-to-name-ext-pair (.getName file)))]
    (get @run-cache name)))

(defn exml-cached?
  [cache-entry]
  (let [cached-exml-digest (get-in cache-entry [:exml :digest] "")
        current-exml-digest (:digest (file-and-digest (get-in cache-entry [:exml :file])))]
    (= cached-exml-digest current-exml-digest)))


(defn mbin-cached?
  "get a map of mbin :file and :digest. 
   Return true if both mbin and exml entries exist in a converter cache and
   their digests are matching"
  [new-file]
  (let [cache-entry (cache-entry (:file new-file))
        new-mbin-digest (:digest new-file)
        cached-mbin-digest (get-in cache-entry [:mbin :digest] "")
        mbin-match (= cached-mbin-digest new-mbin-digest)]
    (info "MBIN cache check for" (:file new-file) "(" mbin-match ")")
    (if mbin-match
      (do (debug "MBIN" (:file new-file) "cached.")
          (if (exml-cached? cache-entry)
            (do (debug "EXML" (:file new-file) "cached.")
                true)
            false))
      false)))

(comment
  (mbin-cached? (file-and-digest (first (list-lang-files)))))

;; MBIN decompiler invocation
(defn exml
  "given a mbin file `file` run MBINComplier on it to get a EXML file 
   and return such EXML file back"
  [file]
  (info "Running MBIN -> EXML conversion for " (.toString (.toPath file)))
  (let [path-str (.toString (.toPath file))
        filename (.getName file)
        parent-dir (.getParent file)
        exml-file-path (str parent-dir
                            "\\"
                            (:name (file-to-name-ext-pair filename))
                            ".EXML")
        mbin-compiler-dir (cfg/get :mbin-compiler-dir)
        _  (clojure.java.shell/with-sh-dir mbin-compiler-dir
             (clojure.java.shell/sh "cmd.exe" "/C" "call" "MBINCompiler.exe" "-q" path-str))
        exml (java.io.File. exml-file-path)]
    exml))


(defn mbin->exml
  ""
  [file-def]
  (if (mbin-cached? file-def)
    (:exml (cache-entry (:file file-def)))
    ;; not cached so we need to run exml transformation
    (let [file (:file file-def)
          filename (:name (file-to-name-ext-pair (.getName file)))
          exml-def (file-and-digest (exml file))]
      (swap! run-cache (fn [c k v]
                         (assoc c k v))
             filename {:mbin file-def
                       :exml exml-def})
      exml-def)))

(comment
  (mbin->exml (file-and-digest (second (list-lang-files))))
  (update (file-and-digest (second (list-lang-files)))
          :file #(.getPath %))

  (clojure.java.io/file (System/getenv "LOCALAPPDATA") ".nms-converter-cache")
  @run-cache)


  ;; => {:file #object[java.io.File 0x4496a00e "D:\\NMSUnpacked\\LANGUAGE\\NMS_LOC1_ENGLISH.EXML"],
  ;;     :digest "bf249b8fa2e067955b3564bd93858e02b9a094da8c8a6012ceb6cc0555b4d708"}

;; EXML file accessors

(defn language-files!
  "Take MBIN language files from location based on configuration
   and produces a list of EXML files. 
   Will recreate EXML files from scratch running full MBIN -> EXML
   transfromation step."
  []
  (map (comp #(:file %)
             mbin->exml
             file-and-digest)
       (list-lang-files)))

(defn substance-file!
  "Take MBIN substance file from location based on configuration
   and produces EXML file. 
   Will recreate EXML file from scratch running full MBIN -> EXML
   transfromation step."
  []
  ((comp #(:file %)
         mbin->exml
         file-and-digest)
   (substance-mbin-file)))

(defn product-file!
  "Take MBIN product file from location based on configuration
   and produces EXML file. 
   Will recreate EXML file from scratch running full MBIN -> EXML
   transfromation step."
  []
  ((comp #(:file %)
         mbin->exml
         file-and-digest)
   (product-mbin-file)))

(defn recipe-file!
  "Take MBIN product file from location based on configuration
   and produces EXML file. 
   Will recreate EXML file from scratch running full MBIN -> EXML
   transfromation step."
  []
  ((comp #(:file %)
         mbin->exml
         file-and-digest)
   (recipe-mbin-file)))

(init-config)

(comment
  (language-files!)
  (substance-file!)
  (product-file!)
  (recipe-file!)
  @run-cache)

