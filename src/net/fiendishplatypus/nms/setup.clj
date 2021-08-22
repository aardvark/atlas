(ns net.fiendishplatypus.nms.setup
  "Contain accessor functions for EXML files and for calling
   MBINCompiler process.
   Should be mostly used to rerun dictionaries creation when new
   version of NMS is released and recipes data can be changed.
   Invoking EXML files will run a full MBIN -> EXML transformation steps.

   Input (MBIN) output (EXML) file pairs are checked by hash to skip
   MBINCompiler calls when not needed."
  (:require [clojure.java.io]
            [clojure.java.shell]
            [clojure.string]
            [clojure.edn]
            [omniconf.core :as cfg]
            [taoensso.timbre :as timbre :refer [debug info error]]
            [net.fiendishplatypus.nms.file :as file])
  (:import (java.io FilenameFilter File PushbackReader IOException)
           (java.security MessageDigest DigestInputStream)))

;; Configuration

(defn init-config
  [args]

  (cfg/define
    {:mbin-compiler-dir {:description "Directory where MBINCompiler.exe can be found"
                         :type :string}
     :nms-unpacked-root {:description "Directory root for NMS unpacked files"
                         :type :string}

     :lang-mbin-dir {:description "Directory where MBIN LANGUAGE files are located"
                     :type :string}
     :substance-mbin-dir {:description "Directory where NMS_REALITY_GCSUBSTANCETABLE.MBIN can be found"
                          :type :string}
     :product-mbin-dir {:description "Directory where NMS_REALITY_GCPRODUCTTABLE.MBIN can be found"
                        :type :string}
     :recipe-mbin-dir {:description "Directory where NMS_REALITY_GCRECIPETABLE.MBIN be found"
                       :type :string}})

  (let [f (clojure.java.io/file (System/getenv "LOCALAPPDATA")
                                "nms-atlas-converter.config.edn")]
    (if (.exists f) (cfg/populate-from-file f)
        (do
          (cfg/populate-from-cmd args)
          (let [root (or (cfg/get :nms-unpacked-root) "C:\\Projects\\NMSUnpacked")]
            (cfg/populate-from-map
              ;;  TODO: Pass root from parameters "net.fiendishplatypus.nms.setup.unpacked.root"
             {:mbin-compiler-dir "C:\\Projects\\NoManSkyMods"

              :lang-mbin-dir (str root "\\LANGUAGE")

              :substance-mbin-dir (str root "\\METADATA\\REALITY\\TABLES")
              :substance-filename "NMS_REALITY_GCSUBSTANCETABLE.MBIN"

              :product-mbin-dir (str root "\\METADATA\\REALITY\\TABLES")
              :product-filename "NMS_REALITY_GCPRODUCTTABLE.MBIN"

              :recipe-mbin-dir (str root "\\METADATA\\REALITY\\TABLES")
              :recipe-filename "NMS_REALITY_GCRECIPETABLE.MBIN"}))))
    (cfg/verify)))

;; MBIN file lookup operation

(defn english-lang-file?
  [name]
  (not (nil? (re-matches #"NMS_\w+_ENGLISH.MBIN" name))))


(def english-lang-file
  (reify
    FilenameFilter
    (accept [this _ name]
      (english-lang-file? name))))


(defn list-lang-files
  ([path-to-lang-dir]
   (.listFiles (File. ^String path-to-lang-dir)
               ^FilenameFilter english-lang-file))
  ([]
   (list-lang-files (cfg/get :lang-mbin-dir))))

;; File checksum operations

(defn file-to-a-digest
  "Take a `file` and return sha256 digest of said file"
  [^File file]
  (let [md (MessageDigest/getInstance "SHA-256")
        digest-arr (with-open [s (clojure.java.io/make-input-stream file {})
                               dis (new DigestInputStream s md)]
                     (while (not= (.read dis) -1))
                     (.digest (.getMessageDigest dis)))]
    digest-arr))

(defn file-and-digest
  "Take a java.ioFile and return map with :file and :digest sha-256 string for a given file"
  [^File file]
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


(defn cache-entry
  [file]
   (get @run-cache (file/name file)))


(defn- exml-cached?
  [cache-entry]
  (let [cached-exml-digest (get-in cache-entry [:exml :digest] "")
        current-exml-digest (:digest (file-and-digest (get-in cache-entry [:exml :file])))]
    (= cached-exml-digest current-exml-digest)))


(defn cached?
  "get a map of mbin :file and :digest.
   Return true if both mbin and exml entries exist in a converter cache and
   their digests are matching"
  [new-file]
  (let [cache-entry (cache-entry (:file new-file))
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
  (cached? (file-and-digest (first (list-lang-files)))))

;; MBIN decompiler invocation
(defn to-exml
  "given a mbin file `file` run MBINCompiler on it to get a EXML file
   and return such EXML file back"
  [file]
  (info "Running MBIN -> EXML conversion for " (.toString (.toPath file)))
  (let [path-str (.toString (.toPath file))
        filename (.getName file)
        parent-dir (.getParent file)
        exml-file-path (str parent-dir
                            "\\"
                            (file/name filename)
                            ".EXML")
        mbin-compiler-dir (cfg/get :mbin-compiler-dir)
        _  (clojure.java.shell/with-sh-dir mbin-compiler-dir
             (clojure.java.shell/sh "cmd.exe" "/C" "call" "MBINCompiler.exe" "-q" path-str))
        exml (File. exml-file-path)]
    exml))


(defn- mbin->exml
  ""
  [file-def]
  (if (cached? file-def)
    (:exml (cache-entry (:file file-def)))
    ;; not cached so we need to run exml transformation
    (let [file (:file file-def)
          filename (file/name (.getName file))
          exml-def (file-and-digest (to-exml file))]

      (update-cache run-cache filename {:mbin file-def :exml exml-def})
      exml-def)))

(comment
  (mbin->exml (file-and-digest (second (list-lang-files))))
  (update (file-and-digest (second (list-lang-files)))
          :file #(.getPath %))

  (persist-cache @run-cache)
  (slurp cache-file)
  @run-cache)


  ;; => {:file #object[java.io.File 0x4496a00e "D:\\NMSUnpacked\\LANGUAGE\\NMS_LOC1_ENGLISH.EXML"],
  ;;     :digest "bf249b8fa2e067955b3564bd93858e02b9a094da8c8a6012ceb6cc0555b4d708"}

;; EXML file accessors

(defn language-files!
  "Take MBIN language files from location based on configuration
   and produces a list of EXML files.
   Will recreate EXML files from scratch running full MBIN -> EXML
   transformation step."
  []
  (map (comp #(:file %)
             mbin->exml
             file-and-digest)
       (list-lang-files)))

(defn substance-file!
  "Take MBIN substance file from location based on configuration
   and produces EXML file.
   Will recreate EXML file from scratch running full MBIN -> EXML
   transformation step."
  []
  ((comp #(:file %)
         mbin->exml
         file-and-digest)
   (clojure.java.io/file (cfg/get :substance-mbin-dir)
                         (cfg/get :substance-filename))))

(defn product-file!
  "Take MBIN product file from location based on configuration
   and produces EXML file.
   Will recreate EXML file from scratch running full MBIN -> EXML
   transformation step."
  []
  ((comp #(:file %)
         mbin->exml
         file-and-digest)
   (clojure.java.io/file (cfg/get :product-mbin-dir)
                         (cfg/get :product-filename))))

(defn recipe-file!
  "Take MBIN product file from location based on configuration
   and produces EXML file.
   Will recreate EXML file from scratch running full MBIN -> EXML
   transformation step."
  []
  ((comp #(:file %)
         mbin->exml
         file-and-digest)
   (clojure.java.io/file (cfg/get :recipe-mbin-dir)
                         (cfg/get :recipe-filename))))

;; (init-config)

(defn create-files
  []
  (language-files!)
  (substance-file!)
  (product-file!)
  (recipe-file!)
  nil)

(comment
  (language-files!)
  (substance-file!)
  (product-file!)
  (recipe-file!)
  @run-cache)

