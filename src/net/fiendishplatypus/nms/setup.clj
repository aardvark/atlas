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
            [net.fiendishplatypus.nms.file :as file]
            [net.fiendishplatypus.nms.cache :as cache])
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


(defn- mbin->exml-priv
  ""
  [file-def]
  (if (cache/cached? file-def)
    (:exml (cache/entry (:file file-def)))
    ;; not cached so we need to run exml transformation
    (let [file (:file file-def)
          filename (file/name (.getName file))
          exml-def (file/file-and-digest (to-exml file))]

      (cache/update-cache filename {:mbin file-def :exml exml-def})
      exml-def)))

(def mbin->exml
  (memoize mbin->exml-priv))

;; EXML file accessors

(defn language-files!
  "Take MBIN language files from location based on configuration
   and produces a list of EXML files.
   Will recreate EXML files from scratch running full MBIN -> EXML
   transformation step."
  []
  (map (comp #(:file %)
             mbin->exml
             file/file-and-digest)
       (list-lang-files)))

(defn substance-file!
  "Take MBIN substance file from location based on configuration
   and produces EXML file.
   Will recreate EXML file from scratch running full MBIN -> EXML
   transformation step."
  []
  ((comp #(:file %)
         mbin->exml
         file/file-and-digest)
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
         file/file-and-digest)
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
         file/file-and-digest)
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
  (recipe-file!))

