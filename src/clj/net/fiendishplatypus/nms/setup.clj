(ns net.fiendishplatypus.nms.setup
  "Contain accessor functions for EXML files and for calling
   MBINCompiler process.
   Should be mostly used to rerun dictonaries creation when new
   version of NMS is released and recipes data can be changed.
   Invoking EXML files will run a full MBIN -> EXML transformation steps."
  (:require [clojure.java.io]
            [clojure.java.shell]
            [clojure.string]
            [omniconf.core :as cfg]))

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


(cfg/populate-from-map 
 {:lang-mbin-dir "D:\\NMSUnpacked\\LANGUAGE"
  :mbin-compiler-dir "C:\\Projects\\NoManSkyMods"
  :substance-filename "NMS_REALITY_GCSUBSTANCETABLE.MBIN"
  :substance-mbin-dir "D:\\NMSUnpacked\\METADATA\\REALITY\\TABLES"
  :product-mbin-dir "D:\\NMSUnpacked\\METADATA\\REALITY\\TABLES"
  :product-filename "NMS_REALITY_GCPRODUCTTABLE.MBIN"})
                        
;; MBIN file lookup operation
 
(defn product-mbin-file 
  []
  (clojure.java.io/file (cfg/get :product-mbin-dir) (cfg/get :product-filename)))
 
(defn substance-mbin-file 
  []
  (clojure.java.io/file (cfg/get :substance-mbin-dir) (cfg/get :substance-filename)))


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

;; `file-and-digest` function can be used to do a smart conversion.
;; Meaning we can only run conversion to XML if new file is different
;; from previous one.
;; This raise a separate question of storing 'conversion-run'
;; results somewhere and loading them for checking on the next run
;; saving map of :filename and :sha256sum should be enough
;; :mbin-file and :exml-file parameters can be always added as they
;; appear in the processing step

(defn file-and-digest
  [file]
  (let [digest-arr (file-to-a-digest file)]
    {:filename (.getName file) 
     :mbin-file file
     :sha256sum (clojure.string/join (map #(format "%02x" %)
                                          (seq digest-arr)))}))

(comment
  (map file-and-digest (list-lang-files)))


(defn file-to-name-ext-pair 
  [filename]
  (let [[name ext] (clojure.string/split filename #"\.")]
    {:name name :ext ext}))

;; MBIN decompiler invocation

(defn mbin->exml 
  "Convert MBIN `file` to the EXML, and return it. 
   Converted file will be located in same directory as a passed MBIN `file`. "
  [file]
  (let [mbin-compiler-dir (cfg/get :mbin-compiler-dir)
        path-str (.toString (.toPath file))
        filename (.getName file)
        parent-dir (.getParent file)
        exml-file-path (str parent-dir
                                 "\\"
                                 (:name (file-to-name-ext-pair filename))
                                 ".EXML")]
    (clojure.java.shell/with-sh-dir mbin-compiler-dir
      (clojure.java.shell/sh "cmd.exe" "/C" "call" "MBINCompiler.exe" "-q" path-str))
    (java.io.File. exml-file-path)))

(comment
  (mbin->exml (first (list-lang-files))))

;; EXML file accessors

(defn language-files!
  "Take MBIN language files from location based on configuration
   and produces a list of EXML files. 
   Will recreate EXML files from scratch running full MBIN -> EXML
   transfromation step."
  []
  (map mbin->exml (list-lang-files)))

(defn substance-file!
  "Take MBIN substance file from location based on configuration
   and produces EXML file. 
   Will recreate EXML file from scratch running full MBIN -> EXML
   transfromation step."
  []
  (mbin->exml (substance-mbin-file)))

(defn product-file!
  "Take MBIN product file from location based on configuration
   and produces EXML file. 
   Will recreate EXML file from scratch running full MBIN -> EXML
   transfromation step."
  []
  (mbin->exml (product-mbin-file)))
