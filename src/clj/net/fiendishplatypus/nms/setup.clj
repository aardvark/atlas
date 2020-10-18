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

(def run-cache
  (atom {}))

(defn file-to-name-ext-pair
  [filename]
  (let [[name ext] (clojure.string/split filename #"\.")]
    {:name name :ext ext}))


(defn cache-entry
  [file]
  (let [name (:name (file-to-name-ext-pair (.getName file)))]
    (get run-cache name)))

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
    (and mbin-match
         (exml-cached? cache-entry))))

(comment
  (mbin-cached?  (file-and-digest (first (list-lang-files)))))

;; MBIN decompiler invocation
(defn exml
  [file]
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

(comment 
  (language-files!)
  (substance-file!)
  (product-file!)
  @run-cache)

