(ns net.fiendishplatypus.nms.setup
  (:require [clojure.java.io]
            [clojure.string]))

;; File lookup operation

(defn engilsh-lang-file?
  [name]
  (not (nil? (re-matches #"NMS_\w+_ENGLISH.MBIN" name))))


(def engilsh-lang-file
  (reify
    java.io.FilenameFilter
    (accept [this _ name]
      (engilsh-lang-file? name))))

(comment 
  (cfg/define
    {:lang-mbin-dir {:description "Directory where MBIN LANGUAGE files are located"
                     :type :string}})
  
  (cfg/get :lang-mbin-dir))

(defn list-lang-files
  ([path-to-lang-dir]
   (.listFiles (java.io.File. path-to-lang-dir)
               engilsh-lang-file))
  ([]
   (list-lang-files "D:\\NMSUnpacked\\LANGUAGE")))

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
  (let [digest-arr (file-to-a-digest file)]
    {:filename (.getName file) 
     :sha256sum (clojure.string/join (map #(format "%02x" %)
                                          (seq digest-arr)))}))

(comment
  (map file-and-digest (list-lang-files)))

;; copy file?

;; MBIN decompiler invocation
(defn mbin->exml 
  [mbin]
  (let [mbin-compiler-dir "C:\\Projects\\NoManSkyMods"
        filename "NMS_LOC1_ENGLISH.MBIN"]
    (clojure.java.shell/with-sh-dir mbin-compiler-dir
      (clojure.java.shell/sh "cmd.exe" "/C" "call" "MBINCompiler.exe" "-q" filename))
    (.length (java.io.File. "C:\\Projects\\NoManSkyMods\\NMS_LOC1_ENGLISH.EXML"))))

(comment
  (mbin->exml "whatever"))

(comment 
  "true"
  (engilsh-lang-file? "NMS_LOC1_ENGLISH.MBIN")
  "false"
  (engilsh-lang-file? "NMS_LOC1_GERMAN.MBIN"))
  

