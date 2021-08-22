(ns net.fiendishplatypus.nms.file
  (:require [clojure.string]
            [clojure.java.io :as io])
  (:import (java.io File)
           (java.security MessageDigest DigestInputStream)))

(defn name
  [file]
  (let [[name _ext] (clojure.string/split file #"\.")]
    name))


;; File checksum operations

(defn- file-to-a-digest
  "Take a `file` and return sha256 digest of said file"
  [^File file]
  (let [md (MessageDigest/getInstance "SHA-256")
        digest-arr (with-open [s (io/make-input-stream file {})
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

