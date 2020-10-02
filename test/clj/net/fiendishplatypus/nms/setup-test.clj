(ns net.fiendishplatypus.nms.setup-test
  (:require [net.fiendishplatypus.nms.setup :as t]
            [clojure.test :refer [deftest is]]))
  

(deftest english-lang-file-matcher
  (is (true? (t/engilsh-lang-file? "NMS_LOC1_ENGLISH.MBIN")))
  (is (false? (t/engilsh-lang-file? "NMS_LOC1_GERMAN.MBIN"))))
            
