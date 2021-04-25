(ns net.fiendishplatypus.nms.test
  (:require [net.fiendishplatypus.nms.language :as language]
            [net.fiendishplatypus.file.index :as index]
            [clojure.test :refer [testing is deftest run-tests]]
            [clojure.test.check :as ch]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(comment 
  (language/language 
   (slurp "test/resources/language-with-quote-in-id.xml")
   "EXP_LIFEFORM'S"))

(deftest language-lookup
  (testing "language/language lookup"
    (is (= {"EXP_LIFEFORM'S" "lifefor's"}
           (language/language
            (slurp "test/resources/language-with-quote-in-id.xml")
            "EXP_LIFEFORM'S"))
        "Lookup of EXP_LIFEFORM'S id should return \"lifeform's\" value")))


