(ns net.fiendishplatypus.nms.language-test
  (:require [clojure.test :refer [deftest is testing]]
            [net.fiendishplatypus.nms.language :as language]))


(comment
  (language/language
   (slurp "test/resources/language-with-quote-in-id.xml")
   "EXP_LIFEFORM'S"))

(deftest language-lookup
  (testing "language/language lookup"
    (is (= {"EXP_LIFEFORM'S" "lifeform's"}
           (language/language
            (slurp "test/resources/language-with-quote-in-id.xml")
            "EXP_LIFEFORM'S"))
        "Lookup of EXP_LIFEFORM'S id should return \"lifeform's\" value")))