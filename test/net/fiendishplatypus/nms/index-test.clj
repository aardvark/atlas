(ns net.fiendishplatypus.nms.index-test
  (:require [net.fiendishplatypus.nms.index :as t]
            [clojure.test :refer [deftest is testing]]))


(deftest id
  (is (= "SCAN_BROKEN" (t/id "      <Property name=\"Id\" value=\"SCAN_BROKEN\" />")))
  (is (= "" (t/id "      <Property name=\"Id\" value=\"\" />")))
  (is (nil? (t/id "      <Property name=\"\" value=\"\" />"))))
  
  
