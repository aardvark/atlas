(ns net.fiendishplatypus.nms.setup-test
  (:require [net.fiendishplatypus.nms.setup :as t]
            [clojure.test :refer [deftest is testing]]))


(deftest english-lang-file-matcher
  (is (true? (t/engilsh-lang-file? "NMS_LOC1_ENGLISH.MBIN")))
  (is (false? (t/engilsh-lang-file? "NMS_LOC1_GERMAN.MBIN"))))

(deftest env-value-from-file
  (testing "Env values from file population"
    (omniconf.core/populate-from-file (clojure.java.io/resource "setup-example.edn"))))

(comment
  ;; TODO: write a helper for accessing test resoruces
  (clojure.java.io/resource "resources/setup-example.edn")

  (omniconf.core/populate-from-file
   (clojure.java.io/resource "resources/setup-example.edn"))

  (omniconf.core/get :lang-mbin-dir)

  (into {}
        (filter (fn [[k _]] (not (nil? (clojure.string/index-of k "PP")))))
        (System/getenv)))
