(ns net.fiendishplatypus.nms.setup-test
  (:require [net.fiendishplatypus.nms.setup :as t]
            [omniconf.core :as cfg]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest are is testing]]))


(deftest english-lang-file-matcher
  (is (true? (t/english-lang-file? "NMS_LOC1_ENGLISH.MBIN")))
  (is (false? (t/english-lang-file? "NMS_LOC1_GERMAN.MBIN"))))


(def setup-example
  {:lang-mbin-dir "D:\\NMSUnpacked\\LANGUAGE"
   :mbin-compiler-dir "C:\\Projects\\NoManSkyMods"
   :substance-filename "NMS_REALITY_GCSUBSTANCETABLE.MBIN"
   :substance-mbin-dir "D:\\NMSUnpacked\\METADATA\\REALITY\\TABLES"
   :product-mbin-dir "D:\\NMSUnpacked\\METADATA\\REALITY\\TABLES"
   :product-filename "NMS_REALITY_GCPRODUCTTABLE.MBIN"})


(defn- check-key
  [k]
  (is (= (k setup-example) (cfg/get k))
      (str "value of " k " is not correct")))

(deftest env-value-from-file
  (testing "Env values from file population"
    (cfg/populate-from-file
     (io/resource "resources/setup-example.edn")))
  (testing (str "All keys from resources/setup-example.edn file should be loaded")
    (check-key :lang-mbin-dir)
    (check-key :mbin-compiler-dir)))


(comment
  ;; TODO: write a helper for accessing test resources
  (io/resource "resources/setup-example.edn")

  (cfg/populate-from-file
   (io/resource "resources/setup-example.edn"))

  (cfg/get :lang-mbin-dir)

  (into {}
        (filter (fn [[k _]] (not (nil? (clojure.string/index-of k "PP")))))
        (System/getenv)))

(comment (cfg/populate-from-map
          {:lang-mbin-dir "C:\\Projects\\NMSUnpacked\\LANGUAGE"
           :mbin-compiler-dir "C:\\Projects\\NoManSkyMods"
           :substance-filename "NMS_REALITY_GCSUBSTANCETABLE.MBIN"
           :substance-mbin-dir "C:\\Projects\\NMSUnpacked\\METADATA\\REALITY\\TABLES"
           :product-mbin-dir "C:\\Projects\\NMSUnpacked\\METADATA\\REALITY\\TABLES"
           :product-filename "NMS_REALITY_GCPRODUCTTABLE.MBIN"})

         (cfg/populate-from-cmd ["--nms-unpacked-root" "string"])
         (cfg/get :nms-unpacked-root))

