(ns reagent-shadow-sandbox.prod
  (:require [reagent-shadow-sandbox.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
