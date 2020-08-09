(ns reagent-shadow-sandbox.element)

(defn value
  "Retrieve js element by using id `elId` and return 
   value of it \"value\" field"
  [elId]
  (aget (.getElementById js/document elId) "value"))
