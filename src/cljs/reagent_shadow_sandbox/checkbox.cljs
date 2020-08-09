(ns reagent-shadow-sandbox.checkbox)

(defn value
  "Retrive checkbox element using passed elementId and 
   return it checked value (true/false)"
  [elementId]
  (let [el (.getElementById js/document elementId)
        v (aget el "checked")]
    v))
