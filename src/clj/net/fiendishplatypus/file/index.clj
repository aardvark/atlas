(ns net.fiendishplatypus.file.index
  (:require [clojure.java.io]))

;; indexer
;; 
(defn- matcher [re s]
  (let [match (re-find re s)]
    (if (coll? match) (second match) nil)))

(defn- add
  "Helper for merging reduce product to the vector"
  [xs x]
  (cond
    (map? x) (conj (vec xs) x)
    (coll? x) (vec (concat xs x))
    :else (conj (vec xs) x)))

(defn index 
  "Create file index for passed `f` file. 
   Uses string predicates `start?` and `end?` for marking start and end of records."
  [start? end? f]
  (reduce
   (fn [xs b]
     (let [a (last xs)]
       (add (butlast xs)
            (cond
            ;; found start marker. need to merge id and setup start
              (:start? b)
              {:start (:size a)
               :size (+ (:size a)
                        (:size b))
               :start? true
               :end? false
               :id (:id b)}

            ;; found recipe id after to update
              (and (not (nil? (:id b))) (= (:id a) "GcRefinerRecipe.xml"))
              (assoc a :id (:id b))

              (:end? b)
              (let [m (assoc a :end? true)
                    m (update m :size + (:size b))
                    m (assoc m :end (+ (:size a) (:size b)))]
                [m {:size (:size m)}])
              
              :else 
              (update a :size + (:size b))))))
   [{:size 0}]
   (with-open [rdr (clojure.java.io/reader f)]
     (let [id-matcher (re-pattern #"value=\"(\S+)\"")
           xf (map (fn [s]
                     {:size (+ 2 (count (.getBytes s)))
                      :start? (start? s)
                      :end? (end? s)
                      :id (matcher id-matcher s)}))]
       (into [] xf (line-seq rdr))))))

(comment 
  (index (fn [s] (.contains s "GcRefinerRecipe.xml"))
         (fn [s] (= s "    </Property>"))
         "resources/nms/recipes-example.xml"))
  ;; => [{:start 157, :size 1194, :start? true, :end? true, :id "REFINERECIPE_14", :end 1194}
  ;;     {:start 1194, :size 2115, :start? true, :end? true, :id "REFINERECIPE_15", :end 2115}
  ;;     {:size 2139}]

