(ns net.fiendishplatypus.file.index
  (:require [clojure.java.io])
  (:import [java.io RandomAccessFile]
           [java.nio ByteBuffer]))

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

(defn- to-records
  [start? end? f]
  (with-open [rdr (clojure.java.io/reader f)]
    (let [id-matcher (re-pattern #"value=\"(\S+)\"")
          xf (map (fn [s]
                    {:size (+ 2 (count (.getBytes s)))
                     :start? (start? s)
                     :end? (end? s)
                     :id (matcher id-matcher s)}))]
      (into [] xf (line-seq rdr)))))

(comment
  (to-records (fn [s] (.contains s "GcRefinerRecipe.xml"))
              (fn [s] (= s "    </Property>"))
              "resources/nms/recipes-example.xml"))
  ;; => [{:size 40, :start? false, :end? false, :id nil}
  ;;     {:size 57, :start? false, :end? false, :id nil}
  ;;     {:size 33, :start? false, :end? false, :id nil}
  ;;     {:size 27, :start? false, :end? false, :id nil}
  ;;     {:size 44, :start? true, :end? false, :id "GcRefinerRecipe.xml"}
  ;;     {:size 54, :start? false, :end? false, :id "REFINERECIPE_14"}
  ;;     {:size 102, :start? false, :end? false, :id "RECIPE_OXYGEN"}
  ;;     {:size 49, :start? false, :end? false, :id "20"}
  ;;     {:size 49, :start? false, :end? false, :id "False"}
  ;;     {:size 67, :start? false, :end? false, :id "GcRefinerRecipeElement.xml"}
  ;;     {:size 121, :start? false, :end? false, :id "FUEL1"}
  ;;     {:size 60, :start? false, :end? false, :id "GcInventoryType.xml"}
  ;;     {:size 63, :start? false, :end? false, :id "Substance"}
  ;;     {:size 21, :start? false, :end? false, :id nil}
  ;;     {:size 46, :start? false, :end? false, :id "1"}
  ;;     {:size 19, :start? false, :end? false, :id nil}
  ;;     {:size 37, :start? false, :end? false, :id nil}
  ;;     {:size 55, :start? false, :end? false, :id "GcRefinerRecipeElement.xml"}
  ;;     {:size 49, :start? false, :end? false, :id "OXYGEN"}
  ;;     {:size 62, :start? false, :end? false, :id "GcInventoryType.xml"}
  ;;     {:size 65, :start? false, :end? false, :id "Substance"}
  ;;     {:size 23, :start? false, :end? false, :id nil}
  ;;     {:size 48, :start? false, :end? false, :id "1"}
  ;;     {:size 21, :start? false, :end? false, :id nil}
  ;;     {:size 19, :start? false, :end? false, :id nil}
  ;;     {:size 17, :start? false, :end? true, :id nil}
  ;;     {:size 44, :start? true, :end? false, :id "GcRefinerRecipe.xml"}
  ;;     {:size 54, :start? false, :end? false, :id "REFINERECIPE_15"}
  ;;     {:size 58, :start? false, :end? false, :id "RECIPE_LAUNCHFUEL"}
  ;;     {:size 50, :start? false, :end? false, :id "120"}
  ;;     {:size 49, :start? false, :end? false, :id "False"}
  ;;     {:size 67, :start? false, :end? false, :id "GcRefinerRecipeElement.xml"}
  ;;     {:size 46, :start? false, :end? false, :id "JELLY"}
  ;;     {:size 60, :start? false, :end? false, :id "GcInventoryType.xml"}
  ;;     {:size 61, :start? false, :end? false, :id "Product"}
  ;;     {:size 21, :start? false, :end? false, :id nil}
  ;;     {:size 46, :start? false, :end? false, :id "1"}
  ;;     {:size 19, :start? false, :end? false, :id nil}
  ;;     {:size 37, :start? false, :end? false, :id nil}
  ;;     {:size 55, :start? false, :end? false, :id "GcRefinerRecipeElement.xml"}
  ;;     {:size 52, :start? false, :end? false, :id "LAUNCHSUB"}
  ;;     {:size 62, :start? false, :end? false, :id "GcInventoryType.xml"}
  ;;     {:size 65, :start? false, :end? false, :id "Substance"}
  ;;     {:size 23, :start? false, :end? false, :id nil}
  ;;     {:size 49, :start? false, :end? false, :id "30"}
  ;;     {:size 21, :start? false, :end? false, :id nil}
  ;;     {:size 19, :start? false, :end? false, :id nil}
  ;;     {:size 17, :start? false, :end? true, :id nil}
  ;;     {:size 15, :start? false, :end? false, :id nil}
  ;;     {:size 9, :start? false, :end? false, :id nil}]


(defn records-reducer 
  [start-id]
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
             (and (not (nil? (:id b))) (= (:id a) start-id))
             (assoc a :id (:id b) :size (+ (:size a) (:size b)))

              ;; found end marker, need to generate additional record for next entry
             (:end? b)
             (let [m (assoc a :end? true)
                   m (update m :size + (:size b))
                   m (assoc m :end (+ (:size a) (:size b)))]
               [m {:size (:size m)}])

             :else
             (update a :size + (:size b)))))))


(defn index
  "Create file index for passed `f` file. 
   Uses string predicates `start?` and `end?` for marking start and end of records."
  [start? end? f]
  (reduce
   (records-reducer "GcRefinerRecipe.xml")
   [{:size 0}]
   (to-records start? end? f)))

(comment
  (index (fn [s] (.contains s "GcRefinerRecipe.xml"))
         (fn [s] (= s "    </Property>"))
         "resources/nms/recipes-example.xml"))
  ;; => [{:start 157, :size 1248, :start? true, :end? true, :id "REFINERECIPE_14", :end 1248}
  ;;     {:start 1248, :size 2223, :start? true, :end? true, :id "REFINERECIPE_15", :end 2223}
  ;;     {:size 2247}]


(defn index-transducer
  [start? end? f]
  (with-open [rdr (clojure.java.io/reader f)]
    (let [id-matcher (re-pattern #"value=\"(\S+)\"")
          xf (map (fn [s]
                    {:size (+ 2 (count (.getBytes s)))
                     :start? (start? s)
                     :end? (end? s)
                     :id (matcher id-matcher s)}))
          reducer (completing (records-reducer "GcRefinerRecipe.xml"))]
      (filter #(not (nil? (:id %)))
              (transduce xf reducer [{:size 0}] (line-seq rdr))))))

(comment
  (index-transducer (fn [s] (.contains s "GcRefinerRecipe.xml"))
                    (fn [s] (= s "    </Property>"))
                    "resources/nms/recipes-example.xml"))
  ;; => ({:start 157, :size 1248, :start? true, :end? true, :id "REFINERECIPE_14", :end 1248}
  ;;     {:start 1248, :size 2223, :start? true, :end? true, :id "REFINERECIPE_15", :end 2223})


(defn load-record
  "Given file `f` and index `record` returns full entry"
  [f record]
  (let [buffer (ByteBuffer/allocate (- (:end record) (:start record)))]
    (with-open [file (RandomAccessFile. f "r")]
      (.seek file (:start record))
      (.read (.getChannel file) buffer)
      (.flip buffer)
      (String. (.array buffer)))))
