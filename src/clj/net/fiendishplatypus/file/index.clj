(ns net.fiendishplatypus.file.index
  (:require [clojure.java.io]
            [clojure.string]
            [net.fiendishplatypus.nms.language :as language])
  (:import [java.io RandomAccessFile]
           [java.nio ByteBuffer]))


(defn- matcher [re s]
  (let [match (re-find re s)]
    (if (coll? match) (second match) nil)))

(defn reducer
  [id? acc b]
  (let [a (first acc)
        x (cond
          ;; found start marker. need to merge id and setup start
            (:start? b)
            {:start  (:size a)
             :size   (+ (:size a)
                        (:size b))
             :start? true
             :end?   false
             :id     (:id b)}

          ;; found recipe id after to update
            (and (not (nil? (:id b))) (id? (:id a)))
            (assoc a :id (:id b) :size (+ (:size a) (:size b)))

           ;; found end marker, need to generate additional record for next entry
            (:end? b)
            (let [m (assoc a :end? true)
                  m (update m :size + (:size b))
                  m (assoc m :end (+ (:size a) (:size b)))]
              [{:size (:size m)} m])

            :else
            (update a :size + (:size b)))]
    (if (map? x)
      (cons x (rest acc))
      (concat x (rest acc)))))


(defn index
  "Create index for file `f` based on records. Expect files encoded in UTF-8"
  [start-mark end? f]
  (with-open [rdr (clojure.java.io/reader f)]
    (let [id-matcher (re-pattern #"name=\"(?:ID|Id)\" value=\"(\S+)\"")
          start? (fn [s] (.contains s start-mark))
          id (fn [s] (or (if (start? s) start-mark nil) (matcher id-matcher s)))
          xf (map (fn [s]
                    {:size (+ 2 (count (.getBytes s "UTF-8")))
                     :start? (start? s)
                     :end? (end? s)
                     :id (id s)}))
          reducer (completing (partial reducer start?))
          records (sort-by (fn [a] (:start a)) <
                           (filter #(not (nil? (:id %)))
                                   (transduce xf reducer [{:size 0}] (line-seq rdr))))]
      records)))


(defn language-indexer
  [file]
  ((partial index
            "TkLocalisationEntry.xml"
            (fn [s] (= s "    </Property>"))) file))


(defn load-record
  "Given file `f` and index `record` returns full entry"
  [f record]
  (let [buffer (ByteBuffer/allocate (- (:end record) (:start record)))]
    (with-open [file (RandomAccessFile. f "r")]
      (.seek file (:start record))
      (.read (.getChannel file) buffer)
      (.flip buffer)
      (String. (.array buffer)))))


(defn search-for 
  "Collect strings for enrichment"
  [entity-map keys-to-collect]

  (let [to-enrich entity-map
        f (fn [entry] (-> entry 
                          (second) 
                          (select-keys keys-to-collect)
                          (vals)))
        xf (mapcat f)]
    (into #{} xf to-enrich)))


(defn translate
  [dict input]
  ;; (let [input {"WAR_STANDING_UP" {:name "UI_STANDING_WAR", :id "WAR_STANDING_UP", :namelower "UI_STANDING_WAR"}
  ;;              "ASTEROID3" {:name "UI_ASTEROID3_NAME", :id "ASTEROID3", :namelower "UI_ASTEROID3_NAME_L"}}
  ;;       dict {"UI_STANDING_WAR" "VY'KEEN"
  ;;             "UI_ASTEROID3_NAME" "Translated"}]
  (map (fn [e] 
         {(first e) (assoc (second e)
                           :name (get dict (:name (second e)))
                           :namelower (get dict (:namelower (second e))))})
       input))


;; dictionary prototype
(comment
  (time
   (let [input        {"WAR_STANDING_UP" {:name      "UI_STANDING_WAR"
                                          :id        "WAR_STANDING_UP"
                                          :namelower "UI_STANDING_WAR"}
                       "ASTEROID3"       {:name      "UI_ASTEROID3_NAME"
                                          :id        "ASTEROID3"
                                          :namelower "UI_ASTEROID3_NAME_L"}}
         search-for   (search-for input [:name :namelower])
         file         "D:\\NMSProjects\\Notepad\\LANGUAGE\\NMS_LOC4_ENGLISH.EXML"
         start-mark   (:start-mark language/index-meta)
         start?     (fn [s] (clojure.string/includes? s start-mark))
         end?         (:end? language/index-meta)
         id-matcher (re-pattern #"name=\"(?:ID|Id)\" value=\"(\S+)\"")
         id         (fn [s] (or (if (start? s) start-mark nil) (matcher id-matcher s)))
         xf         (map (fn [s]
                             {:size   (+ 2 (count (.getBytes s "UTF-8")))
                              :start? (start? s)
                              :end?   (end? s)
                              :id     (id s)}))
         red        (completing (partial reducer start?))]
     (with-open [rdr (clojure.java.io/reader file)]
       (let [records    (sort-by (fn [a] (:start a)) <
                                 (filter #(not (nil? (:id %)))
                                         (transduce xf red '({:size 0}) (line-seq rdr))))
             lang       (map
                         (fn [record] (language/language
                                       (load-record file record)
                                       (:id record)))
                         records)
             dict       (into {} (filter (fn [x] (search-for (first (first  x))))) 
                              lang)]
          dict)))))


(comment
  (index "GcRefinerRecipe.xml"
         (fn [s] (= s "    </Property>"))
         "resources/nms/recipes-example.xml"))
  ;; => {"REFINERECIPE_14" {:start 157, :size 1248, :start? true, :end? true, :id "REFINERECIPE_14", :end 1248},
  ;;     "REFINERECIPE_15" {:start 1248, :size 2223, :start? true, :end? true, :id "REFINERECIPE_15", :end 2223}}
