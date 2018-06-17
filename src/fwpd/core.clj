(ns fwpd.core)

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map
   #(clojure.string/split % #",")
   (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [min-glitter records]
  (filter
   #(>= (:glitter-index %) min-glitter)
   records))

(defn glitter-names
  [records]
  (map :name records))

(glitter-filter 5 (mapify (parse (slurp filename))))
(glitter-names (glitter-filter 5 (mapify (parse (slurp filename)))))

(def not-blank? (complement clojure.string/blank?))

(def validators {:name [not-blank?]
                 :glitter-index [number?]})

(defn validate
  [record validators]
  (let [validated (map (fn
                         [[validator-key  validator-rules]]
                         (map (fn [rule] (rule (validator-key record))) validator-rules))
                       validators)]
    (every? #(every? true? %) validated)))

(validate {:name "some name"
           :glitter-index 123}
          validators)

(defn append
  "Append name and glitter-index to CSV"
  [name glitter-index]
  (let [is-valid (validate {:name name
                            :glitter-index glitter-index}
                           validators)]
    (if is-valid
      (spit filename (str name "," glitter-index "\n") :append true)
      (println "Input is not valid."))))

(append "new name" "2")
