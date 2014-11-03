(ns clojure-simfile.core
  (:require [clojure.string :as str]))

(def tag-regex
  #"#(.+):(.*);")

(def chart-meta-regex
  #"#NOTES:(?:\s+.*:)+")

(def notes-descriptor
  {:NotesType #{"dance-single" "dance-double" "dance-couple" "dance-solo" "pump-single" "pump-double" "pump-couple" "ez2-single" "ez2-double" "ez2-real" "para-single"}
   :Description :any
   :DifficultyClass #{"beginner" "easy" "medium" "hard" "challenge"}
   :DifficultyMeter :any
   :RadarValues :any})

(defn parse-sm [filename]
  (let [contents (slurp filename)
        tags (parse-tags contents)
        note-descriptors (parse-charts contents)]
    (assoc tags :NOTES note-descriptors)))

(defn- parse-tags [contents]
  (let [raw-tags (re-seq tag-regex contents)
        tag-seq (map rest raw-tags)]
    (reduce #(assoc %1 (keyword (first %2)) (second %2)) {} tag-seq)))

(defn- valid-meta-descriptor?
  [descriptor options chart-meta-map]
  (or (= options :any) (contains? options (descriptor chart-meta-map))))

(defn- valid-chart-meta-map? [chart-meta-map]
  (every? identity (for [[descriptor options] notes-descriptor]
                     (valid-meta-descriptor? descriptor options chart-meta-map))))

(defn- parse-raw-chart [raw]
  (let [meta (rest (map #(str/trim (str/lower-case %)) (str/split raw #":")))
        keywords [:NotesType :Description :DifficultyClass :DifficultyMeter :RadarValues]
        chart-meta-map (zipmap keywords meta)]
    (if (valid-chart-meta-map? chart-meta-map)
      chart-meta-map
      nil)))

(defn- parse-charts [contents]
  (let [raw-meta (re-seq chart-meta-regex contents)]
    (map parse-raw-chart raw-meta)))
