(ns clojure-simfile.core)

(defn parse-sm [filename]
  (let [contents (slurp filename)]
    contents))


(def tag-regex
  #"#(.+):(.*);")

(defn parse-tags [contents]
  (let [raw-tags (re-seq tag-regex contents)
        tag-seq (map rest raw-tags)]
    (reduce #(assoc %1 (keyword (first %2)) (second %2)) {} tag-seq)))
