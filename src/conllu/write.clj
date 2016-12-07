(ns conllu.write
  "for parsing conllu files."
  (:require [clojure
             [spec :as s]
             [string :as str]]
            [clojure.java.io :as io]
            conllu))

(s/fdef str-avm
        :args (s/cat :c char? :m map?)
        :ret string?)

(defn str-avm
  "the inverse of [[conllu.parse/parse-avm]]."
  [c m]
  (->> (sort-by key m)
       (map (fn [[k v]]
              (let [k (if (keyword? k) (name k) k)
                    v (if (keyword? v) (name v) v)]
                (str k c v))))
       (str/join \|)))

(s/fdef str-word
        :args (s/cat :word :conllu/word)
        :ret string?)

(defn str-word
  "the inverse of [[conllu.parse/parse-word]]."
  [{:keys [:conllu/index :conllu/multi :conllu/empty
           :conllu/form :conllu/lemma :conllu/upos :conllu/xpos :conllu/morph
           :conllu/head :conllu/rel :conllu/deps :conllu/misc]}]
  (->> [(cond index index multi (str/join \- multi) empty (str/join \. empty))
        form lemma (when upos (name upos)) xpos (when morph (str-avm \= morph))
        head (when rel (name rel)) (when deps (str-avm \: deps)) (when misc (str-avm \= misc))]
       (map #(if % % \_))
       (str/join \tab)))

(s/fdef write-file :args (s/cat :sent+ (s/every :conllu/sent) :file some?))

(defn write-file
  "parses a conllu `file` which can be any acceptable input
  for `clojure.java.io/writer`."
  [sent+ file]
  (with-open [^java.io.BufferedWriter wtr (io/writer file)]
    (doseq [sent sent+]
      (doseq [word sent]
        (doto wtr
          (.write ^String (str-word word))
          .newLine))
      (.newLine wtr))))
