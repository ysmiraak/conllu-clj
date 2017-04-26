(ns conllu.parse
  (:refer-clojure :exclude [vector hash-map])
  (:require [clj-tuple :refer [hash-map vector]]
            [clojure
             [spec :as s]
             [string :as str]]
            [clojure.java.io :as io]
            conllu))

(s/fdef specified?
        :args (s/cat :s string?)
        :ret boolean?)

(defn specified?
  "an underscore means unspecified."
  [s]
  (not= "_" s))

(s/fdef parse-pos-int
        :args (s/cat :s string?)
        :ret pos-int?)

(defn parse-pos-int
  "refuses to parse non-pos-int."
  [s]
  (let [i (Long/parseLong s)]
    (if (pos-int? i) i
        (throw (ex-info "int not positive." {:int i})))))

(s/fdef parse-nat-int
        :args (s/cat :s string?)
        :ret nat-int?)

(defn parse-nat-int
  "refuses to parse non-nat-int."
  [s]
  (let [i (Long/parseLong s)]
    (if (nat-int? i) i
        (throw (ex-info "int not natural." {:int i})))))

(s/fdef parse-avm
        :args (s/cat :s string? :c char? :kf ifn? :vf ifn?)
        :ret map?)

(defn parse-avm
  "parses an attribute-value matrix `s` into a map. the attribute-value pairs are
  linked by `c`, and separated by `|`. the keys are transformed by `kf` and the
  values by `vf`.
  ```
  (parse-avm \"a=1|b=2|c=3\" \\= keyword parse-nat-int)
  #_=> {:a 1, :b 2, :c 3}
  ```"
  [s c kf vf]
  (->> (str s \|)
       (re-seq
        (case c
          \= #"(.+?)=(.+?)\|"
          \: #"(.+?):(.+?)\|"
          (re-pattern (str "(.+?)" c "(.+?)\\|"))))
       (map (fn [[_ k v]] (vector (kf k) (vf v))))
       (reduce conj! (transient (hash-map)))
       persistent!))

(defn- parse-init
  ([] #(hash-map :conllu/index (parse-pos-int %)))
  ([t] #(hash-map t (vector (parse-nat-int (nth % 1)) (parse-pos-int (nth % 2))))))

(s/fdef parse-word
        :args (s/cat :line string?)
        :ret :conllu/word)

;; todo more check for well-formedness
;; 1. indices within range
;; 2. ordered

(defn parse-word
  "parses a line of conllu, which must not be empty or comment. turn
  on `clojure.spec/check-asserts` to ensure well-formedness, for
  which `clojure.spec/*compile-asserts*` needs to be `true`."
  [line]
  (let [[id form lemma upos xpos morph head rel deps misc] (str/split line #"\t")
        res (condp re-matches id
              #"\d+" :>> (parse-init)
              #"(\d+)-(\d+)" :>> (parse-init :conllu/multi)
              #"(\d+).(\d+)" :>> (parse-init :conllu/empty))]
    (cond-> (transient res)
      (or (:conllu/index res) (:conllu/multi res) (specified? form))
      (assoc! :conllu/form form)
      (specified? lemma)
      (assoc! :conllu/lemma lemma)
      (specified? upos)
      (assoc! :conllu/upos (s/assert :conllu/upos (keyword upos)))
      (specified? xpos)
      (assoc! :conllu/xpos xpos)
      (specified? morph)
      (assoc! :conllu/morph (parse-avm morph \= keyword identity))
      (or (:conllu/index res) (specified? head))
      (assoc! :conllu/head (parse-nat-int head))
      (specified? rel)
      (assoc! :conllu/rel (keyword rel))
      (or (:conllu/empty res) (specified? deps))
      (assoc! :conllu/deps (parse-avm deps \:
                                      identity ; fix: here could be :conllu/index or :conllu/empty, use proper type for distinction?
                                      keyword))
      (specified? misc)
      (assoc! :conllu/misc (parse-avm misc \= keyword identity))
      :finally persistent!)))

(def conll-xform
  "for transforming a seq of lines in a conll file into a seq of sentences which
  are seqs of lines, ignoring comments."
  (comp (map str/trim)
        (remove #(str/starts-with? % "#"))
        (partition-by str/blank?)
        (remove #(= [""] %))))

(s/fdef parse-file
        :args (s/cat :file any?)
        :ret (s/every :conllu/sent))

(defn parse-file
  "parses a conllu `file` which can be any acceptable input
  for `clojure.java.io/reader`."
  [file]
  (with-open [rdr (io/reader file)]
    (into [] (comp conll-xform (map #(mapv parse-word %)))
          (line-seq rdr))))
