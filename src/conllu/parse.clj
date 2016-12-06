(ns conllu.parse
  (:refer-clojure :exclude [vector hash-map])
  (:require [clj-tuple :refer [hash-map vector]]
            [clojure
             [spec :as s]
             [string :as str]]
            [clojure.java.io :as io]
            conllu
            [clojure.spec.test :as t]))

;; (binding [s/*compile-asserts* true])

(s/check-asserts true)

(s/fdef specified?
        :args (s/cat :s string?)
        :ret boolean?)

(defn specified? [s]
  (not= "_" s))

(s/fdef parse-pos-int
        :args (s/cat :s string?)
        :ret pos-int?)

(defn parse-pos-int [s]
  (s/assert pos-int? (Long/parseLong s)))

(s/fdef parse-nat-int
        :args (s/cat :s string?)
        :ret nat-int?)

(defn parse-nat-int [s]
  (s/assert nat-int? (Long/parseLong s)))

(s/fdef parse-avm
        :args (s/cat :s string? :c char? :kf ifn? :vf ifn?)
        :ret map?)

(defn parse-avm [s c kf vf]
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
  ([t] #(hash-map t (vector (parse-pos-int (nth % 1)) (parse-pos-int (nth % 2))))))

(s/fdef parse-word
        :args (s/cat :line string?)
        :ret :conllu/word)

(defn parse-word [line]
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
      (assoc! :conllu/deps (parse-avm deps \: parse-nat-int keyword))
      (specified? misc)
      (assoc! :conllu/misc (parse-avm misc \= keyword identity))
      :finally persistent!)))

(def conll-xform
  (comp (map str/trim)
        (remove #(str/starts-with? % "#"))
        (partition-by str/blank?)
        (remove #(= [""] %))
        (map #(mapv parse-word %))))

(s/fdef parse-file
        :args (s/cat :file any?)
        :ret :conllu/sent)

(defn parse-file [file]
  (with-open [rdr (io/reader file)]
    (into [] conll-xform (line-seq rdr))))
