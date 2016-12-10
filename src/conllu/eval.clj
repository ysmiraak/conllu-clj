(ns conllu.eval
  "for calculating attachment scores."
  (:refer-clojure :exclude [hash-map])
  (:require [clj-tuple :refer [hash-map]]
            [clojure.spec :as s]
            conllu))

(s/fdef index
        :args (s/cat :sent :conllu/sent)
        :ret (s/every-kv :conllu/index :conllu/word))

(defn index
  "indexes the words in `sent`, ignoring multi-words and empty-words."
  [sent]
  (dissoc (->> sent
               (map (juxt :conllu/index identity))
               (into {})) nil))

(s/fdef attachment
        :args (s/cat :keyf ifn? :gold :conllu/sent :eval :conllu/sent)
        :ret (s/map-of boolean? pos-int?))

(defn attachment
  "returns the stats for attachment comparison between `gold` and `eval`. `keyf`
  can be [[-label]] or [[+label]]."
  [keyf gold eval]
  (let [gold (index gold) eval (index eval)]
    (->> (= (-> idx gold keyf) (-> idx eval keyf))
         (for [idx (keys gold)])
         frequencies)))

(def -label "`keyf` for unlabeled [[attachment]]." :conllu/head)
(def +label "`keyf` for labeled [[attachment]]." (juxt :conllu/head :conllu/rel))

(s/fdef micro-average
        :args (s/cat :res+ (s/every (s/nilable (s/map-of boolean? rational?))))
        :ret (s/and rational? #(<= 0 % 1)))

(defn micro-average
  "word-based. `(micro-average (map (partial attachment +label) gold eval))`"
  [res+]
  (let [{t true f false}
        (reduce (partial merge-with +)
                (hash-map true 0 false 0)
                res+)]
    (/ t (+ t f))))

(defn macro-average
  "sent-based. `(macro-average (map (partial attachment +label) gold eval))`"
  [res+]
  (micro-average
   (map (fn [{t true f false :or {t 0 f 0}}]
          (->> (hash-map true (/ t tot) false (/ f tot))
               (when-not (zero? tot))
               (let [tot (+ t f)])))
        res+)))

(s/fdef diff
        :args (s/cat :gold :conllu/sent :eval :conllu/sent :key+ (s/every keyword?))
        :ret (s/coll-of (s/coll-of keyword? :kind set?) :kind vector?)
        :fn #(= (-> % :args :gold) (-> % :args :eval) (-> % :ret count)))

(defn diff
  "returns for each word the keys in `key+` where `gold` and `eval` differ."
  [gold eval key+]
  (mapv (fn [g e]
          (reduce (fn [res k] (if (= (g k) (e k)) res (conj res k)))
                  #{} key+))
        gold eval))
