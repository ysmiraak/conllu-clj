(ns conllu.eval
  (:refer-clojure :exclude [hash-map])
  (:require [clj-tuple :refer [hash-map]]
            [clojure.spec :as s]
            [clojure.spec.test :as t]
            conllu))

(s/fdef index
        :args (s/cat :sent :conllu/sent)
        :ret (s/every-kv :conllu/index :conllu/word))

(defn index [sent]
  (dissoc (->> sent
               (map (juxt :conllu/index identity))
               (into {})) nil))

(s/fdef attachment
        :args (s/cat :keyf ifn? :gold :conllu/sent :eval :conllu/sent)
        :ret (s/map-of boolean? pos-int?))

(defn attachment [keyf gold eval]
  (let [gold (index gold) eval (index eval)]
    (->> (= (-> idx gold keyf) (-> idx eval keyf))
         (for [idx (keys gold)])
         frequencies)))

(def -label :conllu/head)
(def +label (juxt :conllu/head :conllu/rel))

(s/fdef micro-average
        :args (s/cat :res+ (s/every (s/nilable (s/map-of boolean? rational?))))
        :ret (s/and rational? #(<= 0 % 1)))

(defn micro-average [res+]
  (let [{t true f false}
        (reduce (partial merge-with +)
                (hash-map true 0 false 0)
                res+)]
    (/ t (+ t f))))

(defn macro-average [res+]
  (micro-average
   (map (fn [{t true f false :or {t 0 f 0}}]
          (->> (hash-map true (/ t tot) false (/ f tot))
               (when-not (zero? tot))
               (let [tot (+ t f)])))
        res+)))

(comment
  (require '[conllu.parse :as p])
  (def t (p/parse-file "./corpora/_test.conllu"))
  (def e (p/parse-file "./corpora/_eval.conllu"))
  ;; word-based uas 18/24 = 3/4
  (micro-average (map (partial attachment -label) t e))
  ;; word-based las 9/24 =  3/8
  (micro-average (map (partial attachment +label) t e))
  ;; sent-based uas 2/3
  (macro-average (map (partial attachment -label) t e))
  ;; sent-based las 1/3
  (macro-average (map (partial attachment +label) t e))
  )

(require '[conllu.parse :as p :reload true]
         '[conllu.core :as c])

(defn competition [lang]
  (let [t (p/parse-file (str "./corpora/" (name lang) "-ud-test.conllu"))
        e (p/parse-file (str "./corpora/" (name lang) "-ud-eval.conllu"))
        sent-las (macro-average (map (partial attachment +label) t e))
        word-las (micro-average (map (partial attachment +label) t e))
        sent-uas (macro-average (map (partial attachment -label) t e))
        word-uas (micro-average (map (partial attachment -label) t e))
        {+p true -p false :or {+p 0 -p 0}} (frequencies (map c/projective? t))]
    (println ":language" lang)
    (println ":non-proj" (format "%.4f" (double (/ -p (+ +p -p)))))
    (clojure.pprint/print-table
     [:type :score]
     [{:type "sent-las" :score (format "%.4f" (double sent-las))}
      {:type "word-las" :score (format "%.4f" (double word-las))}
      {:type "sent-uas" :score (format "%.4f" (double sent-uas))}
      {:type "word-uas" :score (format "%.4f" (double word-uas))}])
    (println)))

(comment
  |         :lang | :sent# | :nonp# |               :nonp% |
  |---------------+--------+--------+----------------------|
  |            de |  15894 |   1920 |    0.120800302000755 |
  |            ar |   7664 |    758 |  0.09890396659707724 |
  |            et |  18109 |   1730 |  0.09553260809542218 |
  |            en |  16622 |    823 |  0.04951269401997353 |
  |            bg |  11138 |    321 |  0.02882025498294128 |

  (doseq [lang [:bg :en :et :ar :de]] (competition lang)))
