(ns conllu.core
  "utils."
  (:refer-clojure :exclude [vector])
  (:require [clj-tuple :refer [vector]]
            [clojure.spec :as s]
            conllu))

(s/fdef projective?
        :args (s/cat :sent :conllu/sent)
        :ret boolean?)

(defn projective?
  "checks in `O(|sent|^2)` whether the dep tree for `sent` has crossing arcs."
  [sent]
  (->> (or (< i i' j j') (< i' i j' j))
       (for [[[i j] & i'j'+]
             (->> sent
                  (filter :conllu/index)
                  (map (fn [{i :conllu/index j :conllu/head}]
                         (if (< i j) (vector i j) (vector j i))))
                  (iterate next)
                  (take-while some?))
             [i' j'] i'j'+])
       (every? false?)))
