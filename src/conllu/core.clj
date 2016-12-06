(ns conllu.core
  (:refer-clojure :exclude [vector])
  (:require [clj-tuple :refer [vector]]
            [clojure.spec :as s]
            conllu))

(s/fdef projective?
        :args (s/cat :sent :conllu/sent)
        :ret boolean?)

(defn projective? [sent]
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


;; (defn -main [& [path]]

;;   (def proj-profile+
;;     (->> (or path ".") io/file file-seq
;;          (map (juxt identity io/as-relative-path))
;;          (filter (comp #(str/ends-with? % ".conllu") peek))
;;          (map (fn [[file name]]
;;                 (let [i (inc (str/last-index-of name \/))
;;                       j (str/index-of name \- i)]
;;                   (println "checking" name)
;;                   (-> file
;;                       conll->proj-freq
;;                       (assoc :lang (subs name i j))))))
;;          (group-by :lang)
;;          (mapv (fn [[lang proj-freq+]]
;;                  (let [[proj# nonp#]
;;                        (->> proj-freq+
;;                             (map (juxt #(get % true 0) #(get % false 0)))
;;                             (apply map +))
;;                        sent# (+ proj# nonp#)]
;;                    {:lang lang :sent# sent# :nonp# nonp#
;;                     :nonp% (-> nonp# (/ sent#) double)})))))

;;   (println)
;;   (->> proj-profile+
;;        (sort-by :nonp% >)
;;        clojure.pprint/print-table)

;;   (println)
;;   (->> proj-profile+
;;        (map (juxt :nonp# :sent#))
;;        (apply map +)
;;        (apply /)
;;        double
;;        (println "total non-projective trees in percentage:")))
