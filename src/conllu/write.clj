(ns conllu.write
  "for writing conllu files."
  (:require [clojure
             [spec :as s]
             [string :as str]]
            [clojure.java.io :as io]
            conllu))

(s/fdef str-avm :args (s/cat :c char? :m map?) :ret string?)

(defn str-avm
  "the inverse of [[conllu.parse/parse-avm]]."
  [c m]
  (->> (sort-by key m)
       (map (fn [[k v]]
              (let [k (if (keyword? k) (name k) k)
                    v (if (keyword? v) (name v) v)]
                (str k c v))))
       (str/join \|)))

(s/fdef str-word :args (s/cat :word :conllu/word) :ret string?)

(defn str-word
  "the inverse of [[conllu.parse/parse-word]]."
  [word]
  (let [{:keys [:conllu/index :conllu/multi :conllu/empty
                :conllu/form :conllu/lemma :conllu/upos :conllu/xpos :conllu/morph
                :conllu/head :conllu/rel :conllu/deps :conllu/misc]} word]
    (->> [(cond index index multi (str/join \- multi) empty (str/join \. empty))
          form lemma (when upos (name upos)) xpos (when morph (str-avm \= morph))
          head (when rel (name rel)) (when deps (str-avm \: deps)) (when misc (str-avm \= misc))]
         (map #(if % % \_))
         (str/join \tab))))

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

(s/fdef tex-escape :args (s/cat :s string?) :ret string?)

(defn tex-escape
  "escape `s` for `tex`."
  [s]
  (str/escape s {\$ "\\$" \% "\\%"}))

(s/def :tikz/style string?)

(s/fdef tikz-dep
        :args (s/cat :sent :conllu/sent :sel-fn+ (s/every ifn?))
        :ret (s/coll-of string? :kind vector?))

(defn tikz-dep
  "format `:conllu/index` words in `sent` into lines of `tex` code to be used
  with the `tikz-dependency` package. the text at each node will be selected by
  `sel-fn+` for each word:
  ```
  [(comp tex-escape name :conllu/xpos)
   (comp tex-escape name :conllu/upos)
   (comp tex-escape :conllu/form)]
  ```
  note that each `sel-fn` needs to escape the string properly for the target
  `tex` engine (`xelatex` recommended).

  if a word has an entry of `:tikz/style`, such as `[edge style={red},label
  style={fill=red}]`, that style will be used.

  `xelatex` template:
  ```
  \\documentclass{article}
  \\usepackage[a0paper,left=0cm,right=0cm,top=4cm,bottom=4cm]{geometry}
  \\usepackage{tikz-dependency}
  \\usepackage{xeCJK}
  \\begin{document}
  ...
  \\end{document}
  ```"
  [sent sel-fn+]
  (let [arc (fn [{:keys [:conllu/index :conllu/head :conllu/rel :tikz/style] :or {style ""}}]
              (if (zero? head) (str "\\deproot" style "{" index "}{root}")
                  (str "\\depedge" style "{" index "}{" head "}{" (-> rel name tex-escape) "}")))
        w+ (filter :conllu/index sent)]
    (as-> ["\\begin{dependency}" "\\begin{deptext}"] $
      (reduce #(-> %1 (conj (str/join " \\& " (map %2 w+))) (conj "\\\\")) $ sel-fn+)
      (conj $ "\\end{deptext}")
      (reduce #(conj %1 (arc %2)) $ w+)
      (conj $ "\\end{dependency}"))))
