(ns conllu
  (:require [clojure.spec :as s]
            [clojure.spec.test :as t]))

(s/def ::multi (s/tuple pos-int? pos-int?))
(s/def ::empty (s/tuple pos-int? pos-int?))
(s/def ::index pos-int?)
(s/def ::form string?)
(s/def ::lemma string?)
(s/def ::upos #{:ADJ :ADP :ADV :AUX :CONJ :DET :INTJ :NOUN :NUM
                :PART :PRON :PROPN :PUNCT :SCONJ :SYM :VERB :X})
(s/def ::xpos string?)
(s/def ::morph (s/map-of simple-keyword? string?))
(s/def ::head nat-int?)
(s/def ::rel simple-keyword?)
(s/def ::deps (s/map-of nat-int? simple-keyword?))
(s/def ::misc (s/map-of simple-keyword? string?))

(s/def ::word
  (s/or :index (s/keys :req [::index ::form ::head]
                       :opt [::lemma ::upos ::xpos ::morph ::rel ::deps ::misc])
        :multi (s/keys :req [::multi ::form])
        :empty (s/keys :req [::empty ::deps])))

(s/def ::sent (s/every ::word :min-count 1))
