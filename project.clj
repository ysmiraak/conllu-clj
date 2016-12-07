(defproject conllu "0.1.0-SNAPSHOT"
  :description "a clojure library for working with the CoNLL-U format."
  :url "https://github.com/ysmiraak/conllu-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [clj-tuple "0.2.2"]
                 [org.maltparser/maltparser "1.9.0"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  ;; :jvm-opts ["-Xmx1g"]
  :jar-exclusions [#"(?:^|/)(trial|example)/"]
  :codox {:output-path "doc"
          :source-uri "https://github.com/ysmiraak/conllu-clj/blob/master/{filepath}#L{line}"
          :metadata {:doc/format :markdown}})
