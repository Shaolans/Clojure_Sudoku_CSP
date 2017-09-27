(defproject mrsudoku "0.1.0-SNAPSHOT"
  :description "Mini-projet Sudoku"
  :url ".."
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [midje "1.8.3"]
                 [seesaw "1.4.5"]
                 [rescribe "0.2.0-SNAPSHOT"]]
  :main ^:skip-aot mrsudoku.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
