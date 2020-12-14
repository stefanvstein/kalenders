(defproject timeing "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :plugins          [[lein-cloverage "1.2.1"]]
  :main ^:skip-aot timeing.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
