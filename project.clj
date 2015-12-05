(defproject cjunimir "0.1.0-SNAPSHOT"
  :description "Reimplementation of good old Krunimir in Clojure"
  :url "http://github.com/honzasp/cjunimir"
  :license {:name "Unlicense"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [the/parsatron "0.0.7"]]
  :plugins [[cider/cider-nrepl "0.10.0"]]
  :main cjunimir.core
  :aot [cjunimir.core]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
