(ns cjunimir.core
  (:require [cjunimir.parse :as parse]
            [cjunimir.eval :as eval]
            [cjunimir.render :as render]
            [clojure.java.io :as io])
  (:gen-class))

(defn slurp-program [argv]
  (let [stream (if (>= (count argv) 1)
                 (java.io.File. (get argv 0))
                 *in*)]
    (with-open [input (io/reader stream)]
      (slurp input))))

(defn -main [& args]
  (let [argv (vec args)
        program (parse/parse (slurp-program argv))
        segments (eval/eval-program program)]
    (render/render-svg *out* segments)))
