(ns cjunimir.core
  (:require [cjunimir.parse :as parse]
            [cjunimir.eval :as eval]
            [cjunimir.render :as render]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main [& args]
  (let [argv (vec args)
        input-file (java.io.File. (argv 0))
        output-file (java.io.File. (argv 1))]
    (with-open [input (io/reader input-file)
                output (java.io.FileOutputStream. output-file)]
      (let [program (parse/parse (slurp input))
            segments (eval/eval-program program)]
        (render/render-png output segments)))))
