(ns cjunimir.core
  (:require [cjunimir.parse :as parse]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main [& args]
  (let [args-vec (vec args)]
    (with-open [input (io/reader
                (if (>= (count args-vec) 1) 
                  (java.io.File. (get args-vec 0))
                  *in*))]
      (prn (parse/parse (slurp input))))))
