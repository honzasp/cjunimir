(ns cjunimir.render
  (:import (java.util Formatter Locale)))

(defn render-svg [output segments]
  (let [formatter (Formatter. output Locale/US)
        fmt (fn [s & args] (.format formatter s (to-array args)))]
    (fmt "<svg version=\"1.1\" width=\"701\" height=\"701\"")
    (fmt " xmlns=\"http://www.w3.org/2000/svg\">\n")
    (doseq [segment segments
            :let [pen (:pen segment)]]
      (defn coord [kw] (+ 350 (kw segment)))
      (fmt "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\""
           (coord :x1) (coord :y1) (coord :x2) (coord :y2))
      (fmt " stroke=\"rgb(%d, %d, %d)\" stroke-width=\"%d\" />\n"
               (:red pen) (:green pen) (:blue pen) (:width pen)))
    (fmt "</svg>\n")))
