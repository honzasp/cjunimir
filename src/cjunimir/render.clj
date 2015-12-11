(ns cjunimir.render
  (:import (java.awt BasicStroke Color)
           (java.awt.geom Line2D$Double)
           (java.awt.image BufferedImage)
           (java.util Formatter Locale)
           (javax.imageio ImageIO)))

(defn render-svg [out-writer segments]
  (let [formatter (Formatter. out-writer Locale/US)
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

(defn render-png [out-stream segments]
  (let [buf-img (BufferedImage. 701 701 BufferedImage/TYPE_INT_ARGB)
        gfx (.createGraphics buf-img)]
    (doseq [segment segments]
      (defn coord [kw] (+ 350 (kw segment)))
      (let [pen (:pen segment)
            line (Line2D$Double. (coord :x1) (coord :y1) 
                                 (coord :x2) (coord :y2))
            color (Color. (:red pen) (:green pen) (:blue pen))
            stroke (BasicStroke. (:width pen))]
        (doto gfx
          (.setPaint color)
          (.setStroke stroke)
          (.draw line))))
    (ImageIO/write buf-img "png" out-stream)))
