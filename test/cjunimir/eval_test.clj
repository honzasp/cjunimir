(ns cjunimir.eval-test
  (:require [clojure.test :refer :all]
            [cjunimir.eval :refer :all]))

(def black (->Pen 1 0 0 0))
(def red (->Pen 1 255 0 0))

(defmacro are-evaluated [& clauses]
  `(are [expected# prog#] (= expected# (eval-program prog#))
        ~@clauses))

(deftest eval-primitives
  (are-evaluated
    [(->Segment black 0.0 0.0 0.0 -10.0)]
    '[[:proc "pen" [[:int 1]]]
      [:proc "forward" [[:int 10]]]]

    [(->Segment black 0.0 0.0 10.0 0.0)
     (->Segment black 10.0 0.0 10.0 -10.0)]
    '[[:proc "right" [[:int 90]]]
      [:proc "pen" [[:int 1]]]
      [:proc "forward" [[:int 10]]]
      [:proc "left" [[:int 90]]]
      [:proc "forward" [[:int 10]]]]

    [(->Segment (->Pen 10 255 0 100) 0.0 0.0 0.0 -10.0)]
    '[[:proc "pen" [[:int 10]]]
      [:proc "color" [[:int 255] [:int 0] [:int 100]]]
      [:proc "forward" [[:int 10]]]]))

(deftest eval-expressions
  (are-evaluated
    [(->Segment black 0.0 0.0 0.0 -42.0)]
    '[[:proc "pen" [[:int 1]]]
      [:proc "forward" 
            [[:mul [:int 2] [:add [:int 20] [:int 1]]]]]]))

(deftest eval-procedures
  (are-evaluated
    [(->Segment black 0.0 0.0 0.0 -10.0)
     (->Segment black 0.0 -10.0 20.0 -10.0)]
    '[[:define "line" ["length"]
              [[:proc "forward" [[:var "length"]]]
                [:proc "right" [[:int 90]]]]]
      [:proc "pen" [[:int 1]]]
      [:proc "line" [[:int 10]]]
      [:proc "line" [[:int 20]]]]))

(deftest eval-repeat-statements
  (are-evaluated
    [(->Segment black 0.0 0.0 0.0 -2.0)
     (->Segment black 0.0 -2.0 0.0 -4.0)
     (->Segment black 0.0 -4.0 0.0 -6.0)]
    '[[:proc "pen" [[:int 1]]]
      [:repeat [:int 3]
               [[:proc "forward" [[:int 2]]]]]]))

(deftest eval-if-statemets
  (are-evaluated
    [(->Segment black 0.0 0.0 0.0 -5.0)]
    '[[:proc "pen" [[:int 1]]]
      [:if [:int -2] [[:proc "forward" [[:int 8]]]]]
      [:if [:int 2] [[:proc "forward" [[:int 5]]]]]
      [:if [:int 0] [[:proc "forward" [[:int 3]]]]]]))

(deftest eval-split-statemets
  (are-evaluated
    [(->Segment black 0.0 0.0    0.0 -10.0)
     (->Segment red   0.0 -10.0 -15.0 -10.0)
     (->Segment black 0.0 -10.0  12.0 -10.0)
     (->Segment black 12.0 -10.0 25.0 -10.0)]
    '[[:proc "pen" [[:int 1]]]
      [:proc "forward" [[:int 10]]]
      [:split
              [[:proc "left" [[:int 90]]]
               [:proc "color" [[:int 255] [:int 0] [:int 0]]]
               [:proc "forward" [[:int 15]]]]]
      [:proc "right" [[:int 90]]]
      [:proc "forward" [[:int 12]]]
      [:proc "forward" [[:int 13]]]]))
