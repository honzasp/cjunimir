(ns cjunimir.parse-test
  (:require [clojure.test :refer :all]
            [cjunimir.parse :refer :all]
            [the.parsatron :as p]))

(defn parse-all [parser input]
  (p/run (<* parser (p/eof)) input))
        
(deftest parse-integers
  (are [expected input] (= expected (parse-all (integer) input))
       0 "0"
       10 "10"
       89 "89  "))

(deftest parse-identifiers
  (are [expected input] (= expected (parse-all (ident) input))
       "krunimir" "krunimir"
       "x86" "x86 "
       "B0MB" "B0MB   "))

(deftest parse-ops
  (are [expected input] (= expected (parse-all (add-op) input))
       :add "+ "
       :sub "- ")
  (are [expected input] (= expected (parse-all (mul-op) input))
       :mul "*  "
       :div "/")
  (are [expected input] (= expected (parse-all (neg-op) input))
       :neg "- "))

(deftest parse-simple-exprs
  (are [expected input] (= expected (parse-all (expr) input))
      '(:var "foo") "foo"
      '(:int 100) "100"
      '(:var "baz") "(baz)"
      '(:int 23) "(((23)))"))

(deftest parse-arithmetic
  (are [expected input] (= expected (parse-all (expr) input))
      '(:neg (:int 10)) "-10"
      '(:add (:int 1) (:int 2)) "1+2"
      '(:sub (:var "foo") (:int 10)) "foo - 10"
      '(:mul (:int 2) (:var "baz")) "2*baz"
      '(:div (:var "x") (:var "y")) "x/y"
      '(:add (:mul (:int 2) (:var "x")) (:int 4)) "2*x + 4"
      '(:sub (:sub (:int 2) (:int 3)) (:int 4)) "2 - 3 - 4"
      '(:sub (:neg (:int 2)) (:div (:neg (:int 3)) (:neg (:int 4)))) "-2 - -3/-4"
      '(:mul (:add (:int 1) (:int 2)) (:int 3)) "( 1 + 2 ) * 3 "
      '(:mul (:mul (:int 3) (:int 2)) (:int 1)) "( 3 * 2 ) * 1 "))

(deftest parse-statements
  (are [expected input] (= expected (parse-all (stmt) input))
      '(:proc "forward" [(:int 2)]) "forward(2)"
      '(:proc "color" [(:int 255) (:int 0) (:int 0)]) "color ( 255 , 0 , 0 ) "
      '(:split [(:proc "beep" [])]) "split { beep() }"
      '(:proc "splitt" []) "splitt()"
      '(:split [(:proc "beep" []) (:proc "bark" [])]) "split { beep() bark() }"
      '(:if (:int 10) [(:proc "beep" [])]) "if ( 10 ) { beep() }"
      '(:repeat (:int 20) [(:proc "beep" [])]) "repeat(20) { beep() }"))

(deftest parse-program
  (are [expected input] (= expected (parse-all (program) input))
      '[(:define "foo" ["x", "y", "z"] [(:proc "beep" [])])] 
          "define foo(x, y, z) { beep() }"
      '[(:define "foo" [] []) (:proc "beep" [])]
          " define foo ( ) { } beep ( ) "))
