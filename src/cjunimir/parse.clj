(ns cjunimir.parse
  (:require [the.parsatron :as p]
             clojure.string))

(defn $>
  ([p f] (p/let->> [x p] (p/always (f x))))
  ([p q f] (p/let->> [x p, y q] (p/always (f x y))))
  ([p q r f] (p/let->> [x p, y q, z r] (p/always (f x y z))))
  ([p q r s f] (p/let->> [x p, y q, z r, w s] (p/always (f x y z w)))))

(defn <*
  ([p q] (p/let->> [x p, _ q] (p/always x)))
  ([p q r] (p/let->> [x p, _ q, _ r] (p/always x))))

(defn *>
  ([p q] (p/let->> [_ p, x q] (p/always x)))
  ([p q r] (p/let->> [_ p, _ q, x r] (p/always x))))

(defn sep-by [p sep]
  (p/either ($> (p/attempt p) (p/many (*> (p/attempt sep) p)) #(cons %1 %2))
            (p/always (list))))

(defn ord [ch] (Character/getNumericValue ch))
(defn in-range? [begin ch end] (<= (ord begin) (ord ch) (ord end)))
(defn digit? [ch] (in-range? \0 ch \9))
(defn letter? [ch] (or (in-range? \a ch \z) (in-range? \A ch \Z)))
(defn alpha-num? [ch] (or (digit? ch) (letter? ch)))
(defn space? [ch] (#{\space \tab \return \newline} ch))

(defn digit [] (p/token digit?))
(defn letter [] (p/token letter?))
(defn alpha-num [] (p/token alpha-num?))
(defn space [] (p/token space?))
(defn spaces [] (p/many (p/attempt (space))))

(defmacro deflex [name args & body]
  `(p/defparser ~name ~args
     (<* (p/>> ~@body) (spaces))))

(defn left-assoc [op expr]
  (p/let->> [head expr
             tail (p/many (p/attempt ($> op expr vector)))]
    (let [f (fn [left [op right]] (vector op left right))
          tree (reduce f head tail)]
      (p/always tree))))


(deflex integer []
  (p/let->> [char-digits (p/many1 (digit))]
    (let [decimal-digits (map #(- (ord %) (ord \0)) char-digits)
          decimal-powers (take (count decimal-digits) (iterate (partial * 10) 1))
          digit-values (map * (reverse decimal-powers) decimal-digits)]
      (p/always (reduce + 0 digit-values)))))

(deflex ident []
  (p/let->> [first-char (letter)
             next-chars (p/many (p/attempt (alpha-num)))]
    (let [all-chars (lazy-cat [first-char] next-chars)]
      (p/always (clojure.string/join all-chars)))))

(deflex add-op []
  (p/choice ($> (p/char \+) (constantly :add))
            ($> (p/char \-) (constantly :sub))))
(deflex mul-op []
  (p/choice ($> (p/char \*) (constantly :mul))
            ($> (p/char \/) (constantly :div))))
(deflex neg-op []
  ($> (p/char \-) (constantly :neg)))

(deflex left-paren [] (p/char \())
(deflex right-paren [] (p/char \)))
(deflex left-brace [] (p/char \{))
(deflex right-brace [] (p/char \}))
(deflex comma [] (p/char \,))

(defn special-ident [word]
  (p/attempt (p/bind (ident) 
                     (fn [w] (if (= w word) (p/always word) (p/never))))))

(defn parens [p] (p/between (left-paren) (right-paren) p))
(defn braces [p] (p/between (left-brace) (right-brace) p))

(declare expr)

(p/defparser integer-expr []
  ($> (integer) #(vector :int %)))
(p/defparser var-expr []
  ($> (ident) #(vector :var %)))
(p/defparser atom-expr []
  (p/choice (integer-expr) (var-expr) (parens (expr))))

(p/defparser neg-expr []
  (p/choice ($> (p/attempt (neg-op)) (atom-expr) #(vector %1 %2))
            (atom-expr)))
(p/defparser mul-expr []
  (left-assoc (mul-op) (neg-expr)))
(p/defparser add-expr []
  (left-assoc (add-op) (mul-expr)))
(p/defparser expr [] (add-expr))

(declare stmt)

(p/defparser braces-stmts []
  ($> (braces (p/many (stmt))) vec))

(p/defparser proc-stmt []
  ($> (ident) (parens (sep-by (expr) (comma))) #(vector :proc %1 (vec %2))))
(p/defparser repeat-stmt []
  ($> (special-ident "repeat") (parens (expr)) (braces-stmts) #(vector :repeat %2 %3)))
(p/defparser if-stmt []
  ($> (special-ident "if") (parens (expr)) (braces-stmts) #(vector :if %2 %3)))
(p/defparser split-stmt []
  ($> (special-ident "split") (braces-stmts) #(vector :split %2)))
(p/defparser stmt []
  (p/choice (repeat-stmt) (if-stmt) (split-stmt) (proc-stmt)))

(p/defparser define-top-stmt []
  ($> (special-ident "define")
      (ident)
      (parens (sep-by (ident) (comma)))
      (braces-stmts)
      #(vector :define %2 (vec %3) %4)))
(p/defparser top-stmt []
  (p/choice (define-top-stmt) (stmt)))
(p/defparser program []
  ($> (spaces) (p/many (top-stmt)) #(vec %2)))

(defn parse [input]
  (p/run (<* (program) (p/eof)) input))
