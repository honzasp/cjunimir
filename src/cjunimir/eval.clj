(ns cjunimir.eval)

(defrecord Pen [width red green blue])
(defrecord Turtle [pen x y angle])
(defrecord Segment [pen x1 y1 x2 y2])

(defn eval-expr [vars expr]
  (defn binop [op left right]
    (op (eval-expr vars left) (eval-expr vars right)))
  (defn unop [op middle]
    (op (eval-expr vars middle)))
  (case (expr 0)
    :int (expr 1)
    :var (vars (expr 1))
    :add (binop + (expr 1) (expr 2))
    :sub (binop - (expr 1) (expr 2))
    :mul (binop * (expr 1) (expr 2))
    :div (binop quot (expr 1) (expr 2))
    :neg (unop - (expr 1))))

(declare eval-stmt)

(defn eval-stmts [procs vars stmts turtle cont]
  (if (empty? stmts)
    (cont turtle)
    (eval-stmt procs vars (first stmts) turtle 
      #(eval-stmts procs vars (next stmts) % cont))))

(defn eval-repeat [procs vars counter body-stmts turtle cont]
  (let [next-cont #(eval-repeat procs vars (dec counter) body-stmts % cont)
        next-thunk #(eval-stmts procs vars body-stmts turtle next-cont)]
    (if (pos? counter)
      [:poline next-thunk]
      (cont turtle))))

(defn eval-stmt [procs vars stmt turtle cont]
  (case (stmt 0)
    :if (if (pos? (eval-expr vars (stmt 1)))
          (eval-stmts procs vars (stmt 2) turtle cont)
          (cont turtle))
    :repeat (eval-repeat procs vars 
                         (eval-expr vars (stmt 1)) (stmt 2)
                         turtle cont)
    :proc (let [proc (get procs (stmt 1))
                args (map #(eval-expr vars %) (stmt 2))]
            (if (nil? proc)
              [:error (str "procedure " (stmt 1) " does not exist")]
              (proc procs (vec args) turtle cont)))
    :split (let [split-cont (fn [turtle] [:leaf])
                 split-tree (eval-stmts procs vars (stmt 1) turtle split-cont)]
             [:branch (cont turtle) split-tree])
    :define (cont turtle)))

(defn proc-forward [_ args turtle cont]
  (if (= 1 (count args))
    (let [distance (args 0)
          angle-rad (* Math/PI (/ (:angle turtle) 180.0))
          x-shift (* distance (Math/sin angle-rad))
          y-shift (* (- distance) (Math/cos angle-rad))
          x1 (:x turtle) y1 (:y turtle)
          x2 (+ x1 x-shift) y2 (+ y1 y-shift)
          next-turtle (assoc turtle :x x2 :y y2)
          coord #(/ (Math/round (* 32.0 %)) 32.0)]
      (if (pos? (:width (:pen turtle)))
        (let [segment (Segment. (:pen turtle) (coord x1) (coord y1) 
                                (coord x2) (coord y2))]
          [:segment segment (cont next-turtle)])
        (cont next-turtle)))
    [:error "forward() must be called with one argument"]))

(defn proc-color [_ args turtle cont]
  (if (= 3 (count args))
    (let [next-pen (assoc (:pen turtle) 
                          :red (args 0) :green (args 1) :blue (args 2))]
      (cont (assoc turtle :pen next-pen)))
    [:error  "color() must be called with three arguments"]))

(defn proc-pen [_ args turtle cont]
  (if (= 1 (count args))
    (cont (assoc-in turtle [:pen :width] (args 0)))
    [:error  "pen() must be called with one argument"]))

(defn proc-rotate-meta [proc-name dir]
  (fn [_ args turtle cont]
    (if (= 1 (count args))
      (cont (update-in turtle [:angle] (partial + (* dir (args 0)))))
      [:error (str proc-name "() must be called with one argument")])))

(defn proc-proc-meta [proc-name arg-names stmts]
  (fn [procs args turtle cont]
    (if (= (count args) (count arg-names))
      (let [vars (into (hash-map) (map vector arg-names args))]
        [:poline #(eval-stmts procs vars stmts turtle cont)])
      [:error (str proc-name "() must be called with " (count arg-names)
                       (if (= 1 (count arg-names)) "argument" "arguments"))])))

(defn collect-procs [prog]
  (let [prim-procs {"forward" proc-forward
                    "color" proc-color
                    "pen" proc-pen
                    "left" (proc-rotate-meta "left" -1)
                    "right" (proc-rotate-meta "right" 1)}
        user-defs (filter #(= (% 0) :define) prog)
        user-procs (map #(vector (% 1) (proc-proc-meta (% 1) (% 2) (% 3))) user-defs)]
    (into prim-procs user-procs)))

(defn peel-layers [root]
  (defn peel-layer [[trees _]]
    (loop [in-trees trees
           out-trees (list)
           out-segments (list)]
      (if (empty? in-trees)
        (if (and (empty? out-trees) (empty? out-segments))
          nil 
          [out-trees out-segments])
        (let [in-tree (first in-trees)
              next-trees (next in-trees)]
          (case (in-tree 0)
            :segment (recur next-trees
                            (cons (in-tree 2) out-trees)
                            (cons (in-tree 1) out-segments))
            :branch (recur (cons (in-tree 1) 
                                 (cons (in-tree 2) next-trees))
                           out-trees out-segments)
            :poline (recur (cons ((in-tree 1)) next-trees)
                            out-trees out-segments)
            :error (throw (RuntimeException. (in-tree 1)))
            :leaf (recur next-trees out-trees out-segments))))))
                        
  (let [all-layers (iterate peel-layer [(list root) nil])
        layers (take-while #(not (nil? %)) all-layers)]
    (for [[_ segments] layers segment segments] segment)))

(defn eval-program [prog]
  (let [procs (collect-procs prog)
        vars (hash-map)
        turtle (Turtle. (Pen. 0 0 0 0) 0 0 0)
        tree-root (eval-stmts procs vars prog turtle (fn [_] [:leaf]))]
    (peel-layers tree-root)))
