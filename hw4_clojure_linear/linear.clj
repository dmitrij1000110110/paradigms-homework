; Checkers
(defn vector-of-type? [type-checker v] (and
                                         (vector? v)
                                         (every? type-checker v)))

(defn equal-size-collection-of-type? [collection-checker type-checker x y] (and
                                                                             (collection-checker type-checker x)
                                                                             (collection-checker type-checker y)
                                                                             (== (count x) (count y))))

(def equal-size-vector-of-number? (partial equal-size-collection-of-type? vector-of-type? number?))

(defn matrix-of-number? [m] (vector-of-type? (fn [v] (partial equal-size-vector-of-number? v)) m))

(defn equal-size-matrix-of-number? [x y] (and
                                           (matrix-of-number? x)
                                           (matrix-of-number? y)
                                           (equal-size-collection-of-type? every? vector? x y)
                                           (== (equal-size-vector-of-number? (first x) (first y)))))

; Vectors
(defn for-each-coordinate [fun checker] (fn [& args]
                                          {:pre  [(every? (partial checker (first args)) args)]
                                           :post [(checker (first args) %)]}
                                          (apply mapv fun args)))

(def v+ (for-each-coordinate + equal-size-vector-of-number?))
(def v- (for-each-coordinate - equal-size-vector-of-number?))
(def v* (for-each-coordinate * equal-size-vector-of-number?))

(defn scalar [x y]
  {:pre  [(equal-size-vector-of-number? x y)]
   :post [(number? %)]}
  (apply + (v* x y)))

(defn v*s [v & scalars]
  {:pre  [(and (vector-of-type? number? v) (every? number? scalars))]
   :post [(equal-size-vector-of-number? v %)]}
  (let [scalar (reduce * scalars)]
    (mapv (partial * scalar) v)))

; Matrix
(def m+ (for-each-coordinate v+ equal-size-matrix-of-number?))
(def m- (for-each-coordinate v- equal-size-matrix-of-number?))
(def m* (for-each-coordinate v* equal-size-matrix-of-number?))

(defn transpose [m]
  {:pre  [(matrix-of-number? m)]
   :post [(and
            (matrix-of-number? %)
            (== (count m) (count (first %)))
            (== (count (first m)) (count %)))]}
  (apply mapv vector m))

(defn m*s [m & scalars]
  {:pre  [(and
            (matrix-of-number? m)
            (every? number? scalars))]
   :post [(equal-size-matrix-of-number? m %)]}
  (let [scalar (reduce * scalars)]
    (mapv (fn [x] (v*s x scalar)) m)))

(defn m*v [m v]
  {:pre  [(and
            (matrix-of-number? m)
            (vector-of-type? number? v)
            (== (count (first m)) (count v)))]
   :post [(and
            (vector-of-type? number? %)
            (== (count m) (count %)))]}
  (mapv (partial scalar v) m))
(defn v*m [v m] (m*v (transpose m) v))

(defn reduce-operation [fun] (fn [& vectors] (reduce fun vectors)))
(def m*m (reduce-operation (fn [x y]
                             {:pre  [(and
                                       (matrix-of-number? x)
                                       (matrix-of-number? y)
                                       (== (count (first x)) (count y)))]
                              :post [(and (matrix-of-number? %)
                                          (== (count %) (count x))
                                          (== (count (first %)) (count (first y))))]}
                             (mapv (partial m*v (transpose y)) x))))
(def vect (reduce-operation (fn [a b]
                              {:pre  [(and
                                        (equal-size-vector-of-number? a b)
                                        (== (count a) 3))]
                               :post [(equal-size-vector-of-number? a %)]}
                              (let [[x y z] a
                                    m [[0 (- z) y]
                                       [z 0 (- x)]
                                       [(- y) x 0]]]
                                (m*v m b)))))

; Broadcast
(defn equal-size-tensor-of-number [a b]
  (or (and (number? a)
           (number? b))
      (equal-size-vector-of-number? a b)
      (and
        (equal-size-collection-of-type? vector-of-type? vector? a b)
        (every? true? (mapv equal-size-tensor-of-number a b)))
      ))

(defn is-suffix? [x y]                                      ; is y dimensions suffix of x
  (or (equal-size-tensor-of-number x y)
      (and (vector? x)
           (is-suffix? (first x) y))))

(defn broadcast [x y]
  {:pre [(is-suffix? x y)]}
  (if (equal-size-tensor-of-number x y) y (mapv #(broadcast % y) x))) ; broadcast y to x

(defn reduce-tensor [fun neutral] (fn [& vectors] (if (== 1 (count vectors))
                                                    (fun neutral (first vectors))
                                                    (reduce fun vectors))))

(defn for-each-tensor-with-broadcast [baseFun neutral]
  (reduce-tensor (fn red [x y]
                   (letfn [(fun [& args] (if (every? number? args)
                                           (apply baseFun args)
                                           (apply mapv fun args)))]
                     (fun (if (is-suffix? x y) (identity x) (broadcast y x))
                          (if (is-suffix? x y) (broadcast x y) (identity y)))))
                 neutral))
(def b+ (for-each-tensor-with-broadcast + 0))
(def b- (for-each-tensor-with-broadcast - 0))
(def b* (for-each-tensor-with-broadcast * 1))
(def bd (for-each-tensor-with-broadcast / 1))