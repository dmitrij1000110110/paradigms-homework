(def constant constantly)
(defn variable [name] (fn [vars] (vars name)))

(defn createOperation [operation] (fn [& funcs] (fn [params] (apply operation (mapv (fn [f] (f params)) funcs)))))

(def add (createOperation +))
(def subtract (createOperation -))
(def multiply (createOperation *))
(def divide (createOperation (fn ([x] (/ (double x)))
                               ([first & args] (reduce #(/ (double %1) (double %2))
                                                       first args)))))
(def negate subtract)
(def exp (createOperation #(Math/exp %)))
(defn sumexp [& args] (apply add (mapv exp args)))
(defn softmax [& args] (divide (exp (first args)) (apply sumexp args)))

(def OPERATIONS
  {'+       add,
   '-       subtract,
   '*       multiply,
   '/       divide,
   'negate  negate
   'sumexp  sumexp
   'softmax softmax})

(defn parseFunction [expression]
  (letfn [(parse [token]
            (cond
              (list? token) (apply (OPERATIONS (first token)) (mapv parse (rest token)))
              (number? token) (constant token)
              :else (variable (str token))
              ))]
    (parse (read-string expression))))
