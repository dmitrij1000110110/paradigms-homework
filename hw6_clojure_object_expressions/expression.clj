; ============== HW 10 ==============
(def constant constantly)
(defn variable [name] (fn [vars] (vars name)))

(defn createFunctionalOperation [operation] (fn [& funcs] (fn [params] (apply operation (mapv (fn [f] (f params)) funcs)))))
(defn _divide ([x] (/ (double x)))
  ([first & args] (reduce #(/ (double %1) (double %2)) first args)))
(def _exp #(Math/exp %))

(def add (createFunctionalOperation +))
(def subtract (createFunctionalOperation -))
(def multiply (createFunctionalOperation *))
(def divide (createFunctionalOperation _divide))
(def negate subtract)
(def exp (createFunctionalOperation _exp))
(defn sumexp [& args] (apply add (mapv exp args)))
(defn softmax [& args] (divide (exp (first args)) (apply sumexp args)))

(defn getListParser [TOKEN_TO_OBJ constantCreator variableCreator]
  (fn parse [token]
    (cond
      (list? token) (apply (TOKEN_TO_OBJ (first token)) (mapv parse (rest token)))
      (number? token) (constantCreator token)
      :else (variableCreator (str token))
      )))

(defn getParser [TOKEN_TO_OBJ constantCreator variableCreator]
  (fn parser [expression]
    ((getListParser TOKEN_TO_OBJ constantCreator variableCreator)
     (read-string expression))))

(def OPERATIONS_FUNC
  {'+       add,
   '-       subtract,
   '*       multiply,
   '/       divide,
   'negate  negate
   'sumexp  sumexp
   'softmax softmax})
(def parseFunction (getParser OPERATIONS_FUNC constant variable))

; ============== HW 11 ==============

(defn proto-get [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :proto) (proto-get (obj :proto) key)))
(defn proto-call [obj key & args]
  (apply (proto-get obj key) obj args))
(defn field [key] #(proto-get % key))
(defn method [key] (fn [obj & args] (apply proto-call obj key args)))
(defn constructor [cons proto]
  (fn [& args] (apply cons {:proto proto} args)))

(def _operation (field :operation))
(def _value (field :value))
(def _varName (field :varName))
(def _operand (field :operand))
(def _args (field :args))
(def _diffFunction (field :diffFunction))
(def diff (method :diff))
(def evaluate (method :evaluate))
(def toString (method :toString))
(def toStringInfix (method :toStringInfix))

(defn ExprCons [evaluate toString toStringInfix diff]
  {:evaluate      evaluate
   :toString      toString
   :toStringInfix toStringInfix
   :diff          diff})
(declare ZERO)
(def Constant (constructor
                (fn [this value]
                  (assoc this :value value))
                (ExprCons
                  (fn [this _] (_value this))
                  (fn [this] (format "%.1f" (double (_value this))))
                  toString
                  (fn [_ _] ZERO))))
(def ZERO (Constant 0.0))
(def ONE (Constant 1.0))

(def Variable (constructor
                (fn [this varName]
                  (assoc this :varName varName))
                (ExprCons
                  (fn [this vars] (get vars (_varName this)))
                  (fn [this] (_varName this))
                  toString
                  (fn [this diffVariable]
                    (if (= (_varName this) diffVariable) ONE ZERO)))))

(def OperationProto (ExprCons
                      (fn [this vars] (apply (_operation this) (mapv (fn [x] (evaluate x vars)) (_args this))))
                      (fn [this] (str "(" (_operand this) " "
                                      (clojure.string/join " " (mapv toString (_args this))) ")"))
                      (fn [this] (if (= (count (_args this)) 1)
                                   (str (_operand this) "(" (toStringInfix (first (_args this))) ")")
                                   (str "(" (clojure.string/join (str " " (_operand this) " ") (mapv toStringInfix (_args this))) ")")))
                      (fn [this diffVariable] ((_diffFunction this) (_args this) (mapv #(diff % diffVariable) (_args this))))))

(defn OperationCons [this operation operand diffFunction]
  (assoc this
    :operation operation
    :operand operand
    :diffFunction diffFunction))
(def Operation (constructor OperationCons OperationProto))
(defn createOperation [operation operand diffFunction]
  (constructor
    (fn [this & args] (assoc this :args args))
    (Operation operation operand diffFunction)))

(defn simpleDiff [operation] (fn [_ args'] (apply operation args')))
(def Add (createOperation
           +
           "+"
           (simpleDiff #(apply Add %&))))
(def Subtract (createOperation
                -
                "-"
                (simpleDiff #(apply Subtract %&))))
(def Negate (createOperation
              -
              "negate"
              (simpleDiff #(apply Negate %&))))

(declare Multiply)
(declare Divide)
(defn multDiff [args args'] (second (reduce
                                      (fn [[x x'] [y y']] [(Multiply x y)
                                                           (Add (Multiply x' y) (Multiply x y'))])
                                      (mapv vector args args'))))
(defn divDiff [[firstArg & restArgs] [firstArg' & restArgs']]
  (if (empty? restArgs)
    (Negate (Divide firstArg' (Multiply firstArg firstArg)))
    (let [denom (apply Multiply restArgs)
          denom' (multDiff restArgs restArgs')]
      (Divide (Subtract
                (Multiply firstArg' denom)
                (Multiply firstArg denom'))
              (Multiply denom denom)))))

(def Multiply (createOperation
                *
                "*"
                multDiff))
(def Divide (createOperation
              _divide
              "/"
              divDiff))

(declare Exp)
(defn expDiff [arg diffArg] (Multiply (Exp arg) diffArg))
(def Exp (createOperation
           #(Math/exp %)
           "exp"
           expDiff))
(defn sumexpDiff [args diffArgs] (apply Add (mapv expDiff args diffArgs)))
(def Sumexp (createOperation
              (fn [& args] (apply + (mapv _exp args)))
              "sumexp"
              sumexpDiff))

(def Softmax (createOperation
               (fn [& args] (_divide (_exp (first args)) (apply + (mapv _exp args))))
               "softmax"
               (fn [args diffArgs] (divDiff
                                     [(Exp (first args)) (apply Sumexp args)]
                                     (list (expDiff (first args) (first diffArgs))
                                           (sumexpDiff args diffArgs))))))


(defn bitsEval [operation] #(Double/longBitsToDouble (apply operation (mapv (fn [x] (Double/doubleToLongBits x)) %&))))
(def And (createOperation
           (bitsEval bit-and)
           "&"
           nil))
(def Or (createOperation
          (bitsEval bit-or)
          "|"
          nil))
(def Xor (createOperation
           (bitsEval bit-xor)
           "^"
           nil))
(def Impl (createOperation
            (bitsEval #(bit-or (bit-not %1) %2))
            "=>"
            nil))
(def Iff (createOperation
           (bitsEval #(bit-not (bit-xor %1 %2)))
           "<=>"
           nil))


(def OPERATIONS_OBJ
  {'+           Add,
   '-           Subtract,
   '*           Multiply,
   '/           Divide,
   'negate      Negate,
   'softmax     Softmax,
   'sumexp      Sumexp,
   '&           And,
   '|           Or,
   (symbol "^") Xor,
   '=>          Impl,
   '<=>         Iff})

(def parseObject (getParser OPERATIONS_OBJ Constant Variable))
