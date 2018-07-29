(ns mister-rogers.protocols)

(defprotocol Objective
  (evaluate [this solution data] [this move cur-solution cur-evaluation data]
    "Return number or Evaluation, move arity is optional")
  (minimizing? [this]))

(defprotocol Evaluation
  (value [this] "Return number"))

(defprotocol Constraint
  (validate [this solution data] [this move cur-solution cur-validation data]
    "Return Validation, move arity is optional"))

(defprotocol Validation
  (passed? [this]))

(defprotocol PenalizingValidation
  (penalty [this] "Return zero if validation passes, or positive penalty"))

(defprotocol Move
  (apply-move [this solution] "Return Solution"))

;; Default implementations

(extend-type Object
  Objective
  (evaluate [this move cur-solution cur-evaluation data]
    (evaluate this (apply-move move cur-solution) data))
  Constraint
  (validate [this move cur-solution cur-validation data]
    (validate this (apply-move move cur-solution) data)))

(extend-protocol Evaluation
  Double (value [this] this)
  Long (value [this] this))

(extend-protocol Validation
  Boolean (passed? [this] this)
  nil (passed? [this] false)
  Double (passed? [this] (= this 0.0))
  Long (passed? [this] (= this 0))
  mister_rogers.protocols.PenalizingValidation (passed? [this] (zero? (penalty this))))

(extend-protocol PenalizingValidation
  Double (penalty [this] this)
  Long (penalty [this] this))

;; Simple Implementations

(defrecord UnanimousValidation [validations]
  Validation
  (passed? [this] (every? passed? validations)))

(defrecord PenalizedEvaluation [evaluation penalizing-validations minimizing?]
  Evaluation
  (value [this]
    (let [p (transduce (map penalty) + penalizing-validations)]
      (if minimizing?
        (+ (value evaluation) p)
        (- (value evaluation) p)))))

;; Problem class

(defrecord Problem [data objective solution-generator mandatory-constraints penalizing-constraints])
