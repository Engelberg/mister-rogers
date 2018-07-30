(ns mister-rogers.protocols)

(defprotocol Objective
  (evaluate [this solution data] "Return number or Evaluation")
  (minimizing? [this]))

(defprotocol ObjectiveDelta
  "Optionally implement this protocol if there is a more efficient way to evaluate one solution in terms of another"
  (evaluate-delta [this move cur-solution cur-evaluation data]))

(defprotocol Evaluation
  (value [this] "Return number"))

(defprotocol Constraint
  (validate [this solution data] "Return Validation, move arity is optional"))

(defprotocol ConstraintDelta
  "Optionally implement this protocol if there is a more efficient way to evaluate one constraint in terms of another"
  (validate-delta [this move cur-solution cur-validation data]))

(defprotocol Validation
  (passed? [this]))

(defprotocol PenalizingValidation
  (penalty [this] "Return zero if validation passes, or positive penalty"))

(defprotocol Move
  (apply-move [this solution] "Return Solution"))

;; Default implementations

(extend-type Object
  ObjectiveDelta
  (evaluate-delta [this move cur-solution cur-evaluation data]
    (evaluate this (apply-move move cur-solution) data))
  ConstraintDelta
  (validate-delta [this move cur-solution cur-validation data]
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

