(ns mister-rogers.protocols
  (:import org.jamesframework.core.problems.objectives.evaluations.Evaluation
           org.jamesframework.core.problems.objectives.evaluations.SimpleEvaluation
           org.jamesframework.core.problems.constraints.validations.Validation
           org.jamesframework.core.problems.constraints.validations.SimpleValidation
           org.jamesframework.core.problems.constraints.validations.PenalizingValidation
           org.jamesframework.core.problems.constraints.validations.SimplePenalizingValidation))
             
           

(defprotocol Objective
  (evaluate [this solution data] "Return number or Evaluation")
  (minimizing? [this]))

(defprotocol ObjectiveDelta
  "Optionally implement this protocol if there is a more efficient way to evaluate one solution in terms of another"
  (evaluate-delta [this move cur-solution cur-evaluation data]))

(defprotocol RandomSolutionGenerator
  (create [this data]))
   
;; (defprotocol Evaluation
;;   (value [evaluation] "Return number"))

;; (definterface Evaluation
;;   (^double value [evaluation]))

(defprotocol Constraint
  (validate [constraint solution data] "Return Validation, move arity is optional"))

(defprotocol ConstraintDelta
  "Optionally implement this protocol if there is a more efficient way to evaluate one constraint in terms of another"
  (validate-delta [constraint move cur-solution cur-validation data]))

;; (definterface Validation
;;   (^boolean passed? [validation]))

;; (definterface PenalizingValidation
;;   (^double penalty [validation] "Returns 0.0 if validation passes, or positive penalty"))

(defprotocol Move
  (apply-move [move solution] "Return Solution"))

(defprotocol Neighborhood
  (random-move [neighborhood solution])
  (all-moves [neighborhood solution]))

(defprotocol StopCriterion
  (search-should-stop? [this search]))

;; (defprotocol SearchStrategy
;;   (init [this])
;;   (start [this])
;;   (stop [this])
;;   (search-started [this])
;;   (search-stopped [this])
;;   ;; The following two aren't implemented in the "base classes" only in overrides
;;   (search-step [this])
;;   (search-disposed [this]))

(defprotocol SearchStrategy
  (search-step [strategy search]))

(defprotocol EvaluatedMoveCache
  (cache-move-evaluation [cache move evaluation])
  (get-cached-move-evaluation [cache move])
  (cache-move-validation [cache move validation])
  (get-cached-move-validation [cache move])
  (clear [cache]))

;; Default implementations

(extend-type Object
  ObjectiveDelta
  (evaluate-delta [this move cur-solution cur-evaluation data]
    (evaluate this (apply-move move cur-solution) data))
  ConstraintDelta
  (validate-delta [this move cur-solution cur-validation data]
    (validate this (apply-move move cur-solution) data)))

;; (extend-protocol Evaluation
;;   Double (value [this] this)
;;   Long (value [this] this))

;; (extend-protocol Validation
;;   Boolean (passed? [this] this)
;;   nil (passed? [this] false)
;;   Double (passed? [this] (= this 0.0))
;;   Long (passed? [this] (= this 0))
;;   mister_rogers.protocols.PenalizingValidation (passed? [this] (<= (penalty this) 0.0)))

;; (extend-protocol PenalizingValidation
;;   Double (penalty [this] this)
;;   Long (penalty [this] this))

(defn value ^double [evaluation]
  (if (number? evaluation) evaluation (.getValue ^Evaluation evaluation)))

(defn passed? [validation]
  (cond
    (boolean? validation) validation
    (nil? validation) false
    (number? validation) (zero? validation)
    :else (.passed ^Validation validation)))

(defn penalty ^double [validation]
  (cond
    (number? validation) (zero? validation)
    :else (.penalty ^PenalizingValidation validation)))
