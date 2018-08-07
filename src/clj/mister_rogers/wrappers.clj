(ns mister-rogers.wrappers
  (:refer-clojure :exclude [cond])
  (:require [mister-rogers.protocols :as mrp]
            [mister-rogers.problem :as prob]
            [better-cond.core :refer [cond defnc]]
            [clojure.data.generators :refer [*rnd*]])
  (:import org.jamesframework.core.problems.objectives.Objective
           org.jamesframework.core.problems.objectives.evaluations.Evaluation
           org.jamesframework.core.problems.objectives.evaluations.SimpleEvaluation
           org.jamesframework.core.problems.constraints.Constraint
           org.jamesframework.core.problems.constraints.validations.Validation
           org.jamesframework.core.problems.constraints.validations.SimpleValidation
           org.jamesframework.core.problems.constraints.validations.PenalizingValidation
           org.jamesframework.core.problems.constraints.validations.SimplePenalizingValidation
           org.jamesframework.core.search.neigh.Neighbourhood
           org.jamesframework.core.search.neigh.Move
           io.github.engelberg.mister_rogers.Solution
           ))

(defmacro wrap-solution [s]
  `(Solution. ~s))

;; (defmacro unwrap-solution [s]
;;   `(.o ~(with-meta s {:tag Solution})))

(defn unwrap-solution [s]
  (.o ^Solution s))

(defmacro wrap-evaluation [evaluation]
  `(if (number? ~evaluation) (SimpleEvaluation/WITH_VALUE ~evaluation)
       ~evaluation))

;; (defmacro unwrap-evaluation [evaluation]
;;   `(if (instance? SimpleEvaluation ~evaluation) (.getValue ~(with-meta evaluation {:tag Evaluation}))
;;        ~evaluation))

(defn unwrap-evaluation ^double [evaluation]
  (if (instance? SimpleEvaluation evaluation) (.getValue ^Evaluation evaluation)
      evaluation))

;; (defn unwrap-evaluation ^double [evaluation] (mrp/value evaluation))

(deftype WrapMove [move]
  Move
  (apply [this solution]
    (let [^Solution solution solution
          o (.o solution)]
      (set! (.undo solution) o)
      (set! (.o solution) (mrp/apply-move move o))))
  (undo [this solution]
    (let [^Solution solution solution]
      (set! (.o solution) (.undo solution)))))

(defmacro wrap-move [move]
  `(WrapMove. ~move))

;; (defmacro unwrap-move [wrapped-move]
;;   `(.-move ~(with-meta wrapped-move {:tag WrapMove})))

(defn unwrap-move [wrapped-move]
  (.-move ^WrapMove wrapped-move))

;; No longer relevant
(deftype WrapEvaluation [evaluation]
  Evaluation
  (getValue [this] (mrp/value evaluation)))

(deftype WrapValidation [validation]
  Validation
  (passed [this] (if (boolean? validation) validation (mrp/passed? validation)))
  PenalizingValidation
  (getPenalty [this] (double (mrp/penalty validation))))

(defnc wrap-validation [validation]
  (boolean? validation) (SimpleValidation. validation),
  :let [num? (number? validation)]
  (and num? (pos? validation))
  (SimplePenalizingValidation/FAILED validation),
  num? (SimplePenalizingValidation/PASSED),
  :else (WrapValidation. validation))

(defnc unwrap-validation [validation]
  (instance? SimpleValidation validation) (.passed ^SimpleValidation validation),
  (instance? SimplePenalizingValidation validation)
  (.getPenalty ^SimplePenalizingValidation validation),
  :else (.-validation ^WrapValidation validation))

(defrecord WrapConstraint [constraint]
  Constraint
  (validate [this solution data]
    (wrap-validation (mrp/validate constraint (.-o ^Solution solution) data))))

(defrecord WrapConstraintDelta [constraint]
  Constraint
  (validate [this solution data]
    (wrap-validation (mrp/validate constraint (.-o ^Solution solution) data)))
  (validate [this move curSolution curValidation data]
    (wrap-validation
     (mrp/validate-delta constraint (.-move ^WrapMove move)
                         (.-o ^Solution curSolution)
                         (unwrap-validation curValidation) data))))

(defrecord WrapConstraintFn [name validate]
  Constraint
  (validate [this solution data]
    (wrap-validation (validate (.-o ^Solution solution) data))))

(defrecord WrapConstraintDeltaFn [name validate validate-delta]
  Constraint
  (validate [this solution data]
    (wrap-validation (validate (.-o ^Solution solution) data)))
  (validate [this move curSolution curValidation data]
    (wrap-validation
     (validate-delta (.-move ^WrapMove move)
                     (.-o ^Solution curSolution)
                     (unwrap-validation curValidation) data))))

(defnc wrap-constraint [constraint]
  (satisfies? mrp/validate-delta constraint) (WrapConstraintDelta. constraint)
  (satisfies? mrp/validate constraint) (WrapConstraint. constraint)
  :let [{:keys [name validate validate-delta]} constraint]
  validate-delta (WrapConstraintDeltaFn. name validate validate-delta)
  :else (WrapConstraintFn. name validate))

(defnc unwrap-constraint [constraint]
  (instance? WrapConstraint constraint) (:constraint constraint)
  (instance? WrapConstraintDelta constraint) (:constraint constraint)
  :else constraint)
  
(defrecord WrapNeighborhood [neighborhood]
  Neighbourhood
  (getRandomMove [this solution rnd]
    (wrap-move (mrp/random-move neighborhood (.-o ^Solution solution))))
  (getAllMoves [this solution]
    (list* (map ->WrapMove (mrp/all-moves neighborhood (.-o ^Solution solution))))))

(defrecord WrapNeighborhoodFn [random-move all-moves]
  Neighbourhood
  (getRandomMove [this solution rnd]
    (wrap-move (random-move (.-o ^Solution solution))))
  (getAllMoves [this solution]
    (list* (map ->WrapMove (all-moves (.-o ^Solution solution))))))

(defnc wrap-neighborhood [neighborhood]
  (satisfies? mrp/Neighborhood neighborhood) (WrapNeighborhood. neighborhood)
  (map->WrapNeighborhoodFn neighborhood))

(defrecord WrapProblem [problem]
  org.jamesframework.core.problems.Problem
  (evaluate [this solution]
    (WrapEvaluation. (prob/evaluate problem (.-o ^Solution solution))))
  (evaluate [this move curSolution curEvaluation]
    (WrapEvaluation. (prob/evaluate-delta problem (:move move)
                                          (.-o ^Solution curSolution)
                                          (:evaluation curEvaluation))))
  (validate [this solution]
    (->WrapValidation (prob/validate problem (.-o ^Solution solution))))
  (validate [this move curSolution curValidation]
    (->WrapValidation (prob/validate-delta problem (:move move)
                                           (.-o ^Solution curSolution)
                                           (:validation curValidation))))
  (isMinimizing [this] (boolean (prob/minimizing? problem)))
  (createRandomSolution [this rnd]
    (binding [*rnd* rnd]
      (wrap-solution (prob/create-random-solution problem))))
  (createRandomSolution [this]
    (wrap-solution (prob/create-random-solution problem))))

(defrecord WrapObjective [objective]
  Objective
  (isMinimizing [this] (boolean (mrp/minimizing? objective)))
  (evaluate [this solution data]
    (->WrapEvaluation (mrp/evaluate objective (.-o ^Solution solution) data)))
  (evaluate [this move curSolution curEvaluation data]
    (->WrapEvaluation
     (mrp/evaluate-delta objective (:move move) (.-o ^Solution curSolution)
                         (:evaluation curEvaluation) data))))
