(ns mister-rogers.wrappers
  (:refer-clojure :exclude [cond])
  (:require [mister-rogers.protocols :as mrp]
            [mister-rogers.problem :as prob]
            [better-cond.core :refer [cond defnc]]
            [clojure.data.generators :refer [*rnd*]])
  (:import org.jamesframework.core.problems.objectives.Objective
           org.jamesframework.core.problems.objectives.evaluations.Evaluation
           org.jamesframework.core.problems.constraints.Constraint
           org.jamesframework.core.problems.constraints.validations.Validation
           org.jamesframework.core.problems.constraints.validations.PenalizingValidation
           org.jamesframework.core.search.neigh.Neighbourhood
           org.jamesframework.core.search.neigh.Move
           io.github.engelberg.mister_rogers.Solution
           ))

(defn wrap-solution [o]
  (Solution. o))

(defnc unwrap-solution [s]
;;  (instance? org.jamesframework.core.subset.SubsetSolution s) s
  (.o ^Solution s))

(defrecord WrapEvaluation [evaluation]
  Evaluation
  (getValue [this] (mrp/value evaluation)))

(defrecord WrapObjective [objective]
  Objective
  (isMinimizing [this] (boolean (mrp/minimizing? objective)))
  (evaluate [this solution data]
    (->WrapEvaluation (mrp/evaluate objective (unwrap-solution solution) data)))
  (evaluate [this move curSolution curEvaluation data]
    (->WrapEvaluation
     (mrp/evaluate-delta objective (:move move) (unwrap-solution curSolution)
                         (:evaluation curEvaluation) data))))

(defrecord WrapValidation [validation]
  Validation
  (passed [this] (boolean (mrp/passed? validation)))
  PenalizingValidation
  (getPenalty [this] (double (mrp/penalty validation))))

(defrecord WrapConstraint [constraint]
  Constraint
  (validate [this solution data]
    (->WrapValidation (mrp/validate constraint (unwrap-solution solution) data)))
  (validate [this move curSolution curValidation data]
    (->WrapValidation
     (mrp/validate-delta constraint (:move move) (unwrap-solution curSolution)
                         (:validation curValidation) data))))

(defrecord WrapMove [move prior-state]
  Move
  (apply [this solution]
    (let [^Solution solution solution
          o (.o solution)]
      (reset! prior-state o)
      (set! (.o solution) (mrp/apply-move move o))))
  (undo [this solution]
    (let [^Solution solution solution]
      (set! (.o solution) @prior-state)
      (reset! prior-state nil))))

(defn wrap-move [move]
  (->WrapMove move (atom nil)))

(defrecord WrapNeighborhood [neighborhood]
  Neighbourhood
  (getRandomMove [this solution rnd]
    (binding [*rnd* rnd]
      (wrap-move (mrp/random-move neighborhood (unwrap-solution solution)))))
  (getAllMoves [this solution]
    (list* (map wrap-move (mrp/all-moves neighborhood (unwrap-solution solution))))))

(defrecord WrapProblem [problem]
  org.jamesframework.core.problems.Problem
  (evaluate [this solution]
    (->WrapEvaluation (prob/evaluate problem (unwrap-solution solution))))
  (evaluate [this move curSolution curEvaluation]
    (->WrapEvaluation (prob/evaluate-delta problem (:move move)
                                           (unwrap-solution curSolution)
                                           (:evaluation curEvaluation))))
  (validate [this solution]
    (->WrapValidation (prob/validate problem (unwrap-solution solution))))
  (validate [this move curSolution curValidation]
    (->WrapValidation (prob/validate-delta problem (:move move)
                                           (unwrap-solution curSolution)
                                           (:validation curValidation))))
  (isMinimizing [this] (boolean (prob/minimizing? problem)))
  (createRandomSolution [this rnd]
    (binding [*rnd* rnd]
      (wrap-solution (prob/create-random-solution problem))))
  (createRandomSolution [this]
    (wrap-solution (prob/create-random-solution problem))))






(definterface Interface (test []))
(def p (proxy [Object Interface] [] (test [] 1)))
(defn get-test [o] (.test ^Interface o))
