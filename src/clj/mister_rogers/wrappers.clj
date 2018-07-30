(ns mister-rogers.wrappers
  (:refer-clojure :exclude [cond])
  (:require [mister-rogers.protocols :as mrp]
            [better-cond.core :refer [cond defnc]]
            [clojure.data.generators :refer [*rnd*]])
  (:import org.jamesframework.core.problems.objectives.Objective
           org.jamesframework.core.problems.objectives.evaluations.Evaluation
           org.jamesframework.core.problems.constraints.Constraint
           org.jamesframework.core.problems.constraints.validations.Validation
           org.jamesframework.core.problems.constraints.validations.PenalizingValidation
           org.jamesframework.core.problems.sol.Solution
           org.jamesframework.core.search.neigh.Neighbourhood
           org.jamesframework.core.search.neigh.Move))

(definterface CljSolution (unwrapSolution []) (getAtom []))
(defn unwrap-solution [o] (.unwrapSolution o))

(defrecord WrapEvaluation [evaluation]
  Evaluation
  (getValue [this] (mrp/value evaluation)))

(defrecord WrapObjective [objective]
  Objective
  (isMinimizing [this] (mrp/minimizing? objective))
  (evaluate [this solution data]
    (->WrapEvaluation (mrp/evaluate objective (unwrap-solution solution) data)))
  (evaluate [this move curSolution curEvaluation data]
    (->WrapEvaluation
     (mrp/evaluate-delta objective (:move move) (unwrap-solution curSolution)
                         (:evaluation curEvaluation) data))))

(defrecord WrapValidation [validation]
  Validation
  (passed [this] (mrp/passed? validation))
  PenalizingValidation
  (getPenalty [this] (mrp/penalty validation)))

(defrecord WrapConstraint [constraint]
  Constraint
  (validate [this solution data]
    (->WrapValidation (mrp/validate constraint (unwrap-solution solution) data)))
  (validate [this move curSolution curValidation data]
    (->WrapValidation
     (mrp/validate-delta constraint (:move move) (unwrap-solution curSolution)
                         (:validation curValidation) data))))

(defn wrap-solution [o]
  (let [sol (atom o)]
    (proxy [Solution CljSolution] []
      (copy [] (wrap-solution @sol))
      (equals [other] (= @sol (.unwrapSolution other)))
      (hashCode [] (hash @sol))
      (unwrapSolution [] @sol)
      (getAtom [] sol))))

(defrecord WrapNeighborhood [neighborhood]
  Neighbourhood
  (getRandomMove [this solution rnd]
    (binding [*rnd* rnd]
      (mrp/random-move neighborhood (unwrap-solution solution))))
  (getAllMoves [this solution]
    (mrp/all-moves neighborhood (unwrap-solution solution))))

(defrecord WrapMove [move prior-state]
  Move
  (apply [this solution]
    (let [a (.getAtom solution)]
      (reset! prior-state @a)
      (swap! a #(mrp/apply-move move %))))
  (undo [this solution]
    (let [a (.getAtom solution)]
      (reset! a @prior-state)
      (reset! prior-state nil))))

(defn wrap-move [move]
  (->WrapMove move (atom nil)))



(definterface Interface (test []))
(def p (proxy [Object Interface] [] (test [] 1)))
(defn get-test [o] (.test ^Interface o))
