(ns mister-rogers.problem
  (:refer-clojure :exclude [cond])
  (:require [mister-rogers.protocols :as mrp]
            [clojure.data.generators :as gen]
            [better-cond.core :refer [cond defnc]]
            [medley.core :refer [map-entry map-kv]])
  (:import java.util.concurrent.ThreadLocalRandom))

(defrecord Problem [data objective solution-generator mandatory-constraints penalizing-constraints])

(defn ->Problem
  ([data objective solution-generator]
   (Problem. data objective solution-generator [] []))
  ([data objective solution-generator mandatory-constraints]
   (Problem. data objective solution-generator mandatory-constraints []))
  ([data objective solution-generator mandatory-constraints penalizing-constraints]
   (Problem. data objective solution-generator mandatory-constraints penalizing-constraints)))

(defrecord PenalizedEvaluation [evaluation penalizing-validations minimizing?]
  ;; penalizing-validations is a map from constraints to validations
  mrp/Evaluation
  (value [this]
    (let [p (transduce (map mrp/penalty) + (vals penalizing-validations))]
      (if minimizing?
        (+ (mrp/value evaluation) p)
        (- (mrp/value evaluation) p)))))

(defnc evaluate [^Problem problem solution]
  :let [objective (.-objective problem),
        data (.-data problem),
        penalizing-constraints (.-penalizing-constraints problem),
        evaluation (mrp/evaluate objective solution data)]
  (empty? penalizing-constraints) evaluation
  :else (->PenalizedEvaluation
         evaluation
         (into {} (map (fn [constraint]
                         (map-entry constraint (mrp/validate constraint solution data))))
               penalizing-constraints)
         (mrp/minimizing? objective)))

(defnc evaluate-delta [^Problem problem move cur-solution cur-evaluation]
  :let [objective (.-objective problem),
        data (.-data problem),
        penalizing-constraints (.-penalizing-constraints problem)]
  (empty? penalizing-constraints)
  (mrp/evaluate-delta objective move cur-solution cur-evaluation data),
  ;; extract components and perform deltas
  :let [{:keys [evaluation penalizing-validations minimizing?]} cur-evaluation
        new-evaluation (mrp/evaluate-delta objective move cur-solution evaluation data)]
  (->PenalizedEvaluation
   new-evaluation
   (map-kv (fn [constraint validation]
             (map-entry constraint
                        (mrp/validate-delta constraint move cur-solution validation data)))
           penalizing-validations)
   minimizing?))

(defn minimizing? [^Problem problem]
  (mrp/minimizing? (.-objective problem)))

(defrecord UnanimousValidation [validations]
  mrp/Validation
  (passed? [this] (every? mrp/passed? validations)))

(defn- unchunk "Unchunk a chunked lazy sequence" [s]
  (lazy-seq
   (when (seq s)
     (cons (first s) (unchunk (rest s))))))

(defnc validate [^Problem problem solution]  
  :let [mc (.-mandatory-constraints problem)
        data (.-data problem)
        num-mc (count mc)]
  (= num-mc 0) true
  (= num-mc 1) (mrp/validate (nth mc 0) solution data)
  :else (->UnanimousValidation
         (for [constraint (unchunk mc)]  ;; unchunk for short-circuiting behavior
           (mrp/validate constraint solution data))))

(defnc validate-delta [^Problem problem move cur-solution cur-validation]
  :let [mc (.-mandatory-constraints problem)
        data (.-data problem)
        num-mc (count mc)]
  (= num-mc 0) true
  (= num-mc 1) (mrp/validate-delta
                (nth mc 0) move cur-solution cur-validation data)
  :else (->UnanimousValidation
         (for [constraint (unchunk mc)]  ;; unchunk for short-circuiting behavior
           (mrp/validate-delta constraint move cur-solution cur-validation data))))

(defn create-random-solution [{:keys [solution-generator data]}]
  (binding [gen/*rnd* (ThreadLocalRandom/current)]
    (solution-generator data)))

(defn violated-constraints
  [{mc :mandatory-constraints, pc :penalizing-constraints, data :data} solution]
  (for [constraint (concat mc pc)
        :when (not (mrp/passed? (mrp/validate constraint solution data)))]
    constraint))

