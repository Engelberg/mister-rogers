(ns mister-rogers.problem
  (:refer-clojure :exclude [cond])
  (:require [mister-rogers.protocols :as mrp]
            [clojure.data.generators :as gen]
            [better-cond.core :refer [cond defnc]])
  (:import java.util.concurrent.ThreadLocalRandom))

(defrecord Problem [data objective solution-generator mandatory-constraints penalizing-constraints])

(defrecord PenalizedEvaluation [evaluation penalizing-validations minimizing?]
  ;; penalizing-validations is a map from constraints to validations
  mrp/Evaluation
  (value [this]
    (let [p (transduce (map mrp/penalty) + (vals penalizing-validations))]
      (if minimizing?
        (+ (mrp/value evaluation) p)
        (- (mrp/value evaluation) p)))))

(defnc evaluate [{:keys [objective data penalizing-constraints]} solution]
  :let [evaluation (mrp/evaluate objective solution data)]
  (empty? penalizing-constraints) evaluation
  :else (->PenalizedEvaluation
         evaluation
         (into {}
               (for [constraint penalizing-constraints]
                 [constraint (mrp/validate constraint solution data)]))
         (mrp/minimizing? objective)))

(defnc evaluate-delta [{:keys [objective data penalizing-constraints]}
                       move cur-solution cur-evaluation]
  (empty? penalizing-constraints)
  (mrp/evaluate-delta objective move cur-solution cur-evaluation data),
  ;; extract components and perform deltas
  :let [{:keys [evaluation penalizing-validations minimizing?]} cur-evaluation
        new-evaluation (mrp/evaluate-delta objective move cur-solution evaluation data)]
  (->PenalizedEvaluation
   new-evaluation
   (into {}
         (for [constraint penalizing-constraints
               :let [validation (get penalizing-validations constraint)]]
           [constraint (mrp/validate-delta constraint move cur-solution validation data)]))
   minimizing?))

(defn minimizing? [{:keys [objective]}]
  (mrp/minimizing? objective))

(defrecord UnanimousValidation [validations]
  mrp/Validation
  (passed? [this] (every? mrp/passed? validations)))

(defn- unchunk "Unchunk a chunked lazy sequence" [s]
  (lazy-seq
   (when (seq s)
     (cons (first s) (unchunk (rest s))))))

(defnc validate [{mc :mandatory-constraints, data :data} solution]
  :let [num-mc (count mc)]
  (= num-mc 0) true
  (= num-mc 1) (mrp/validate (nth mc 0) solution data)
  :else (->UnanimousValidation
         (for [constraint (unchunk mc)]  ;; unchunk for short-circuiting behavior
           (mrp/validate constraint solution data))))

(defnc validate-delta [{mc :mandatory-constraints, data :data}
                       move cur-solution cur-validation]
  :let [num-mc (count mc)]
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

