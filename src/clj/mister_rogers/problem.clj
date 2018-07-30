(ns mister-rogers.problem
  (:refer-clojure :exclude [cond])
  (:require [mister-rogers.protocols :as mrp]
            [clojure.data.generators :as gen]
            [better-cond.core :refer [cond defnc]])
  (:import java.util.concurrent.ThreadLocalRandom))

(defrecord Problem [data objective solution-generator mandatory-constraints penalizing-constraints])

(defn evaluate [{:keys [objective data]} solution]
  (mrp/evaluate objective solution data))

(defn evaluate-delta [{:keys [objective data]} move cur-solution cur-evaluation]
  (mrp/evaluate-delta objective move cur-solution cur-evaluation data))

(defn minimizing? [{:keys [objective]}]
  (mrp/minimizing? objective))

(defrecord UnanimousValidation [validations]
  Validation
  (passed? [this] (every? passed? validations)))

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
  (binding [*rnd* (ThreadLocalRandom/current)]
    (solution-generator data)))



