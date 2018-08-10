(ns mister-rogers.permutations
  (:refer-clojure :exclude [cond])
  (:require [better-cond.core :refer [cond defnc]]
            [mister-rogers.protocols :as mrp]
            [mister-rogers.core :as mr]))

(defnc swap-position [^clojure.lang.PersistentVector v ^long i ^long j]
  :let [vi (mr/nth v i), vj (mr/nth v j)]
  (.assocN ^clojure.lang.PersistentVector (.assocN v i vj) j vi))

;; Version for transient vectors
(defnc swap-position! [v i j]
  :let [vi (mr/nth v i), vj (mr/nth v j)]
  (assoc! v i vj j vi))

(defnc reverse-subsequence [v ^long i ^long j]
  :let [n (mr/count v)]
  (< i j) (into [] (concat (subvec solution 0 i)
                           (rseq (subvec solution i (inc j)))
                           (subvec solution (inc j) n)))
  (== i j) v  
  :let [reversed-section (concat (rseq (subvec solution 0 (inc j)))
                                 (rseq (subvec solution i n)))]
  :else (into [] (concat (drop (- n i) reversed-section)
                         (subvec solution (inc j) i)
                         (take (- n i) reversed-section))))




