(ns traveling-salesman
  (:refer-clojure :exclude [cond])
  (:require [mister-rogers.protocols :as mrp]
            [mister-rogers.problem :as prob]
            [mister-rogers.wrappers :as w]
            [clojure.core.matrix :as m]            
            [better-cond.core :refer [cond defnc]]
            [primitive-math :as pm]
            [clojure.data.generators :as gen :refer [*rnd*]]
            [clojure.tools.reader.edn :as edn]
            [net.cgrand.xforms :as x])
  (:import org.jamesframework.core.search.algo.RandomDescent
           org.jamesframework.core.search.Search
           org.jamesframework.core.problems.Problem
           org.jamesframework.core.problems.GenericProblem
           org.jamesframework.core.search.neigh.Neighbourhood           
           org.jamesframework.core.problems.sol.RandomSolutionGenerator
           org.jamesframework.core.search.stopcriteria.MaxRuntime
           org.jamesframework.core.search.stopcriteria.MaxTimeWithoutImprovement
           org.jamesframework.core.search.listeners.SearchListener
           java.util.concurrent.TimeUnit
           org.jamesframework.core.problems.objectives.Objective
           org.jamesframework.core.problems.objectives.evaluations.SimpleEvaluation
           org.jamesframework.core.problems.objectives.evaluations.Evaluation
           org.jamesframework.core.search.algo.ParallelTempering
           org.jamesframework.core.search.neigh.Move
           io.github.engelberg.mister_rogers.Solution
           java.util.concurrent.ThreadLocalRandom
           ))

;; traveling salesman data is given as triangular half of a matrix
(defn triangle-indices [n]
  (for [i (range n), j (range i)] [i j]))

(deftype TSPData [^long num-cities distances])

(defnc build-matrix [n distance-map]
  :let [m (m/zero-matrix :vectorz n n)]
  :do (doseq [i (range n) j (range n)]
        (m/mset! m i j (cond
                         (> i j) (distance-map [i j])
                         (< i j) (distance-map [j i])
                         :else 0)))
  m)

(defnc read-file [filename]
  :let [s (str \[ (slurp filename), \])
        data (edn/read-string s)
        n (first data)
        distance-map (into {} (map vector (triangle-indices n) (rest data)))]
  (TSPData. n (build-matrix n distance-map)))

(defn get-distance ^double [distances i j]
  (m/mget distances i j))

(defrecord TSPSolution [n tour])
;; A TSP Solution is a vector that is a permutation of (range num-cities). n is count
;; We need to define an Objective, with a way to evaluate a given solution

(defn evaluate ^double [solution ^TSPData data]
  (let [distances (.distances data), n (.num-cities data),        
        decn (dec n), ^TSPSolution solution (w/unwrap-solution solution),
        solution (.tour solution)]
    (+ (get-distance distances (nth solution 0) (nth solution decn))
       (loop [i 0 total 0.0]
         (cond
           (= i decn) total
           (recur (inc i) (+ total (m/mget distances (nth solution i)
                                           (nth solution (inc i))))))))))

(declare evaluate-delta)
(def TSPObjective
  (reify
    Objective
    (evaluate [this solution data] (SimpleEvaluation/WITH_VALUE
                                    (evaluate solution data)))
    (isMinimizing [this] true)
    (evaluate [this move cur-solution cur-evaluation data]
      (SimpleEvaluation/WITH_VALUE
       (evaluate-delta move cur-solution cur-evaluation data)))))

;; Random solution generator to kick things off
;; It is crucial to use the random functions in clojure.data.generators ns

(defn generate-solution [^TSPData data]
  (w/wrap-solution (TSPSolution. (.num-cities data)
                                 (vec (gen/shuffle (range (.num-cities data)))))))

(def random-solution-generator
  (reify RandomSolutionGenerator
    (create [this r d] (generate-solution d))))

;; Now we can create a problem that puts these elements together

(defn tsp-problem [filename]
  (GenericProblem. (read-file filename) TSPObjective random-solution-generator))

(def tsp1 (tsp-problem "examples/data/tsp1.txt"))

;; A move is two indices, i and j, and we will reverse the subsequence
;; from solution[i] through solution[j] (inclusive)
;; If i>j, that indicates we need to wrap around to do reversal
;; This is known as a 2-opt Move

(declare apply-move)
(deftype TSP-2-Opt-Move [^long i ^long j]
  Move
  (apply [this solution]
    (let [^Solution solution solution
          o (.o solution)]
      (set! (.undo solution) o)
      (set! (.o solution) (apply-move this o))))
  (undo [this solution]
    (let [^Solution solution solution]
      (set! (.o solution) (.undo solution))
      (set! (.undo solution) nil))))

(defnc apply-move [^TSP-2-Opt-Move move ^TSPSolution solution]
  :let [n (.n solution)
        solution (.tour solution)
        i (.i move)
        j (.j move)]
  (< i j) (TSPSolution. n (into [] (concat (subvec solution 0 i)
                                           (rseq (subvec solution i (inc j)))
                                           (subvec solution (inc j) n))))
  :let [reversed-section (concat (rseq (subvec solution 0 (inc j)))
                                 (rseq (subvec solution i n)))]
  :else (TSPSolution. n (into [] (concat (drop (- n i) reversed-section)
                                         (subvec solution (inc j) i)
                                         (take (- n i) reversed-section)))))

;; Now we create a neighborhood of these possible moves

(defnc random-move [solution]
  :let [^TSPSolution solution (w/unwrap-solution solution)
        n (.n solution),
        i (gen/uniform 0 n)
        j (gen/uniform 0 (dec n))
        j (if (>= j i) (inc j) j)]
  (TSP-2-Opt-Move. i j))

(defnc all-moves [solution]
  :let [^TSPSolution solution (w/unwrap-solution solution)
        n (.n solution)]
  (for [i (range n), j (range n)
        :when (not= i j)]
    (TSP-2-Opt-Move. i j)))

(def TSP-2-Opt-Neighborhood
  (reify Neighbourhood
    (getRandomMove [this solution r]
      (random-move solution))
    (getAllMoves [this solution] (all-moves solution))))

;; We can do a more efficient delta evaluation

(defn evaluate-delta ^double [^TSP-2-Opt-Move move ^Solution cur-solution
                              ^Evaluation cur-evaluation ^TSPData data]
  ;; Special case when whole trip is reversed
  (cond
    :let [i (.i move), j (.j move),
          distances (.distances data), n (.num-cities data)]
    (or (= (mod (inc j) n) i) (= (mod (+ 2 j) n) i)
        (= (mod (dec i) n) j) (= (mod (- i 2) n) j))
    (.getValue cur-evaluation),
    :let [^TSPSolution cur-solution (w/unwrap-solution cur-solution)
          cur-solution (.tour cur-solution),
          cur-total (.getValue cur-evaluation),
          ;; Get crucial cities
          before-reversed (nth cur-solution (mod (dec i) n))
          first-reversed (nth cur-solution i)
          last-reversed (nth cur-solution j)
          after-reversed (nth cur-solution (mod (inc j) n))]
    ;; Two distances are dropped by the reversal, and two are added
    :let [total (pm/- cur-total
                      (get-distance distances before-reversed first-reversed))
          total (pm/- total
                      (get-distance distances last-reversed after-reversed))
          total (pm/+ total
                      (get-distance distances before-reversed last-reversed))
          total (pm/+ total                    
                      (get-distance distances first-reversed after-reversed))]
    total))
  
;; Let's use a RandomDescent search

(defnc solution-info [^Search s]
  :let [sol (.getBestSolution s)
        p (.getProblem s)]
  {:solution (w/unwrap-solution sol)
   :objective (.evaluate p sol)})

(def progress-listener
  (reify SearchListener
    (searchStarted [this search] (println " >>> Search started"))
    (searchStopped [this search]
      (println (str " >>> Search stopped ("
                    (/ (.getRuntime search) 1000)
                    " sec, "
                    (.getSteps search)
                    " steps)")))
    (newBestSolution [this search newBestSolution newBestSolutionEvaluation newBestSolutionValidation]
      (println (str "New best solution: "
                    (.evaluate ^Problem (.getProblem search) newBestSolution))))))


(defnc random-descent-search [problem time-limit]
  :let [search (RandomDescent. problem TSP-2-Opt-Neighborhood)]
  :do (doto search
        (.addSearchListener progress-listener)
        (.addStopCriterion (MaxRuntime. time-limit TimeUnit/SECONDS))
        (.start))
  (solution-info search))
