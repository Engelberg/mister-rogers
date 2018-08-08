(ns traveling-salesman-wrapped
  (:refer-clojure :exclude [cond])
  (:require [mister-rogers.protocols :as mrp]
            [mister-rogers.core :as mr]
            [clojure.core.matrix :as m]            
            [better-cond.core :refer [cond defnc]]
            [primitive-math :as pm]
            [clojure.data.generators :as gen :refer [*rnd*]]
            [clojure.tools.reader.edn :as edn]
            [net.cgrand.xforms :as x]
            [mister-rogers.wrappers :as w]))

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

(declare apply-move)
(deftype TSP-2-Opt-Move [^long i ^long j]
  mrp/Move
  (apply-move [this solution] (apply-move this solution)))

(defrecord TSPSolution [n tour])
;; A TSP Solution is a vector that is a permutation of (range num-cities). n is count
;; We need to define an Objective, with a way to evaluate a given solution

(declare evaluate evaluate-delta)

(def TSPObjective
  (reify
    mrp/Objective
    (evaluate [this solution data]
      (evaluate solution data))
    ;; (let [distances (.distances ^TSPData data), n (.num-cities ^TSPData data)
    ;;       decn (dec n), solution (.tour ^TSPSolution solution)]
    ;;   (+ (get-distance distances (nth solution 0) (nth solution decn))
    ;;      (loop [i 0 total 0.0]
    ;;        (cond
    ;;          (= i decn) total
    ;;          (recur (inc i) (+ total (m/mget distances (nth solution i)
    ;;                                          (nth solution (inc i))))))))))
    mrp/ObjectiveDelta
    (evaluate-delta [this move cur-solution cur-evaluation data]
      (evaluate-delta move cur-solution cur-evaluation data))))
    ;; (cond
    ;;   :let [i (.i ^TSP-2-Opt-Move move), j (.j ^TSP-2-Opt-Move move),
    ;;         distances (.distances ^TSPData data), n (.num-cities ^TSPData data)]
    ;;   (or (= (rem (inc j) n) i) (= (rem (+ 2 j) n) i)
    ;;       (= (rem (+ n (dec i)) n) j) (= (rem (+ n (- i 2))  n) j))
    ;;   cur-evaluation,
    ;;   :let [cur-solution (.tour ^TSPSolution cur-solution),
    ;;         ;; Get crucial cities
    ;;         before-reversed (nth cur-solution (rem (+ n (dec i)) n))
    ;;         first-reversed (nth cur-solution i)
    ;;         last-reversed (nth cur-solution j)
    ;;         after-reversed (nth cur-solution (rem (inc j) n))]
    ;;   ;; Two distances are dropped by the reversal, and two are added
    ;;   :let [total (pm/- (double cur-evaluation)
    ;;                     (get-distance distances before-reversed first-reversed))
    ;;         total (pm/- total
    ;;                     (get-distance distances last-reversed after-reversed))
    ;;         total (pm/+ total
    ;;                     (get-distance distances before-reversed last-reversed))
    ;;         total (pm/+ total                    
    ;;                     (get-distance distances first-reversed after-reversed))]
    ;;   total))))

(defn evaluate ^double [^TSPSolution solution ^TSPData data]
  (let [distances (.distances data), n (.num-cities data),        
        decn (dec n), solution (.tour solution)]
    (+ (get-distance distances (nth solution 0) (nth solution decn))
       (loop [i 0 total 0.0]
         (cond
           (= i decn) total
           (recur (inc i) (+ total (m/mget distances (nth solution i)
                                           (nth solution (inc i))))))))))

;; We can do a more efficient delta evaluation

(defn evaluate-delta ^double [^TSP-2-Opt-Move move ^TSPSolution cur-solution
                              ^double cur-evaluation ^TSPData data]
  ;; Special case when whole trip is reversed
  (cond
    :let [i (.i move), j (.j move),
          distances (.distances data), n (.num-cities data)]
    (or (= (rem (inc j) n) i) (= (rem (+ 2 j) n) i))
    ;;    (= (rem (+ n (dec i)) n) j) (= (rem (+ n (- i 2))  n) j))
    (double cur-evaluation),
    :let [cur-solution (.tour cur-solution),
          ;; Get crucial cities
          before-reversed (nth cur-solution (rem (+ n (dec i)) n))
          first-reversed (nth cur-solution i)
          last-reversed (nth cur-solution j)
          after-reversed (nth cur-solution (rem (inc j) n))]
    ;; Two distances are dropped by the reversal, and two are added
    :let [total (pm/- (double cur-evaluation)
                      (get-distance distances before-reversed first-reversed))
          total (pm/- total
                      (get-distance distances last-reversed after-reversed))
          total (pm/+ total
                      (get-distance distances before-reversed last-reversed))
          total (pm/+ total                    
                      (get-distance distances first-reversed after-reversed))]
    total))

;; Random solution generator to kick things off
;; It is crucial to use the random functions in clojure.data.generators ns

(defn generate-solution [^TSPData data]
  (TSPSolution. (.num-cities data)
                (vec (gen/shuffle (range (.num-cities data))))))

(def solution-generator
  (reify
    mrp/RandomSolutionGenerator
    (create [this data]
      (TSPSolution. (.num-cities ^TSPData data)
                    (vec (gen/shuffle (range (.num-cities ^TSPData data))))))))

;; Now we can create a problem that puts these elements together

(def tsp1 (read-file "examples/data/tsp1.txt"))

;; A move is two indices, i and j, and we will reverse the subsequence
;; from solution[i] through solution[j] (inclusive)
;; If i>j, that indicates we need to wrap around to do reversal
;; This is known as a 2-opt Move

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

;; (defnc apply-move [^TSP-2-Opt-Move move ^TSPSolution solution]
;;   :let [n (.n solution)
;;         solution (.tour solution)
;;         i (.i move)
;;         j (.j move)]
;;   (< i j) (TSPSolution. n (into [] cat [(subvec solution 0 i)
;;                                         (rseq (subvec solution i (inc j)))
;;                                         (subvec solution (inc j) n)]))
;;   :else (TSPSolution. n (into [] cat [(subvec solution (inc j) i)
;;                                       (rseq (subvec solution 0 (inc j)))
;;                                       (rseq (subvec solution i n))])))

;; Now we create a neighborhood of these possible moves

(defnc random-move [^TSPSolution solution]
  :let [n (.n solution),
        i (gen/uniform 0 n)
        j (gen/uniform 0 (dec n))
        j (if (>= j i) (inc j) j)]
  (TSP-2-Opt-Move. i j))

(defnc all-moves [^TSPSolution solution]
  :let [n (.n solution)]
  (for [i (range n), j (range n)
        :when (not= i j)]
    (TSP-2-Opt-Move. i j)))

;; (def TSP-2-Opt-Neighborhood {:random-move random-move :all-moves all-moves})
(def TSP-2-Opt-Neighborhood
  (reify mrp/Neighborhood
    (random-move [this solution] (random-move solution))
    (all-moves [this solution] (all-moves solution))))

;; Let's use a RandomDescent search

(def progress-listener
  {:search-started (fn [search] (println " >>> Search started"))
   :search-stopped (fn [search]
                     (let [search-state (mr/search-state search)]
                       (println (str " >>> Search stopped ("
                                     (/ (:runtime search-state) 1000)
                                     " sec, "
                                     (:steps search-state)
                                     " steps)"))))
   :new-best-solution (fn [search newBestSolution newBestSolutionEvaluation newBestSolutionValidation]
                        (println (str "New best solution: "
                                      (pr-str (mr/get-best search)))))})

(defnc traveling-salesman [data time-limit]
  :let [search (mr/random-descent-search
                {:name "Traveling Salesman"
                 :evaluate evaluate,
                 :evaluate-delta evaluate-delta,
                 ;; :objective TSPObjective
                 :minimizing? true,
                 :solution-generator generate-solution,
                 :neighborhood TSP-2-Opt-Neighborhood,
                 :stop-criteria [(mr/max-runtime time-limit :seconds)]
                 :search-listeners [progress-listener],
                 :data data})]
  (mr/start search))

