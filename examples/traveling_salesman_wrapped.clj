(ns traveling-salesman-wrapped
  (:refer-clojure :exclude [cond])
  (:require [better-cond.core :refer [cond defnc]]
            [mister-rogers.protocols :as mrp]
            [mister-rogers.core :as mr]
            [primitive-math :as pm]
            [mister-rogers.data.generators :as gen]
            [clojure.data.generators :as cgen]
            [clojure.tools.reader.edn :as edn]))

;; traveling salesman data is given as triangular half of a matrix
(defn triangle-indices [n]
  (for [i (range n), j (range i)] [i j]))

(deftype TSPData [^long num-cities distances])

(defnc build-matrix [n distance-map]
  :let [m (make-array Double/TYPE n n)]
  :do (doseq [i (range n) j (range n)]
        (aset2 m i j (cond
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

(def tsp1 (read-file "examples/data/tsp1.txt"))
(def tsp2 (read-file "examples/data/tsp2.txt"))
(def tsp3 (read-file "examples/data/tsp3.txt"))
(def tsp4 (read-file "examples/data/tsp4.txt"))

(defn get-distance ^double [distances i j]
  (mr/aget2 distances i j))


;; A TSP Solution is a vector that is a permutation of (range num-cities). n is count

(defrecord TSPSolution [n tour])

(defn evaluate ^double [^TSPSolution solution ^TSPData data]
  (let [distances (.distances data), n (.num-cities data),        
        decn (dec n), solution (.tour solution)]
    (+ (get-distance distances (nth solution 0) (nth solution decn))
       (loop [i 0 total 0.0]
         (cond
           (= i decn) total
           (recur (inc i) (+ total (mr/aget2 distances (nth solution i)
                                           (nth solution (inc i))))))))))

;; Random solution generator to kick things off
;; It is crucial to use the random functions in clojure.data.generators ns

(defn generate-solution [^TSPData data]
  (TSPSolution. (.num-cities data)
                (vec (gen/shuffle (range (.num-cities data))))))

;; A move is two indices, i and j, and we will reverse the subsequence
;; from solution[i] through solution[j] (inclusive)
;; If i>j, that indicates we need to wrap around to do reversal
;; This is known as a 2-opt Move

(declare apply-move)
(deftype TSP-2-Opt-Move [^long i ^long j]
  mrp/Move
  (apply-move [this solution] (apply-move this solution)))

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

;; We can do a more efficient delta evaluation

(defn evaluate-delta ^double [^TSP-2-Opt-Move move ^TSPSolution cur-solution
                              ^double cur-evaluation ^TSPData data]
  (cond
    :let [i (.i move), j (.j move),
          distances (.distances data), n (count (.tour cur-solution))]
    ;; Special case when whole trip is reversed
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
                                      (mrp/value (:evaluation (mr/get-best search))))))})

(defnc traveling-salesman [data time-limit]
  :let [search (mr/random-descent-search
                {:name "Traveling Salesman"
                 :evaluate evaluate,
                 :evaluate-delta evaluate-delta,
                 :minimizing? true,
                 :solution-generator generate-solution,
                 :neighborhood TSP-2-Opt-Neighborhood,
                 :stop-criteria [(mr/max-runtime time-limit :seconds)]
                 :search-listeners [progress-listener],
                 :data data})]
  (mr/start search))

