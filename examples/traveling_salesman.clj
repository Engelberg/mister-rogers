(ns traveling-salesman
  (:refer-clojure :exclude [cond])
  (:require [better-cond.core :refer [cond defnc]]
            [mister-rogers.protocols :as mrp]
            [mister-rogers.core :as mr]
            [primitive-math :as pm]
            [mister-rogers.data.generators :as gen]
            [clojure.data.generators :as cgen]
            [clojure.tools.reader.edn :as edn]))

;; To create a local neighborhood optimization search, we need to
;; provide several pieces to the underlying framework.
;; We'll need to provide a function to evaluate candidate 
;; solutions, usually based on some information from a dataset.
;; We need to say whether we are trying to
;; minimize or maximize that evaluation; we need a function to
;; generate an initial random solution; we need to define
;; what kind of "moves" or alterations we want to do to a
;; solution to get another candidate solution.
;; Then, the framework will use the pieces you provided to
;; generate a start solution and try lots
;; of random moves according to some strategy, trying to optimize.
;;
;; Optionally, you can provide a more efficient way of evaluating
;; solutions based on the "delta" from the previous solution; 
;; you can provide hard constraints or penalizing constraints;
;; you can establish a stop criterion for the search, and set up
;; callback functions to be invoked at certain points in the search.
;; In this initial example, we will not be discussing constraints,
;; but many of the other features are covered here.

;; The first step is reading the data file, and turning it into a data structure

;; Traveling salesman data is given as triangular half of a matrix
;; We would rather have it as a full matrix
;; Either deftype or defrecord would work just fine to contain our matrix.
;; I tend to prefer deftype here, which forces me to use the (slightly)
;; higher-performance field accessors rather than keywords.

(deftype TSPData [^long num-cities distances])

;; The data text file only contains entries for i,j where i>j

(defn triangle-indices
  "Builds a seq of all [i j] entries where i>j" [n]
  (for [i (range n), j (range i)] [i j]))

;; For the matrix, we use a 2d array of doubles, using some helper
;; macros (aget2 and aset2) from the mister-rogers.core namespace.
;; Be careful though -- using arrays without the proper type hints
;; will give performance that is far, far worse than just using
;; plain Clojure data structures like a vector of vectors, or a
;; map from [i j] indices to values. Don't use arrays unless you
;; know what you are doing. clojure.core.matrix with the :vectorz
;; implementation is an excellent compromise between performance
;; (very close to arrays) and ease of use (can't screw it up by
;; missing a type hint). I recommend clojure.core.matrix with
;; mget and mset! to manipulate entries in your matrix, if you
;; don't want to mess with arrays.  A library called hiphip is
;; also helpful for working with 1d primitive arrays.

(defnc build-matrix
  "Takes dimension of matrix (n) and a distance-map with entries from
[i j] where i>j to Doubles, and returns a full 2d matrix of doubles"
  [n distance-map]
  :let [m (make-array Double/TYPE n n)]
  :do (doseq [i (range n) j (range n)]
        (mr/aset2 m i j (cond
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

;; Read in the four sample files

(def tsp1 (read-file "examples/data/tsp1.txt"))
(def tsp2 (read-file "examples/data/tsp2.txt"))
(def tsp3 (read-file "examples/data/tsp3.txt"))
(def tsp4 (read-file "examples/data/tsp4.txt"))

;; Let's give our distance fn a name that is more communicative than aget2

(defmacro get-distance [distances i j]
  `(mr/aget2 ~distances ~i ~j))

;; A TSP Solution is a vector that is a permutation of (range num-cities).
;; So, for example, [1 4 2 3 0] tells us to visit 1, 4, 2, 3, 0,
;; and then back to 1 to finish out our tour of all the cities.

;; evaluate is only going to be executed once at the beginning of the search,
;; to evaluate the intial, randomly-generated solution. After that we will
;; be using evaluate-delta which can do a faster computation by looking
;; only at the changes as we move from one candidate solution to another.
;; So this doesn't have to be super-fast, and we can feel free to use
;; higher-order functions like partition and transduce if so desired.

(defn evaluate 
  "solution tells us what order we are visiting the cities, and
  data tells us the distances. We return the total trip distance
  (returning to our start city at the end of our tour)"
  ^double [solution ^TSPData data]
  (let [distances (.distances data)]
    (transduce (map (fn [[i j]] (get-distance distances i j)))
               + (partition 2 1 (conj solution (solution 0))))))

;; Random solution generator to kick things off
;; It is recommended to use the random functions in
;; mister-rogers.data.generators which has an API that
;; exactly parallels clojure.data.generators, but uses
;; (ThreadLocalRandom/current) as the source of randomness,
;; for better multithreaded behavior (important for some of
;; the search stratgies).

(defn generate-solution [^TSPData data]
  (vec (gen/shuffle (range (.num-cities data)))))

;; A move is two indices, i and j, and we will reverse the subsequence
;; from solution[i] through solution[j] (inclusive)
;; If i>j, that indicates we need to wrap around to do reversal
;; This is known as a 2-opt Move
;; Moves must implement the Move protocol in mister-rogers.protocols
;; which simply involves implementing apply-move.
;; You can use deftype or defrecord, whatever you prefer.
;; I tend to prefer deftype here, which forces me to use the (slightly)
;; higher-performance field accessors rather than keywords.

(declare apply-move)
(deftype TSP-2-Opt-Move [^long i ^long j]
  mrp/Move
  (apply-move [this solution] (apply-move this solution)))

;; This is one of our performance-sensitive functions
;; that will be called millions of times per second.
;; mister-rogers.core provides a count macro that
;; is faster on Counted collections, like vectors.
;; Using this version of count makes the code twice as fast!

(defnc apply-move [^TSP-2-Opt-Move move solution]
  :let [n (mr/count solution)
        i (.i move)
        j (.j move)]
  (< i j) (into [] (concat (subvec solution 0 i)
                           (rseq (subvec solution i (inc j)))
                           (subvec solution (inc j) n)))
  :let [reversed-section (concat (rseq (subvec solution 0 (inc j)))
                                 (rseq (subvec solution i n)))]
  :else (into [] (concat (drop (- n i) reversed-section)
                         (subvec solution (inc j) i)
                         (take (- n i) reversed-section))))

;; We can do a more efficient delta evaluation.  When we provide
;; this function, this will be called on every solution after the
;; first one, instead of evaluate.  That means this is a
;; performance-critical function, and we can benefit from
;; accessing our fields directly and using primitive math.
;; Also, mister-rogers.core provides a version of nth that is
;; faster for Clojure's Indexed collections, like vectors.

(defn evaluate-delta ^double [^TSP-2-Opt-Move move cur-solution
                              ^double cur-evaluation ^TSPData data]
  (cond
    :let [i (.i move), j (.j move), 
          distances (.distances data), n (.num-cities data)]
    ;; Special case when whole trip is reversed
    (or (== (rem (inc j) n) i) (== (rem (+ 2 j) n) i)) cur-evaluation
    :let [;; Get crucial cities
          before-reversed (mr/nth cur-solution (rem (+ n (dec i)) n))
          first-reversed (mr/nth cur-solution i)
          last-reversed (mr/nth cur-solution j)
          after-reversed (mr/nth cur-solution (rem (inc j) n))]
    ;; Two distances are dropped by the reversal, and two are added
    :let [total (- (double cur-evaluation)
                   (get-distance distances before-reversed first-reversed))
          total (- total
                   (get-distance distances last-reversed after-reversed))
          total (+ total
                   (get-distance distances before-reversed last-reversed))
          total (+ total                    
                   (get-distance distances first-reversed after-reversed))]
    total))

;; Now we create a "neighborhood" of these possible moves by
;; implementing the Neighborhood protocol which has two functions.
;; Most search strategies use either one function or the other, but
;; not both. But we provide both implementations here for completeness.

(defnc random-move [solution]
  :let [n (mr/count solution),
        i (gen/uniform 0 n)
        j (gen/uniform 0 (dec n))
        j (if (>= j i) (inc j) j)]
  (TSP-2-Opt-Move. i j))

(defnc all-moves [solution]
  :let [n (mr/count solution)]
  (for [i (range n), j (range n)
        :when (not= i j)]
    (TSP-2-Opt-Move. i j)))

(def TSP-2-Opt-Neighborhood
  (reify mrp/Neighborhood
    (random-move [this solution] (random-move solution))
    (all-moves [this solution] (all-moves solution))))

;; RANDOM DESCENT

;; A search is a stateful object, provided by the underlying JAMES framework

;; During the search, let's print out the value of every new solution
;; There are several possible callbacks we can implement in our listener.
;; The three below are probably the most common.
;; A mister-rogers.core function called search-state can give us
;; a map with a lot of useful information about the state of the search.
;; There are also some helper functions like get-best-solution-evaluation
;; that return that information directly from the search.

(def progress-listener
  {:search-started (fn [search] (println " >>> Search started"))
   :search-stopped (fn [search]
                     (let [search-state (mr/search-state search)]
                       (println (str " >>> Search stopped ("
                                     (/ (:runtime search-state) 1000.0)
                                     " sec, "
                                     (:steps search-state)
                                     " steps)"))))
   :new-best-solution (fn [search newBestSolution newBestSolutionEvaluation newBestSolutionValidation]
                        (println (str "New best solution: "
                                      (mr/get-best-solution-evaluation search))))})

(defn traveling-salesman-random-descent [data time-limit]
  (mr/random-descent-search
   {:name "Traveling Salesman"
    :evaluate evaluate,
    :evaluate-delta evaluate-delta,
    :minimizing? true,
    :solution-generator generate-solution,
    :neighborhood TSP-2-Opt-Neighborhood,
    :stop-criteria [(mr/max-runtime time-limit :seconds)]
    :search-listeners [progress-listener],
    :data data}))

;; This just creates a stateful search object, but it doesn't start it yet.
;; There are three main functions for interacting with the search:
;; mr/search-state, mr/start and mr/stop
;; All three return the search-state, a snapshot of the current state of
;; search. However, mr/search-state does this without interrupting the
;; search in any way, whereas mr/start and mr/stop start and stop the
;; search, respectively.  The rest is up to you.

;; Here are some possible ways to interact with the search at the REPL

;; Let's set up a search for the first data set, for 10 seconds

;; > (def search (traveling-salesman-random-descent tsp1 10))
;; > (mr/start search)

;; The search runs for 10 seconds, our listener will show
;; the progress, and the search-state will be returned at the end
;; of the search.  The search-state is a Clojure map containing
;; information such as the best solution so far, and its evaluation.

;; Most search types will let you re-run the search once it has
;; completed and its status has been set to :idle.
;; If you want to run it for another 10 seconds, you can restart it with
;; > (mr/start search)
;; which will continue from the best solution it has found, but
;; restarts the time counter and other per-run statistics.

;; But let's say you know you want to run it for a minute or longer,
;; and you'd like to be able to interact with the search while it
;; is running.  That's easy to do with Clojure's `future`.

;; > (def search (traveling-salesman-random-descent tsp1 60))
;; > (def result (future (mr/start search)))

;; This kicks off the search on another thread, so you are free to
;; continue interacting at the REPL.

;; > (mr/search-state search)
;; will return the current state of the search, in progress, without
;; interrupting it.

;; > (mr/stop search)
;; will stop the search prematurely and return the current search-state

;; Even though the search is running on a separate thread, our search
;; listener will print a message to our REPL when it is complete.
;; At that point, the search-state will also be deposited
;; into the future object, since it is returned by the original call to
;; mr/search-start.  So you can retrieve the search-state with @result.

;; Alternatively, you may prefer to combine the creation and start
;; of the search into one function:

(defn solve-tsp [data time-limit]
  (mr/start (traveling-salesman-random-descent data time-limit)))

;; > (solve-tsp tsp1 10)

;; If you Ctrl-C a search rather than using mr/stop, the search
;; won't have the opportunity to "clean up" and will be left in
;; an odd state. You'll be able to inspect the search-state of the
;; search and thus extract the best solution it has found so far,
;; but you won't be able to restart the search. And the stop
;; criterion checker, which always runs in a separate thread,
;; will continue to poll the search once per second, uselessly,
;; until the time limit is up.

;; So, use of Ctrl-C to interrupt a search is not recommended,
;; although of course you can do it in a pinch, if you need to.
