(ns mister-rogers.core
  (:refer-clojure :exclude [cond])
  (:require [better-cond.core :refer [cond defnc defnc-]]
            [medley.core :as medley]
            [com.rpl.specter :as specter
             :refer [ALL NONE ATOM keypath select-any transform setval]]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [mister-rogers.protocols :as mrp]
            [mister-rogers.problem :as prob]
            [mister-rogers.stop-criterion-checker :as check]
            [mister-rogers.criteria :as crit]
            [mister-rogers.wrappers :as w]
            [clojure.data.generators :as gen :refer [*rnd*]]
            [primitive-math :as pm])
  (:import org.jamesframework.core.search.algo.RandomDescent
           org.jamesframework.core.search.Search
           org.jamesframework.core.search.LocalSearch
           org.jamesframework.core.search.NeighbourhoodSearch
           org.jamesframework.core.search.status.SearchStatus
           org.jamesframework.core.problems.Problem
           org.jamesframework.core.problems.GenericProblem
           org.jamesframework.core.search.neigh.Neighbourhood           
           org.jamesframework.core.problems.sol.RandomSolutionGenerator
           org.jamesframework.core.search.stopcriteria.StopCriterion
           org.jamesframework.core.search.stopcriteria.MaxRuntime
           org.jamesframework.core.search.stopcriteria.MaxTimeWithoutImprovement
           org.jamesframework.core.search.stopcriteria.MaxSteps
           org.jamesframework.core.search.stopcriteria.MaxStepsWithoutImprovement
           org.jamesframework.core.search.stopcriteria.MinDelta
           org.jamesframework.core.search.listeners.SearchListener
           java.util.concurrent.TimeUnit
           org.jamesframework.core.problems.objectives.Objective
           org.jamesframework.core.problems.objectives.evaluations.SimpleEvaluation
           org.jamesframework.core.problems.objectives.evaluations.Evaluation
           org.jamesframework.core.search.algo.ParallelTempering
           org.jamesframework.core.search.neigh.Move
           io.github.engelberg.mister_rogers.Solution
           java.util.concurrent.ThreadLocalRandom))

;; Creating Objective classes

(deftype Obj [^boolean minimizing? evaluate]
  Objective
  (isMinimizing [this] minimizing?)
  (evaluate [this solution data]
    (w/wrap-evaluation (evaluate (.-o ^Solution solution) data))))

(deftype ObjDelta [^boolean minimizing? evaluate evaluate-delta]
  Objective
  (isMinimizing [this] minimizing?)
  (evaluate [this solution data]
    (w/wrap-evaluation (evaluate (.-o ^Solution solution) data)))
  (evaluate [this move curSolution curEvaluation data]
    (w/wrap-evaluation
     (evaluate-delta
      (w/.move ^mister_rogers.wrappers.WrapMove move)
      (.-o ^Solution curSolution)
      (w/unwrap-evaluation curEvaluation) data))))

;; Creating Problem classes

(defn map->problem [{:keys [evaluate evaluate-delta minimizing? data
                            solution-generator mandatory-constraints
                            penalizing-constraints] :as init-map}]
  (let [objective
        (if evaluate-delta
          (ObjDelta. minimizing? evaluate evaluate-delta)
          (Obj. minimizing? evaluate))
        random-solution-generator
        (reify
          RandomSolutionGenerator
          (create [this rnd data]
            (w/wrap-solution (solution-generator data))))
        problem (GenericProblem. data objective random-solution-generator)]
    (doseq [mc mandatory-constraints]
      (.addMandatoryConstraint problem (w/wrap-constraint mc)))
    (doseq [pc penalizing-constraints]
      (.addPenalizingConstraint problem (w/wrap-constraint pc)))
    problem))

;; About constraints
;; A constraint is either an implementation of Constraint protocol or
;; a map {:name Any, :validate (fn [solution data]),
;;        :validate-delta (fn [move cur-solution cur-validation data]) [optional]}

;; Search Status

(def status-name {SearchStatus/IDLE :idle
                  SearchStatus/INITIALIZING :initializing
                  SearchStatus/RUNNING :running
                  SearchStatus/TERMINATING :terminating
                  SearchStatus/DISPOSED :disposed})

;; Creating Search Listeners

(defmacro apply-fn-if-present [f & args]
  `(when ~f (~f ~@args)))

(defn map->search-listener
  "Optional keys:  
  :search-started - (fn [search])
  :search-stopped - (fn [search])
  :new-best-solution - (fn [search] solution evaluation validation)
  :new-current-solution - (fn [search] solution evaluation validation)
  :step-completed - (fn [search num-steps])  
  :status-changed - (fn [search search-status])"
  [{:keys [search-started search-stopped new-best-solution new-current-solution
           step-completed status-changed]}]
  (reify SearchListener
    (searchStarted [this search] (apply-fn-if-present search-started search))
    (searchStopped [this search] (apply-fn-if-present search-stopped search))
    (newBestSolution [this search solution evaluation validation]
      (apply-fn-if-present
       new-best-solution search solution evaluation validation))
    (newCurrentSolution [this search solution evaluation validation]
      (apply-fn-if-present
       new-current-solution search solution evaluation validation))
    (stepCompleted [this search num-steps]
      (apply-fn-if-present step-completed num-steps))
    (statusChanged [this search new-status]
      (apply-fn-if-present status-changed search (status-name new-status)))))

;; Stop criteria

(def time-units {:days TimeUnit/DAYS
                 :d TimeUnit/DAYS
                 :hours TimeUnit/HOURS
                 :h TimeUnit/HOURS
                 :microseconds TimeUnit/MICROSECONDS 
                 :milliseconds TimeUnit/MILLISECONDS
                 :ms TimeUnit/MILLISECONDS                 
                 :minutes TimeUnit/MINUTES
                 :m TimeUnit/MINUTES
                 :nanoseconds TimeUnit/NANOSECONDS 
                 :seconds TimeUnit/SECONDS
                 :s TimeUnit/SECONDS})

(defnc max-runtime [^long time unit-kw]
  (MaxRuntime. time (time-units unit-kw)))

(defnc max-steps [^long max-steps]
  (MaxSteps. max-steps))

(defnc max-steps-without-improvement [^long max-steps-without-improvement]
  (MaxStepsWithoutImprovement. max-steps-without-improvement))

(defnc max-time-without-improvement-millis [^long time unit-kw]
  (MaxTimeWithoutImprovement. time (time-units unit-kw)))

(defnc min-delta [^double min-delta]
  (MinDelta. min-delta))

;; Searches

(defn require-keys [init-map ks]
  (doseq [k ks]
    (when (nil? (get init-map k))
      (throw (ex-info (str k " is required key for map" init-map))))))

(defn prepare-search! [^Search search {:keys [search-listeners stop-criteria]}]
  (doseq [listener search-listeners]
    (.addSearchListener search (map->search-listener listener)))
  (doseq [criterion stop-criteria]
    (.addStopCriterion search criterion)))

(defnc random-descent-search
  "Takes a map with required keys
  :evaluate - (fn [solution data]) -> number or Evaluation
  :minimizing? - boolean
  :solution-generator - (fn [data]) -> random initial solution
  :neighborhood - Neighborhood
  :stop-criteria - vec of stop criteria
and optional keys
  :name - String
  :evaluate-delta (fn [move cur-solution cur-evaluation data])
                       -> number or Evaluation
  :mandatory-constraints - vec of Constraint
  :penalizing-constraints - vec of Constraint
  :search-listeners - list of SearchListeners
  :data - your own dataset, used to evaluate solutions"

  ([init-map]
   :do (require-keys init-map [:evaluate :minimizing? :solution-generator
                               :neighborhood :stop-criteria])
   :let [default-map {:name "RandomDescentSearch"}
         init-map (merge default-map init-map)
         {:keys [name neighborhood search-listeners stop-criteria]} init-map
         problem (map->problem init-map)
         search (RandomDescent. name problem (w/wrap-neighborhood neighborhood))]
   :do (prepare-search! search init-map)
   search))

;; Things you may want to do to a search

(defrecord SearchState
    [name id ^long runtime ^long time-without-improvement
     best-solution best-solution-evaluation best-solution-validation
     current-solution current-solution-evaluation current-solution-validation
     ^long steps ^long steps-without-improvement ^double min-delta
     status ^long num-accepted-moves ^long num-rejected-moves])
                        
(defnc search-state [^Search search]
  :let [local-search? (instance? LocalSearch search),
        neighborhood-search? (instance? NeighbourhoodSearch search)]
  (SearchState. (.getName search)
                (.getID search)
                (.getRuntime search)
                (.getTimeWithoutImprovement search)
                (.-o ^Solution (.getBestSolution search))
                (w/unwrap-evaluation (.getBestSolutionEvaluation search))
                (w/unwrap-validation (.getBestSolutionValidation search))
                (when local-search?
                  (.-o ^Solution (.getCurrentSolution ^LocalSearch search)))
                (when local-search?
                  (w/unwrap-evaluation (.getCurrentSolutionEvaluation ^LocalSearch search)))
                (when local-search?
                  (w/unwrap-validation (.getCurrentSolutionValidation ^LocalSearch search)))
                (.getSteps search)
                (.getStepsWithoutImprovement search)
                (.getMinDelta search)
                (status-name (.getStatus search))
                (when neighborhood-search?
                  (.getNumAcceptedMoves ^NeighbourhoodSearch search))
                (when neighborhood-search?
                  (.getNumRejectedMoves ^NeighbourhoodSearch search))))

(defrecord SEV [solution evaluation validation])
  
(defn get-best [^Search search]
  (SEV. (.-o ^Solution (.getBestSolution search))
        (w/unwrap-evaluation (.getBestSolutionEvaluation search))
        (w/unwrap-validation (.getBestSolutionValidation search))))

(defn get-best-solution [^Search search]
  (.-o ^Solution (.getBestSolution search)))

(defn get-best-solution-evaluation [^Search search]
  (w/unwrap-evaluation (.getBestSolutionEvaluation search)))

(defn get-best-solution-validation [^Search search]
  (w/unwrap-validation (.getBestSolutionValidation search)))

(defnc get-violated-constraints
  ([^Search search solution]
   :let [^GenericProblem problem (.getProblem search)]
   (map w/unwrap-constraint (.getViolatedConstraints problem solution))))

;; Control functions

(defn start [^Search search]
  (.start search)
  (search-state search))

(defn stop [^Search search]
  (.stop search)
  (search-state search))

