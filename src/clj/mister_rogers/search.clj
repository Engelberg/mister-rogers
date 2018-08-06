(ns mister-rogers.search
  (:refer-clojure :exclude [cond])
  (:require [better-cond.core :refer [cond defnc]]
            [medley.core :as medley]
            [com.rpl.specter :as specter
             :refer [ALL NONE ATOM keypath select-any transform setval]]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [mister-rogers.protocols :as mrp]
            [mister-rogers.problem :as prob]
            [mister-rogers.stop-criterion-checker :as crit]
            [mister-rogers.cache :as cache]))

(declare get-runtime get-steps compute-delta
         init start stop search-started search-stopped)

(def next-id (atom 0))
(defn get-next-id [] (swap! next-id inc))

(defrecord Timestamps [^long start-time ^long stop-time
                       ^long last-improvement-time])

(defrecord SEV [solution evaluation validation])

(defrecord StepInfo [^long current-steps ^long steps-since-last-improvement])

;; Primitives are initialized to -1

(defn label [search]
  (str \( (:name search) \: (:id search) \)))

(defrecord Search [name id problem strategy search-listeners stop-criterion-checker
                   a-timestamps a-best a-current a-step-info a-min-delta v-status
                   neighborhood a-num-accepted-moves a-num-rejected-moves cache]
  mrp/Search
  (init [this] (init this))
  (start [this] (start this))
  (stop [this] (stop this))
  (search-started [this] (search-started this))
  (search-stopped [this] (search-stopped this))
  Runnable
  (run [this] (start this))  
  Object
  (toString [this] (label this)))

;; v-status is a volatile containing one of
;; :idle, :initializing, :running, :terminating, :disposed
;; Changes to v-status are generally associated with a
;; triggering of listeners that can potentially have side effects, so
;; neither atoms nor refs will suffice.
;; Therefore, always lock v-status before changing.

(defnc search
  "Takes a map with required key :problem and optional keys
:name, :search-listeners, :stop-criterion-checker, :strategy, :neighborhood.
Creates a search that tracks best solution found so far.
Usually used to aggregate information from other searches."
  [init-map]
  :let [default-map {:name "Search",
                     :id (get-next-id),
                     :a-timestamps (atom (Timestamps. -1 -1 -1)),
                     :a-best (atom (SEV. nil nil nil))
                     :a-current nil ;; basic search doesn't track current solution
                     :a-step-info (atom (StepInfo. -1 -1))
                     :a-min-delta (atom 1.0)
                     :v-status (volatile! :idle)}]
  (nil? (:problem init-map))
  (throw (ex-info ":problem is required key for init-map in search" init-map)),
  :let [search (map->Search (merge default-map init-map))]
  :do (infof "Created search %s" search)
  search)

(defnc local-search
  "Takes a map with required key :problem and optional keys
:name, :search-listeners, :stop-criterion-checker, :strategy, :neighborhood.
Creates a local search, which tracks best solution and also has a notion of
a current solution as it explores the solution space."
  [init-map]
  :let [default-map {:name "LocalSearch"
                     :a-current (atom (SEV. nil nil nil))}]
  (nil? (:problem init-map))
  (throw (ex-info ":problem is required key for init-map in search" init-map)),
  :let [search (search (merge default-map init-map))]
  search)

(defnc neighborhood-search
  "Takes a map with required key :problem and :neighborhood
and optional keys :name, :search-listeners, :stop-criterion-checker, :strategy.
Creates a search that uses moves generated by a neighborhood to
explore out from a randomly-generated solution, to optimize."
  [init-map]
  (nil? (:problem init-map)) 
  (throw (ex-info ":problem is a required key for init-map in search" init-map))
  (nil? (:neighborhood init-map))
  (throw (ex-info ":neighborhood is a required key for init-map in search"
                  init-map))
  :let [default-map {:name "NeighborhoodSearch",
                     :a-num-accepted-moves (atom 0),
                     :a-num-rejected-moves (atom 0),
                     :cache (cache/single-evaluated-move-cache)}
        search (local-search (merge default-map init-map))]
  search)

(defrecord SearchListener [search-started search-stopped new-best-solution
                           new-current-solution step-completed status-changed])
(def ^:private valid-search-listener-key? (set (keys (map->SearchListener {}))))
(defnc search-listener "Takes a map with the following optional keys, and functions as values:
  :search-started - (fn [search])
  :search-stopped - (fn [search])
  :new-best-solution - (fn [search new-best-solution new-best-evaluation new-best-validation])
  :new-current-solution - (fn [search new-current-solution new-current-evaluation new-current-validation])
  :step-completed - (fn [search num-steps])
  :status-changed - (fn [search new-status])"
  [listener-map]
  :do (doseq [k (keys listener-map)]
        (when-not (valid-search-listener-key? k)
          (warnf "%s is invalid key for listener-map in search-listener" k)))
  (->SearchListener listener-map))

(defn fire-search-started [{:keys [search-listeners] :as search}]
  (doseq [{:keys [search-started]} search-listeners]
    (when search-started (search-started search))))

(defn fire-search-stopped [{:keys [search-listeners] :as search}]
  (doseq [{:keys [search-stopped]} search-listeners]
    (when search-stopped (search-stopped search))))

(defn fire-new-best-solution
  [{:keys [search-listeners] :as ^Search search}
   best-solution best-evaluation best-validation]
  (doseq [{:keys [new-best-solution]} search-listeners]
    (when new-best-solution 
      (new-best-solution
       search best-solution best-evaluation best-validation))))

(defn fire-new-current-solution
  [{:keys [search-listeners] :as ^Search search}
   current-solution current-evaluation current-validation]
  (doseq [{:keys [new-current-solution]} search-listeners]
    (when new-current-solution 
      (new-current-solution
       search current-solution current-evaluation current-validation))))

(defn fire-step-completed [{:keys [search-listeners] :as search}
                           ^long current-steps]
  (doseq [{:keys [step-completed]} search-listeners]
    (when step-completed
      (step-completed current-steps))))

(defn fire-status-changed [{:keys [search-listeners] :as search} new-status]
  (doseq [{:keys [status-changed]} search-listeners]
    (when status-changed (status-changed new-status))))

;; Status helpers

(defn get-status [search]
  @(:v-status search))

(defn status-one-of? [search status-set]
  (contains? status-set (get-status search)))

(defn assert-status [search status-set msg]
  (when-not (status-one-of? search status-set)
    (throw (ex-info msg {:status-should-be-one-of status-set,
                         :status (get-status search)}))))

(defn change-status! [{:keys [v-status] :as search} status]
  (locking v-status
    (let [previous-status (get-status search)]
      (vreset! v-status status)
      (debugf "Search %s changed status %s ---> %s" search previous-status status)
      (fire-status-changed search status))))

(defn continue-search? [search]
  (not= (get-status search) :terminating))

;; Search control functions

(defn start [{:keys [stop-criterion-checker a-step-info v-status]
              :as search}]
  (locking v-status
    (assert-status search #{:idle} "Cannot start search")
    (change-status! search :initializing))
  (infof "Search %s started" search)  
  (fire-search-started search)
  (search-started search)  
  (when (continue-search? search)
    (crit/start-checking stop-criterion-checker search)
    (change-status! search :running)
    (while (continue-search? search)
      (let [before-best @(:best search),
            _ (search-step search),
            after-best @(:best search),
            improvement-during-step? (not (identical? before-best after-best)),
            ^StepInfo step-info @a-step-info,
            current-steps (inc (.-current-steps step-info)),
            steps-since-last-improvement
            (if improvement-during-step? 0
                (inc (.-steps-since-last-improvement step-info)))]
        (reset! step-info (StepInfo. current-steps steps-since-last-improvement))
        (fire-step-completed search current-steps))
      (when (crit/stop-criterion-satisfied? stop-criterion-checker search)
        (stop search)))
    (crit/stop-checking stop-criterion-checker search))
  (search-stopped search)
  (fire-search-stopped search)
  (infof "Search %s stopped (runtime: %d ms, steps: %d" search
         (get-runtime search) (get-steps search))
  (change-status! search :idle))

(defn stop "Sets status to terminating to interrupt search" [search]
  (locking (:v-status search)
    (when (status-one-of? search #{:initializing :running})
      (change-status! search :terminating))))

(defn dispose "Sets status to dispose so it can't be restarted" [search]
  (locking (:v-status search)
    (when-not (status-one-of? search #{:disposed})
      (assert-idle search "Cannot dispose search.")
      (change-status! search :disposed))))

;; Updating solution

(defn update-improvement-time [a-timestamps]
  (swap! a-timestamps
         (fn [^Timestamps timestamps]
           (let [start-time (.-start-time timestamps),
                 stop-time (.-stop-time timestamps)]
             (Timestamps. start-time stop-time (System/currentTimeMillis))))))

(defn update-min-delta [a-min-delta ^double delta]
  (swap! a-min-delta
         (fn [^double min-delta]
           (if (or (= min-delta -1.0) (< delta min-delta))
             delta min-delta))))

(defnc update-best-solution "Updates solution if better, returns true if updated"
  ([^Search search new-solution]
   :let [problem (.-problem search),
         new-validation (prob/validate problem new-solution)]
   (not (mrp/passed? new-validation)) false ;; invalid solution
   :let [new-evaluation (prob/evaluate problem new-solution)]
   (update-best-solution search new-solution new-evaluation new-validation))
  
  ([^Search search new-solution new-evaluation new-validation]
   (not (mrp/passed? new-validation)) nil ;; invalid solution
   (let [a-best (.-a-best search)]
     (swap! a-best
            (fn [^SEV best]
              (cond
                :let [best-solution (.-solution best),
                      best-evaluation (.-evaluation best),
                      delta (compute-delta search new-evaluation best-evaluation)]
                (not (or (nil? best-solution) (> delta 0.0))) best ;; no improve
                :do (update-min-delta (.-a-min-delta search) delta)
                :do (update-improvement-time (.-a-timestamps search))
                (SEV. new-solution new-evaluation new-validation))))

     (fire-new-best-solution search new-solution new-evaluation new-validation)
     true)))

(defn update-current-solution "Updates solution, even if invalid"
  ([^Search search new-solution]
   (let [problem (.-problem search),
         new-validation (prob/validate problem new-solution)
         new-evaluation (prob/evaluate problem new-solution)]
     (update-current-solution search new-solution new-evaluation new-validation)))
  
  ([^Search search new-solution new-evaluation new-validation]
   (when-let [cache (.-cache search)] (cache/clear))
   (reset! (.-a-current search) (SEV. new-solution new-evaluation new-validation))
   (fire-new-current-solution search new-solution new-evaluation new-validation)))

(defn update-current-and-best-solution
  "Current solution updates no matter what, even if invalid, while the
  best solution will only update if it is better than the previous best solution.
  Returns true if best solution was updated."
  ([^Search search new-solution]
   (let [problem (.-problem search),
         new-validation (prob/validate problem new-solution)
         new-evaluation (prob/evaluate problem new-solution)]
     (update-current-and-best-solution
      search new-solution new-evaluation new-validation)))
  
  ([^Search search new-solution new-evaluation new-validation]
   (update-current-solution search new-solution new-evaluation new-validation)
   (update-best-solution search new-solution new-evaluation new-validation)))

;; Helper computation functions for stop criteria primarily

(defn get-runtime
  "If search is running or terminating, returns millis since beginning of run.
   If search is idle or disposed, returns millis of last run, or -1 if no run yet.
   If search is initializing, returns -1."
  ^long [{:keys [a-timestamps v-status] :as search}]
  (locking v-status
    (cond
      :let [status @v-status]      
      (= status :intializing) -1
      :let [^Timestamps timestamps @a-timestamps
            start-time (.-start-time timestamps)
            stop-time (.-stop-time timestamps)]
      (or (= status :idle) (= status :disposed))
      (if (= stop-time -1) -1 (- stop-time start-time)),
      :else (- (System/currentTimeMillis) start-time))))

(defn get-steps ^long [{:keys [a-step-info]}]
  (let [^StepInfo step-info @a-step-info]
    (.-current-steps step-info)))

(defn get-min-delta ^double [{:keys [a-min-delta]}] @a-min-delta)

(defn get-time-without-improvement
  "If search is running or terminating, returns runtime since last improvement.
   If search is idle or disposed, returns stop-time - last-improvement-time.
   If search is initializing, returns -1"  
  ^long [{:keys [a-timestamps v-status] :as search}]
  (locking v-status
    (cond
      :let [status @v-status]
      (= status :intializing) -1
      :let [^Timestamps timestamps @a-timestamps
            last-improvement-time (.-last-improvement-time timestamps)
            stop-time (.-stop-time timestamps)]
      (= last-improvement-time -1) (get-runtime search)
      (or (= status :idle) (= status :disposed))
      (- stop-time last-improvement-time)
      :else (- (System/currentTimeMillis) last-improvement-time))))

(defn get-steps-without-improvement
  ^long [{:keys [a-step-info v-status] :as search}]
  (cond
    :let [status @v-status]
    (= status :initializing) -1
    :let [^StepInfo step-info @a-step-info
          steps-since-last-improvement (.-steps-since-last-improvement step-info)]
    (= steps-since-last-improvement -1) (.-current-steps step-info)
    :else steps-since-last-improvement))

(defn compute-delta ^double [^Search search current-evaluation previous-evaluation]
  (cond
    :let [problem (.-problem search)]
    (prob/minimizing? problem)
    (- (mrp/value previous-evaluation) (mrp/value current-evaluation)),
    :else (- (mrp/value current-evaluation) (mrp/value previous-evaluation))))

;; Search callbacks

(defn search-started [{:keys [a-timestamps a-min-delta a-step-info
                              a-num-accepted-moves a-num-rejected-moves]
                       :as search}]
  (init search)
  (when a-num-accepted-moves (reset! a-num-accepted-moves 0))
  (when a-num-rejected-moves (reset! a-num-rejected-moves 0))
  (reset! a-timestamps (Timestamps. (System/currentTimeMillis) -1 -1))
  (reset! a-step-info (StepInfo. 0 -1))
  (reset! a-min-delta -1))

(defn search-stopped [^Search search]
  (swap! (.-state search) assoc :stop-time (System/currentTimeMillis)))

(defnc init "Generates current solution if needed"
  [search]  
  :when-let [a-current (:a-current search)]
  :let [problem (:problem search)
        current-solution (:current-solution @a-current)]
  current-solution nil ;; Solution already is present
  :let [random-solution (prob/create-random-solution problem)]
  (update-current-and-best-solution random-solution))

;; Neighborhood operations

(defnc evaluate-move [^Search search move]
  :let [cache (.-cache search),
        problem (.-problem search),
        evaluation (when cache (cache/get-cached-move-evaluation cache move))]
  evaluation evaluation
  :let [^SEV current @(.-a-current search),
        evaluation (prob/evaluate-delta
                    problem move (.-solution current) (.-evaluation current))]
  :do (when cache (cache/cache-move-evaluation cache move evaluation))
  evaluation)

(defnc validate-move [^Search search move]
  :let [cache (.-cache search),
        problem (.-problem search),
        validation (when cache (cache/get-cached-move-validation cache move))]
  validation validation
  :let [^SEV current @(.-a-current search),
        validation (prob/validate-delta
                    problem move (.-solution current) (.-validation current))]
  :do (when cache (cache/cache-move-validation cache move validation))
  validation)

(defn ^boolean improvement-move? [^Search search move]
  (let [^SEV current @(.-a-current search)
        current-validation (.-validation current)]
    (and (not (nil? move))
         (mrp/passed? (validate-move search move))
         (or (not (mrp/passed? current-validation))
             (< 0.0 (compute-delta (evaluate-move search move)
                                   (.-evaluation current)))))))


;; For speed, we have a record to return values from loop
(defnc get-best-move
  ([search moves require-improvement? filters]
   (get-best-move moves require-improvement? false filters))
  ([^Search search moves require-improvement? accept-first-improvement? filters]
   :let [cache (.-cache search)
         ^SEV current @(.-a-current search),
         current-evaluation (.-evaluation current)]
   (loop [moves (seq moves), chosen-move nil,
          chosen-move-delta (- Double/MAX_VALUE)
          chosen-move-evaluation nil, chosen-move-validation nil]
     (cond
       :let [move (first moves)]       
       (or (nil? moves) (and accept-first-improvement? (improvement-move? move)))
       (do (when (and cache chosen-move)
             (cache/cache-move-evaluation chosen-move chosen-evaluation)
             (cache/cache-move-validation chosen-move chosen-validation))
           chosen-move),       
       ;; If move doesn't pass all the filters, recur
       (not (every? (fn [pred] (pred move)) filters))
       (recur (next moves) chosen-move chosen-move-delta
              chosen-move-evaluation chosen-move-validation),
       ;; If move doesn't lead to a valid solution, recur
       :let [validation (validate-move search move)]
       (not (mrp/passed? validation))
       (recur (next moves) chosen-move chosen-move-delta
              chosen-move-evaluation chosen-move-validation),
       ;; Choose move if it is better, and is improvement or we don't care
       :let [evaluation (evaluation-move search move),
             delta (compute-delta evaluation current-evaluation)]
       (and (> delta chosen-move-delta)
            (or (not require-improvement?) (improvement-move? move)))
       (recur (next moves) move delta evaluation validation)
       :else (recur (next moves) chosen-move chosen-move-delta
                    chosen-move-evaluation chosen-move-validation)))))

(defnc accept-move [^Search search move]
  :let [validation (validate-move search move)]
  (not (mrp/passed? validation)) false
  :let [evaluation (evaluate-move search move)
        ^SEV current @(.-a-current search),
        new-solution (mrp/apply-move move (.-solution current))]
  :do (update-current-and-best-solution search new-solution evaluation validation)
  :do (swap! (.-a-num-accepted-moves search) inc)
  true)

(defn reject-move [^Search search move]
  (swap! (.-a-num-rejected-moves search) inc))



protected boolean accept(Move<? super SolutionType> move){
        // validate move (often retrieved from cache)
        Validation newValidation = validate(move);
        if(newValidation.passed()){
            // evaluate move (often retrieved from cache)
            Evaluation newEvaluation = evaluate(move);
            // apply move to current solution (IMPORTANT: after evaluation/validation of the move!)
            move.apply(getCurrentSolution());
            // update current solution and best solution
            updateCurrentAndBestSolution(getCurrentSolution(), newEvaluation, newValidation);
            // increase accepted move counter
            incNumAcceptedMoves(1);
            // update successful
            return true;
        } else {
            // update cancelled: invalid neighbour
            return false;
        }
}
