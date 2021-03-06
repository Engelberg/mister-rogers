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
            [mister-rogers.stop-criterion-checker :as check]
            [mister-rogers.cache :as cache]
            [primitive-math :as pm]))

(declare get-runtime get-steps compute-delta reorganize-listeners
         init start stop search-started search-stopped)

(def ^:const IDLE 0)
(def ^:const INITIALIZING 1)
(def ^:const RUNNING 2)
(def ^:const TERMINATING 3)
(def ^:const DISPOSED 4)
(def status-name [:idle :intializing :running :terminating :disposed])

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
                   neighborhood v-num-accepted-moves v-num-rejected-moves cache]
  Runnable
  (run [this] (start this))  
  Object
  (toString [this] (label this)))

;; v-status is a volatile containing one of
;; IDLE, INITIALIZING, RUNNING, TERMINATING, DISPOSED
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
                     :v-status (volatile! IDLE)}]
  (nil? (:problem init-map))
  (throw (ex-info ":problem is required key for init-map in search" init-map)),
  :let [search (map->Search (merge default-map
                                   (update init-map :search-listeners
                                           reorganize-listeners)))]
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
                     :v-num-accepted-moves (volatile! 0),
                     :v-num-rejected-moves (volatile! 0),
                     :cache (cache/single-evaluated-move-cache)}
        search (local-search (merge default-map init-map))]
  search)

;; Search listeners

(defrecord SearchListener [search-started search-stopped new-best-solution
                           new-current-solution step-completed status-changed])

(defn reorganize-listeners [listeners]  
  (SearchListener. (seq (doall (keep :search-started listeners)))
                   (seq (doall (keep :search-stopped listeners)))
                   (seq (doall (keep :new-best-solution listeners)))
                   (seq (doall (keep :new-current-solution listeners)))
                   (seq (doall (keep :step-completed listeners)))
                   (seq (doall (keep :status-changed listeners)))))


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
  (map->SearchListener listener-map))

(defn fire-search-started [{:keys [search-listeners] :as search}]
  (when-let [search-started (:search-started search-listeners)]
    (run! #(% search) search-started)))

(defn fire-search-stopped [{:keys [search-listeners] :as search}]
  (when-let [search-stopped (:search-stopped search-listeners)]
    (run! #(% search) search-stopped)))

(defnc fire-new-best-solution
  [^Search search best-solution best-evaluation best-validation]
  :let [^SearchListener search-listeners (.-search-listeners search)]
  (when-let [new-best-solution (.-new-best-solution search-listeners)]
    (run! #(% search best-solution best-evaluation best-validation) new-best-solution)))

(defnc fire-new-current-solution
  [^Search search current-solution current-evaluation current-validation]
  :let [^SearchListener search-listeners (.-search-listeners search)]
  (when-let [new-current-solution (.-new-current-solution search-listeners)]
    (run! #(% search current-solution current-evaluation current-validation) new-current-solution)))

(defnc fire-step-completed [^Search search ^long current-steps]
  :let [^SearchListener search-listeners (.-search-listeners search)]
  (when-let [step-completed (.-step-completed search-listeners)]
    (run! #(% search current-steps) step-completed)))

(defnc fire-status-changed [{:keys [search-listeners] :as search} new-status]
  (when-let [status-changed (:status-changed search)]
    (run! #(% search new-status) status-changed)))

;; Status helpers

(defn get-status ^long [search]
  (long @(:v-status search)))

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
      (debugf "Search %s changed status %s ---> %s" search
              (status-name previous-status) (status-name status))
      (fire-status-changed search status))))

(defn continue-search? [search]
  (not (pm/== (get-status search) TERMINATING)))

;; Search control functions

(defn start [{:keys [stop-criterion-checker a-step-info v-status strategy]
              :as search}]
  (locking v-status
    (assert-status search #{IDLE} "Cannot start search")
    (change-status! search INITIALIZING))
  (infof "Search %s started" search)  
  (fire-search-started search)
  (search-started search)  
  (when (continue-search? search)
    (check/start-checking stop-criterion-checker search stop)
    (change-status! search RUNNING)
    (while (continue-search? search)
      (let [improvement-during-step? (mrp/search-step strategy search),
            ;; before-best @(:best search),
            ;; _ (mrp/search-step strategy search),
            ;; after-best @(:best search),
            ;; improvement-during-step? (not (identical? before-best after-best)),
            ^StepInfo step-info @a-step-info,
            current-steps (inc (.-current-steps step-info)),
            steps-since-last-improvement
            (if improvement-during-step? 0
                (inc (.-steps-since-last-improvement step-info)))]
        (reset! a-step-info (StepInfo. current-steps steps-since-last-improvement))
        (fire-step-completed search current-steps)))
    ;; (when (check/stop-criterion-satisfied? stop-criterion-checker search)
    ;;   (stop search)))
    (check/stop-checking stop-criterion-checker search))
  (search-stopped search)
  (fire-search-stopped search)
  (infof "Search %s stopped (runtime: %d ms, steps: %d" search
         (get-runtime search) (get-steps search))  
  (change-status! search IDLE)
  @(:a-best search))

(defn stop "Sets status to terminating to interrupt search" [search]
  (locking (:v-status search)
    (when (status-one-of? search #{INITIALIZING RUNNING})
      (change-status! search TERMINATING)))
  false)

(defn dispose "Sets status to dispose so it can't be restarted" [search]
  (locking (:v-status search)
    (when-not (status-one-of? search #{DISPOSED})
      (assert-status search #{IDLE} "Cannot dispose search.")
      (change-status! search DISPOSED)))
  false)

;; Getting solution

(defnc get-best-solution [^Search search]
  :let [^SEV current @(.-a-best search)]
  (.-solution current))

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
           (if (or (pm/== min-delta -1.0) (pm/< delta min-delta))
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
                :let [best-solution (.-solution best)
                      best-evaluation (.-evaluation best)
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
   (when-let [cache (.-cache search)] (cache/clear cache))
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
  ^long [^Search search]
  (cond
    :let [status (get-status search)]      
    (pm/== status INITIALIZING) -1
    :let [^Timestamps timestamps @(.-a-timestamps search)
          start-time (.-start-time timestamps)
          stop-time (.-stop-time timestamps)]
    (or (pm/== status IDLE) (pm/== status DISPOSED))
    (if (pm/== stop-time -1) -1 (pm/- stop-time start-time)),
    :else (pm/- (System/currentTimeMillis) start-time)))

(defn get-steps ^long [^Search search]
  (let [^StepInfo step-info @(.-a-step-info search)]
    (.-current-steps step-info)))

(defn get-min-delta ^double [^Search search] @(.-a-min-delta search))

(defn get-time-without-improvement
  "If search is running or terminating, returns runtime since last improvement.
   If search is idle or disposed, returns stop-time - last-improvement-time.
   If search is initializing, returns -1"  
  ^long [^Search search]
  (cond
    :let [status (get-status search)]
    (pm/== status INITIALIZING) -1
    :let [^Timestamps timestamps @(.-a-timestamps search)
          last-improvement-time (.-last-improvement-time timestamps)
          stop-time (.-stop-time timestamps)]
    (pm/== last-improvement-time -1) (get-runtime search)
    (or (pm/== status IDLE) (pm/== status DISPOSED))
    (pm/- stop-time last-improvement-time)
    :else (pm/- (System/currentTimeMillis) last-improvement-time)))

(defn get-steps-without-improvement
  ^long [^Search search]
  (cond
    :let [status (get-status search)]
    (pm/== status INITIALIZING) -1
    :let [^StepInfo step-info @(.-a-step-info search)
          steps-since-last-improvement (.-steps-since-last-improvement
                                        step-info)]
    (pm/== steps-since-last-improvement -1) (.-current-steps step-info)
    :else steps-since-last-improvement))
  
(defn compute-delta ^double [^Search search current-evaluation previous-evaluation]
  (cond
    (or (nil? current-evaluation) (nil? previous-evaluation)) -1.0
    :let [problem (.-problem search)]
    (prob/minimizing? problem)
    (pm/- (mrp/value previous-evaluation) (mrp/value current-evaluation)),
    :else (pm/- (mrp/value current-evaluation) (mrp/value previous-evaluation))))

;; Search callbacks

(defn search-started [{:keys [a-timestamps a-min-delta a-step-info
                              v-num-accepted-moves v-num-rejected-moves]
                       :as search}]
  (init search)
  (when v-num-accepted-moves (vreset! v-num-accepted-moves 0))
  (when v-num-rejected-moves (vreset! v-num-rejected-moves 0))
  (reset! a-timestamps (Timestamps. (System/currentTimeMillis) -1 -1))
  (reset! a-step-info (StepInfo. 0 -1))
  (reset! a-min-delta -1))

(defn search-stopped [^Search search]
  (swap! (.a-timestamps search) assoc :stop-time (System/currentTimeMillis)))

(defnc init "Generates current solution if needed"
  [^Search search]  
  :when-let [a-current (.-a-current search)]
  :let [problem (.-problem search)
        current-solution (.-solution ^SEV @a-current)]
  current-solution nil ;; Solution already is present
  :let [random-solution (prob/create-random-solution problem)]
  (update-current-and-best-solution search random-solution))

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

(defn improvement-move? [^Search search move]
  (let [^SEV current @(.-a-current search)
        current-validation (.-validation current)]
    (and (not (nil? move))
         (mrp/passed? (validate-move search move))
         (or (not (mrp/passed? current-validation))
             (< 0.0 (compute-delta search (evaluate-move search move)
                                   (.-evaluation current)))))))

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
             (cache/cache-move-evaluation chosen-move chosen-move-evaluation)
             (cache/cache-move-validation chosen-move chosen-move-validation))
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
       :let [evaluation (evaluate-move search move),
             delta (compute-delta search evaluation current-evaluation)]
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
  :do (vswap! (.-v-num-accepted-moves search) inc)
  true)

(defn reject-move [^Search search move]
  (vswap! (.-v-num-rejected-moves search) inc)
  false)
