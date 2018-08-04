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
            [mister-rogers.stop-criterion-checker :as crit]))

(def next-id (atom 0))
(defn get-next-id [] (swap! next-id inc))

(defrecord SearchState [^long start-time, ^long stop-time,
                        ^long current-steps, ^long last-improvement-time,
                        ^long steps-since-last-improvement,
                        ^boolean improvement-during-current-step?,
                        ^double min-delta,
                        best-solution, best-solution-evaluation,
                        best-solution-validation, status])

;; The last field in the record, status, is one of either:
;; :idle, :initializing, :running, :terminating, :disposed
;; Primitives are initialized to -1

(defn label [search]
  (str \( (:name search) \: (:id search) \)))
(declare start) ;; To implement runnable interface
(defrecord Search [name id problem search-listeners stop-criterion-checker state]
  Runnable
  (run [this] (start this))
  Object
  (toString [this] (label this)))

;; Search's state is an atom containing a SearchState

;; SearchState getters for convenience
(defn ^long get-start-time [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-start-time search-state)))

(defn ^long get-stop-time [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-stop-time search-state)))

(defn ^long get-current-steps [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-current-steps search-state)))

(defn ^long get-last-improvement-time [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-last-improvement-time search-state)))

(defn ^long get-steps-since-last-improvement [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-steps-since-last-improvement search-state)))

(defn ^boolean get-improvement-during-current-step? [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-improvement-during-current-step? search-state)))

(defn ^double get-min-delta [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-min-delta search-state)))

(defn get-best-solution [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-best-solution search-state)))

(defn get-best-solution-evaluation [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-best-solution-evaluation search-state)))

(defn get-best-solution-validation [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-best-solution-validation search-state)))

(defn get-status [^Search search]
  (let [^SearchState search-state @(.-state search)]
    (.-status search-state)))

(defnc search
  "Takes a map with required key :problem and optional keys
:name, :search-listeners, :stop-criterion-checker"
  [init-map]
  :let [state {:start-time -1, :stop-time -1, :current-steps -1, :last-improvement-time -1,
               :steps-since-last-improvement -1, :improvement-during-current-step? false
               :min-delta -1.0, :status :idle}
        id (get-next-id)
        default-map {:name "Search"
                     :id id
                     :state (atom (map->SearchState state))}]
  (nil? (:problem init-map))
  (throw (ex-info ":problem is required key for init-map in search" init-map)),
  :let [search (map->Search (merge default-map init-map))]
  :do (infof "Created search %s" search)
  search)

(defrecord SearchListener [search-started search-stopped new-best-solution step-completed status-changed])
(def ^:private valid-search-listener-key? (set (keys (map->SearchListener {}))))
(defnc search-listener "Takes a map with the following optional keys, and functions as values:
  :search-started - (fn [search])
  :search-stopped - (fn [search])
  :new-best-solution - (fn [search new-best-solution new-best-evaluation new-best-validation])
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
    (when search-stopped (search-stopped search)))

(defnc fire-new-best-solution
  [{:keys [search-listeners] :as ^Search search}
   best-solution best-solution-evaluation best-solution-validation]
  (doseq [{:keys [new-best-solution]} search-listeners]
    (when new-best-solution 
      (new-best-solution
       search best-solution best-solution-evaluation best-solution-validation))))

(defnc fire-step-completed [{:keys [search-listeners] :as search}
                            ^long current-steps]
  (doseq [{:keys [step-completed]} search-listeners]
    (when step-completed
      (step-completed current-steps))))

(defn fire-status-changed [{:keys [search-listeners] :as search} new-status]
  (when status-changed
    (doseq [{:keys [status-changed]} search-listeners] (status-changed new-status))))

;; Status helpers

(defn status-one-of? [search status-set]
  (contains? status-set (get-status search)))

(defn assert-status [search status-set msg]
  (when-not (status-one-of? search status-set)
    (let [info (ex-info msg {:status-should-be-one-of status-set,
                             :status (get-status search)})]
      (throw info))))

(defn change-status! [{:keys [state] :as search} status]
  (let [previous-status (get-status search)]
    (swap! state assoc :status status)
    (debugf "Search %s changed status %s ---> %s" search previous-status status)
    (fire-status-changed search status)))

(defn continue-search? [search]
  (not= (get-status search) :terminating))

;; Search control functions
(declare search-started search-stopped search-disposed search-step stop)

(defn start [{:keys [stop-criterion-checker] :as ^Search search}]
  (assert-status search #{:idle} "Cannot start search")
  (change-status! search :initializing)
  (infof "Search %s started" search)  
  (fire-search-started search)
  (search-started search)  
  (when (continue-search? search)
    (crit/start-checking search stop-criterion-checker)
    (change-status! search :running)
    (while (continue-search? search)
      ;; TBD: Improve performance
      (let [^SearchState new-ss
            (swap!
             (.-state search)
             (fn [^SearchState ss]
               (let [improvement-during-current-step? (search-step search),
                     current-steps (inc (.-current-steps ss))
                     steps-since-last-improvement
                     (if improvement-during-current-step?
                       0 (inc (.-steps-since-last-improvement ss)))]
                 (assoc ss :improvement-during-current-step? improvement-during-current-step?
                        :current-steps current-steps
                        :steps-since-last-improvement steps-since-last-improvement))))]
        (fire-step-completed search (.-current-steps new-ss)))
      (when (crit/stop-criterion-satisfied? stop-criterion-checker)
        (stop search)))
    (crit/stop-checking search stop-criterion-checker))
  (search-stopped search)
  (fire-search-stopped search)
  (let [^SearchState s @(.-state search)]
    (infof "Search %s stopped (runtime: %d ms, steps: %d" search
           (.-runtime s) (.-current-steps s)))
  (change-status! search :idle))

(defn stop "Sets status to terminating to interrupt search" [search]
  (when (status-one-of? search #{:initializing :running})
    (change-status! search :terminating)))

(defn dispose "Sets status to dispose so it can't be restarted" [search]
  (when-not (status-one-of? search #{:disposed})
    (change-status! search :disposed)))

;; Updating solution

(declare compute-delta)
(defnc update-best-solution "Updates soution if better"
  ([^Search search new-solution]
   :let [problem (.-problem search),
         new-validation (prob/validate problem new-solution)]
   (not (mrp/passed? new-validation)) false ;; invalid solution
   :let [new-evaluation (prob/evaluate problem new-solution)]
   (update-best-solution search new-solution new-evaluation new-validation))
  
  ([^Search search new-solution new-evaluation new-validation]
   (not (mrp/passed? new-validation)) nil ;; invalid solution
   :let [state (.-state search),
         [old new]
         (swap-vals!
          state
          (fn [^SearchState ss]
            (cond
              :let [best-solution (.-solution ss),
                    best-solution-evaluation (.-best-solution-evaluation ss)
                    delta (compute-delta search
                                         new-evaluation best-solution-evaluation)]
              (not (or (nil? best-solution) (> delta 0.0))) ss ;; no improvement
              :let [min-delta (.-min-delta ss)
                    new-delta (if (or (= min-delta -1.0) (< delta min-delta))
                                delta min-delta)]
              ;; TBD: Improve performance
              (assoc ss :min-delta new-delta,
                     :last-improvement-time (System/currentTimeMillis),
                     :improvement-during-current-step? true,
                     :best-solution new-solution
                     :best-solution-evaluation new-evaluation
                     :best-solution-validation new-validation))))]
   (identical? old new) false
   :do (fire-new-best-solution search new-solution new-evaluation new-validation)
   true))

(defn get-runtime
  "If search is running or terminating, returns runtime since beginning of run in ms.
   If search is idle or disposed, returns runtime of last run in ms, or -1 if no run yet.
   If search is initializing, returns -1."
  ^long [^SearchState ss]
  (cond
    :let [status (.-status ss)
          stop-time (.-stop-time ss)
          start-time (.-start-time ss)]
    (= status :initializing) -1
    (or (= status :idle) (= status :disposed)) (if (= stop-time -1) -1 (- stop-time start-time))
    :else (- (System/currentTimeMillis) start-time)))

(defn get-time-without-improvement ^long [^SearchState ss]
  (cond
    :let [status (.-status ss)
          last-improvement-time (.-last-improvement-time ss)
          stop-time (.-stop-time ss)]
    (= status :intializing) -1
    (= last-improvement-time -1) (get-runtime ss)
    (or (= status :idle) (= status :disposed)) (- stop-time last-improvement-time)
    :else (- (System/currentTimeMillis) last-improvement-time)))

(defn get-steps-without-improvement ^long [^SearchState ss]
  (cond
    :let [status (.-status ss)
          steps-since-last-improvement (.-steps-since-last-improvement ss)]
    (= status :initializing) -1
    (= steps-since-last-improvement -1) (.-current-steps ss)
    :else steps-since-last-improvement))

(defnc compute-delta [^Search search current-evaluation previous-evaluation]
  :let [problem (.-problem search)]
  (prob/minimizing? problem) (- (mrp/value previous-evaluation) (mrp/value current-evaluation))
  :else (- (mrp/value current-evaluation) (mrp/value previous-evaluation)))

(defn search-started [^Search search]
  ;; Initialize search by calling init ?
  (let [state (.-state search)
        start-time (System/currentTimeMillis),
        stop-time -1
        current-steps 0
        last-improvement-time -1
        steps-since-last-improvement -1
        min-delta -1]
    (swap!
     state
     (fn [^SearchState ss]
       (let [best-solution (.-best-solution ss),
             best-solution-evaluation (.-best-solution-evaluation ss)
             best-solution-validation (.-best-solution-validation ss)
             status (.-status ss)]
         (SearchState. start-time stop-time
                       current-steps last-improvement-time
                       steps-since-last-improvement false
                       min-delta best-solution best-solution-evaluation
                       best-solution-validation ss))))))

(defn search-stopped [^Search search]
  (swap! (.-state search) assoc :stop-time (System/currentTimeMillis)))

(defn search-disposed [^Search search])

(defn search-step [^Search search])




















;; Performance testing

(defprotocol Tp
  (get-x ^long [this]))

(defrecord T1 [x]
  Tp
  (get-x [this] x))

(defn test1 [n]
  (loop [n n t (T1. 0)]
    (if (= n 0) t
        (recur (dec n) (update t :x inc)))))

(defn test2 [n]
  (loop [n n ^T1 t (T1. 0)]
    (if (= n 0) t
        (let [x (get-x t)]
          (recur (dec n) (T1. (inc x)))))))

(defn test3 [n]
  (loop [n n ^T1 t (T1. 0)]
    (if (= n 0) t
        (let [x (.-x t)]
          (recur (dec n) (assoc t :x (inc x)))))))

(defrecord T2 [^long x ^long y])

(defn test4[n]
  (loop [n n t (T2. 0 0)]
    (if (= n 0) t
        (recur (dec n) (update t :x inc)))))

(defn test5 [n]
  (loop [n n ^T2 t (T2. 0 0)]
    (if (= n 0) t
        (let [x (.-x t) y (.-y t)]
          (recur (dec n) (T2. (inc x) y))))))

(defn test6 [n]
  (loop [n n ^T2 t (T2. 0 0)]
    (if (= n 0) t
        (let [x (.-x t)]
          (recur (dec n) (assoc t :x (inc x)))))))

(defn test7 [n]
  (loop [n n ^T2 t (T2. 0 0)]
    (if (= n 0) t
        (let [x (.-x t), y (.-y t)]
          (recur (dec n) (assoc t :x (inc x) :y (inc y)))))))

(defn test8 [n]
  (loop [n n ^T2 t (T2. 0 0)]
    (if (= n 0) t
        (let [x (.-x t), y (.-y t)]
          (recur (dec n) (into t [(medley/map-entry :x (inc x)) (medley/map-entry :y (inc y))]))))))

(defrecord T3 [x])

(defn test9 [n]
  (let [box (atom (assoc (T3. 0) :y 0))]
    (loop [n n box box]
      (if (= n 0) box
          (recur (dec n)
                 (do (swap! box (fn [^T3 t]
                                  (let [y (:y t)]
                                    (assoc t :y y))))
                     box))))))
