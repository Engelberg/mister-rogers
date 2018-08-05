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

(declare init start stop search-started search-stopped search-disposed search-step
         compute-delta)

(def next-id (atom 0))
(defn get-next-id [] (swap! next-id inc))

(defrecord Timestamps [^long start-time ^long stop-time
                       ^long last-improvement-time])

(defrecord SEV [solution evaluation validation])

(defrecord StepInfo [^long current-steps ^long steps-since-last-improvement])

;; Primitives are initialized to -1

(defn label [search]
  (str \( (:name search) \: (:id search) \)))

(defrecord Search [name id problem search-listeners stop-criterion-checker
                   a-timestamps a-best a-current a-step-info a-min-delta v-status]
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
:name, :search-listeners, :stop-criterion-checker"
  [init-map]
  :let [default-map {:name "Search"
                     :id (get-next-id)
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

(defrecord SearchListener [search-started search-stopped new-best-solution
                           step-completed status-changed])
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
    (when search-stopped (search-stopped search))))

(defn fire-new-best-solution
  [{:keys [search-listeners] :as ^Search search}
   best-solution best-evaluation best-validation]
  (doseq [{:keys [new-best-solution]} search-listeners]
    (when new-best-solution 
      (new-best-solution
       search best-solution best-evaluation best-validation))))

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
    (crit/start-checking search stop-criterion-checker)
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
      (when (crit/stop-criterion-satisfied? stop-criterion-checker)
        (stop search)))
    (crit/stop-checking search stop-criterion-checker))
  (search-stopped search)
  (fire-search-stopped search)
  (infof "Search %s stopped (runtime: %d ms, steps: %d" search
         (get-runtime search) (:current-steps @a-step-info))
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

(defn update-min-delta [a-min-delta]
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
                :do (update-min-delta (.-a-min-delta search))                
                :do (update-improvement-time (.-a-timestamps search))
                (SEV. new-solution new-evaluation new-validation))))

     (fire-new-best-solution search new-solution new-evaluation new-validation)
     true)))

;; Helper computation functions

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
    :let [^StepInfo @a-step-info
          steps-since-last-improvement (.-steps-since-last-improvement step-info)]
    (= steps-since-last-improvement -1) (.-current-steps )
    :else steps-since-last-improvement))

(defn compute-delta ^double [^Search search current-evaluation previous-evaluation]
  (cond
    :let [problem (.-problem search)]
    (prob/minimizing? problem)
    (- (mrp/value previous-evaluation) (mrp/value current-evaluation)),
    :else (- (mrp/value current-evaluation) (mrp/value previous-evaluation))))

;; Search callbacks

(defn search-started [{:keys [a-timestamps a-min-delta a-step-info] :as search}]
  ;; Initialize search by calling init ?
  ;; (init search)
  (reset! a-timestamps (Timestamps. (System/currentTimeMillis) -1 -1))
  (reset! a-step-info (StepInfo. 0 -1))
  (reset! min-delta -1))

(defn search-stopped [^Search search]
  (swap! (.-state search) assoc :stop-time (System/currentTimeMillis)))

(defn search-step [search])

(defn search-disposed [search])

(defn init [search])





















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
