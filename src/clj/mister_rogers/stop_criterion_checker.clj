(ns mister-rogers.stop-criterion-checker
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
            [mister-rogers.problem :as prob])
  (:import java.util.concurrent.Executors
           java.util.concurrent.ScheduledExecutorService
           java.util.concurrent.ScheduledFuture
           java.util.concurrent.ThreadFactory
           java.util.concurrent.TimeUnit))

(declare start-checking stop-checking stop-criterion-satisfied?)

(def scheduler (Executors/newSingleThreadScheduledExecutor
                (reify ThreadFactory
                  (newThread [this runnable]
                    (let [t (Thread. runnable "stop-crit-checker")]
                      (.setDaemon t true)
                      t)))))

(defrecord StopCriterionChecker [stop-criteria, ^long period,
                                 ^TimeUnit period-time-unit, running])

(defrecord Running [^Runnable running-task ^ScheduledFuture running-task-future])

(defn stop-criterion-checker
  ([stop-criteria] (stop-criterion-checker stop-criteria 1 TimeUnit/SECONDS))
  ([stop-criteria period period-time-unit]
   (StopCriterionChecker. stop-criteria period period-time-unit
                          (atom (map->Running {})))))

(defn stop-criterion-satisfied?
  [^StopCriterionChecker stop-criterion-checker search]
  (some (fn [stop-criterion] (mrp/search-should-stop? stop-criterion search))
        (.-stop-criteria stop-criterion-checker)))

(declare stop-checking)
(defn stop-criterion-check-task
  [{:keys [stop-criteria] :as stop-criterion-checker} search stop]
  (fn []
    (when (stop-criterion-satisfied? stop-criterion-checker search)
      (stop-checking stop-criterion-checker search)
      (debugf "Requesting search (%s:%d) to stop" (:name search) (:id search))
      (stop search))))

(defn start-checking [{:keys [stop-criteria period period-time-unit running]
                       :as stop-criterion-checker}
                      search stop]
  (swap! running
         (fn [{:keys [running-task running-task-future]}]
           (cond
             running-task (do (warnf "Attempted to activate already active stop criterion checker for search %s" search) (Running. running-task running-task-future))
             (empty? stop-criteria) nil
             :let [running-task
                   (stop-criterion-check-task stop-criterion-checker search stop),
                   running-task-future
                   (.scheduleWithFixedDelay ^ScheduledExecutorService scheduler
                                            running-task period period
                                            period-time-unit)]
             :do (debugf "Stop criterion checker for search %s activated" search)
             (Running. running-task running-task-future)))))

  

(defn stop-checking [{:keys [running] :as stop-criterion-checker} search]
  (swap! running
         (fn [{:keys [running-task ^ScheduledFuture running-task-future]}]
           (cond
             (nil? running-task) nil
             :do (.cancel running-task-future false)
             :do (debugf "Stop criterion checker for search %s deactivated" search)
             (Running. nil nil)))))
