(ns mister-rogers.criteria
  (:refer-clojure :exclude [cond])
  (:require [better-cond.core :refer [cond defnc]]
            [medley.core :as medley]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [mister-rogers.protocols :as mrp]
            [mister-rogers.search :as search])
  (:import java.util.concurrent.Executors
           java.util.concurrent.ScheduledExecutorService
           java.util.concurrent.ScheduledFuture
           java.util.concurrent.ThreadFactory
           java.util.concurrent.TimeUnit))

;; Specific stop criteria

(defrecord MaxRuntime [^long max-runtime-millis]
  mrp/StopCriterion
  (search-should-stop? [this search]
    (>= (search/get-runtime search) max-runtime-millis)))
         
(defnc max-runtime [^long time ^TimeUnit unit]
  :let [max-runtime-millis (.toMillis unit time)]
  (<= max-runtime-millis 0)
  (throw (ex-info "Error while creating stop criterion: maximum runtime should be at least 1 millisecond."
                  {:max-runtime-millis max-runtime-millis}))
  (MaxRuntime. max-runtime-millis))

(defrecord MaxSteps [^long max-steps]
  mrp/StopCriterion
  (search-should-stop? [this search]
    (>= (search/get-steps search) max-steps)))

(defnc max-steps [^long max-steps]
  (<= max-steps 0)
  (throw (ex-info "Error while creating stop criterion: maximum number of steps should be > 0." {:max-steps max-steps}))
  (MaxSteps. max-steps))

(defrecord MaxStepsWithoutImprovement [^long max-steps-without-improvement]
  mrp/StopCriterion
  (search-should-stop? [this search]
    (>= (search/get-steps-without-improvement) max-steps-without-improvement)))

(defnc max-steps-without-improvement [^long max-steps-without-improvement]
  (<= max-steps-without-improvement 0)
  (throw (ex-info "Error while creating stop criterion: maximum number of steps without improvement should be > 0."
                  {:max-steps-without-improvement max-steps-without-improvement}))
  (MaxStepsWithoutImprovement. max-steps-without-improvement))

(defrecord MaxTimeWithoutImprovement [^long max-time-without-improvement-millis]
  mrp/StopCriterion
  (search-should-stop? [this search]
    (>= (search/get-time-without-improvement search)
        max-time-without-improvement-millis)))
         
(defnc max-time-without-improvement-millis [^long time ^TimeUnit unit]
  :let [max-time-without-improvement-millis (.toMillis unit time)]
  (<= max-time-without-improvement-millis 0)
  (throw (ex-info "Error while creating stop criterion: maximum time without improvement should be at least 1 millisecond."
                  {:max-time-without-improvement-millis max-time-without-improvement-millis}))
  (MaxTimeWithoutImprovement. max-time-without-improvement-millis))

(defrecord MinDelta [^double min-delta]
  mrp/StopCriterion
  (search-should-stop? [this search]
    (let [current-min-delta (search/get-min-delta)]
      (and (not= current-min-delta -1.0)    
           (< current-min-delta min-delta)))))

(defnc min-delta [^double min-delta]
  (<= min-delta 0)
  (throw (ex-info "Error while creating stop criterion: minimum delta should be > 0.0." {:min-delta min-delta}))
  (MinDelta. min-delta))




  
