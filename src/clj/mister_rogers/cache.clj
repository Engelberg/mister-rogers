(ns mister-rogers.cache
  (:refer-clojure :exclude [cond])
  (:require [better-cond.core :refer [cond defnc]]
            [medley.core :as medley]
            [com.rpl.specter :as specter
             :refer [ALL NONE ATOM keypath select-any transform setval]]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]])
  (:import io.github.engelberg.mister_rogers.SingleEvaluatedMoveCache))

(defn single-evaluated-move-cache []
  (SingleEvaluatedMoveCache.))

(defn clear [^SingleEvaluatedMoveCache cache]
  (.clear cache))

(defn cache-move-evaluation [^SingleEvaluatedMoveCache cache move evaluation]
  (.cacheMoveEvaluation cache move evaluation))

(defn get-cached-move-evaluation [^SingleEvaluatedMoveCache cache move]
  (.getCachedMoveEvaluation cache move))

(defn cache-move-validation [^SingleEvaluatedMoveCache cache move validation]
  (.cacheMoveValidation cache move validation))

(defnc get-cached-move-validation [^SingleEvaluatedMoveCache cache move]
  (.getCachedMoveValidation cache move))
