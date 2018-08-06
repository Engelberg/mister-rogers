(ns mister-rogers.cache
  (:refer-clojure :exclude [cond])
  (:require [better-cond.core :refer [cond defnc]]
            [medley.core :as medley]
            [com.rpl.specter :as specter
             :refer [ALL NONE ATOM keypath select-any transform setval]]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

(defrecord MoveEvaluation [move evaluation])
(defrecord MoveValidation [move validation])
(defrecord SingleEvaluatedMoveCache [v-evaluation v-validation])

(defn single-evaluated-move-cache []
  (SingleEvaluatedMoveCache. (volatile! nil) (volatile! nil)))

(defn clear [^SingleEvaluatedMoveCache cache]
  (vreset! (.-v-evaluation cache) nil)
  (vreset! (.-v-validation cache) nil))

(defn cache-move-evaluation [^SingleEvaluatedMoveCache cache move evaluation]
  (vreset! (.-v-evaluation cache) (MoveEvaluation. move evaluation)))

(defnc get-cached-move-evaluation [^SingleEvaluatedMoveCache cache move]
  :when-let [^MoveEvaluation move-evaluation @(.-v-evaluation cache)]
  :let [evaluated-move (.-move move-evaluation)]
  (= evaluated-move move) (.-evaluation move-evaluation))

(defn cache-move-validation [^SingleEvaluatedMoveCache cache move validation]
  (vreset! (.-v-validation cache) (MoveValidation. move validation)))

(defnc get-cached-move-validation [^SingleEvaluatedMoveCache cache move]
  :when-let [^MoveValidation move-validation @(.-v-validation cache)]
  :let [validated-move (.-move move-validation)]
  (= validated-move move) (.-validation move-validation))
