/*
 * Copyright 2014 Ghent University, Bayer CropScience.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jamesframework.core.search.stopcriteria;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import org.jamesframework.core.exceptions.IncompatibleStopCriterionException;
import org.jamesframework.core.search.Search;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A stop criterion checker is responsible for checking the stop criteria of a given search, while this
 * search is running. At construction, a reference to the search is given. Stop criteria can be added
 * using {@link #add(StopCriterion)}, which usually only happens from within the search, which passes
 * all its stop criteria to its dedicated checker.
 * <p>
 * When {@link #startChecking()} is called, a task will be scheduled to periodically check the stop
 * criteria in the background, by default with a period of 1 second, while the search is running.
 * These tasks are executed by a dedicated thread which is shared among all stop criterion checker
 * instances, as checking stop criteria is not an intensive task. A manual check can be performed
 * with {@link #stopCriterionSatisfied()}.
 * <p>
 * As soon as some stop criterion is satisfied, the checker will cancel the scheduled task for
 * further execution and will request the search to stop.
 *
 * @author <a href="mailto:herman.debeukelaer@ugent.be">Herman De Beukelaer</a>
 */
public class StopCriterionChecker {

    // scheduled executor service, shared by all checkers
    // (single thread as stop criterion checking is not intensive)
    private static final ScheduledExecutorService SCHEDULER = Executors.newSingleThreadScheduledExecutor(runnable -> {
        Thread t = new Thread(runnable, "stop-crit-checker");
        t.setDaemon(true);
        return t;
    });
    
    // currently scheduled check task, null if not running
    private StopCriterionCheckTask runningTask;
    // future of running check task, null if not running
    private ScheduledFuture<?> runningTaskFuture;
    // lock used for synchronization with running task updates
    private final Object runningTaskLock = new Object();
    
    // logger
    private static final Logger LOGGER = LoggerFactory.getLogger(StopCriterionChecker.class);

    // period between consecutive checks, and corresponding time unit
    private long period;
    private TimeUnit periodTimeUnit;

    // search for which the stop criteria have to be checked
    private final Search<?> search;

    // list of stop criteria
    private final List<StopCriterion> stopCriteria;

    /**
     * Create a stop criterion checker dedicated to checking the stop criteria of the given search.
     * By default, consecutive checks will be separated with a period of 1 second.
     * This can be customized using {@link #setPeriod(long, TimeUnit)}.
     *
     * @param search search for which the stop criteria should be checked while running
     */
    public StopCriterionChecker(Search<?> search) {
        // store search
        this.search = search;
        // initialize stop criterion list
        stopCriteria = new ArrayList<>();
        // set default period to 1 second
        period = 1;
        periodTimeUnit = TimeUnit.SECONDS;
        // not active
        runningTask = null;
    }

    /**
     * Add a stop criterion to check.
     *
     * @param stopCriterion stop criterion to add
     * @throws IncompatibleStopCriterionException if the given stop criterion is not compatible with the search
     *                                            to which this checker belongs
     */
    public void add(StopCriterion stopCriterion) {
        // check compatibility through a fake call (throws error if incompatible)
        stopCriterion.searchShouldStop(search);
        // add to list
        stopCriteria.add(stopCriterion);
    }

    /**
     * Remove a stop criterion. Returns <code>false</code> if the given stop criterion had never been added.
     *
     * @param stopCriterion stop criterion to remove
     * @return <code>true</code> if the stop criterion has successfully been removed
     */
    public boolean remove(StopCriterion stopCriterion) {
        return stopCriteria.remove(stopCriterion);
    }
    
    /**
     * Remove all stop criteria.
     */
    public void clear(){
        stopCriteria.clear();
    }

    /**
     * Set the period between consecutive stop criterion checks. By default, this period is set to 1 second.
     * The new settings will apply as from the next call of {@link #startChecking()}.
     *
     * @param period period between two checks
     * @param timeUnit time unit of this period
     */
    public void setPeriod(long period, TimeUnit timeUnit) {
        this.period = period;
        periodTimeUnit = timeUnit;
    }

    /**
     * Start checking the stop criteria, in a separate background thread. If the stop criterion checker is
     * already active, or if no stop criteria have been added, calling this method does not have any effect.
     */
    public void startChecking() {
        // synchronize with other attempts to update the running task
        synchronized(runningTaskLock){
            // check if not already active
            if(runningTask == null) {
                // only activate if at least one stop criterion has been set
                if(!stopCriteria.isEmpty()){
                    // schedule periodical check
                    runningTask = new StopCriterionCheckTask();
                    runningTaskFuture = SCHEDULER.scheduleWithFixedDelay(runningTask, period, period, periodTimeUnit);
                    // log
                    LOGGER.debug("Stop criterion checker for search {} activated", search);
                }
            } else {
                // issue a warning
                LOGGER.warn("Attempted to activate already active stop criterion checker for search {}", search);
            }
        }
    }

    /**
     * Instructs the stop criterion checker to stop checking. In case the checker is not active,
     * calling this method does not have any effect.
     */
    public void stopChecking() {
        // synchronize with other attempts to update the running task
        synchronized(runningTaskLock){
            if(runningTask != null) {
                // cancel task (let it complete its current run if running)
                runningTaskFuture.cancel(false);
                // log
                LOGGER.debug("Stop criterion checker for search {} deactivated", search);
                // discard task
                runningTask = null;
                runningTaskFuture = null;
            }
        }
    }
    
    /**
     * Check whether at least one stop criterion is satisfied.
     * 
     * @return <code>true</code> if a stop condition is satisfied
     */
    public boolean stopCriterionSatisfied(){
        int i = 0;
        while (i < stopCriteria.size() && !stopCriteria.get(i).searchShouldStop(search)) {
            i++;
        }
        return i < stopCriteria.size();
    }

    /**
     * Runnable submitted to scheduler for checking the stop criteria.
     */
    private class StopCriterionCheckTask implements Runnable {
        
        /**
         * Check stop criteria.
         */
        @Override
        public void run() {
            
            // if a stop criterion is satisfied, the search is requested to stop, but ONLY IF this task
            // is still running: it might have been cancelled during its current run, in which case no
            // actions should be taken anymore (stopping the search could be dangerous because a next
            // search run might have been initiated in the meantime)
            
            // synchronize on running task updates to ensure that this task
            // is not cancelled while executing the following code block
            synchronized(runningTaskLock){
                if (runningTask == this) {
                    if(stopCriterionSatisfied()){
                        // stop checking
                        stopChecking();
                        // log
                        LOGGER.debug("Requesting search {} to stop", search);
                        // stop the search
                        search.stop();
                    }
                } else {
                    if(LOGGER.isDebugEnabled()){
                        // log
                        LOGGER.debug("Aborting cancelled stop criterion check task @{} for search {} "
                                     + "(currently scheduled task: @{})",
                                     Integer.toHexString(this.hashCode()),
                                     search,
                                     Integer.toHexString(runningTask.hashCode()));
                    }
                }
            }
            
        }

    }

}
