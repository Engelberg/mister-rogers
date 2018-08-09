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
 *
 * Edited by Mark Engelberg to remove stop criteria check after every step
 */

package org.jamesframework.core.search;

import java.util.ArrayList;
import java.util.Collections;
import org.jamesframework.core.search.status.SearchStatus;
import org.jamesframework.core.search.listeners.SearchListener;
import org.jamesframework.core.search.stopcriteria.StopCriterion;
import java.util.List;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import org.jamesframework.core.exceptions.IncompatibleStopCriterionException;
import org.jamesframework.core.exceptions.JamesRuntimeException;
import org.jamesframework.core.exceptions.SearchException;
import org.jamesframework.core.problems.Problem;
import org.jamesframework.core.problems.sol.Solution;
import org.jamesframework.core.problems.constraints.validations.Validation;
import org.jamesframework.core.problems.objectives.evaluations.Evaluation;
import org.jamesframework.core.search.stopcriteria.StopCriterionChecker;
import org.jamesframework.core.util.JamesConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * General abstract search used to solve a problem with the specified solution type. It provides general methods to
 * start and stop the search, and to access state information and metadata such as the best solution found so far and
 * the runtime of the current run. It also provides methods to add and remove search listeners and stop criteria.
 * </p>
 * <p>
 * A search can have five possible statuses: IDLE, INITIALIZING, RUNNING, TERMINATING or DISPOSED
 * (see {@link SearchStatus}). When a search is created, it is IDLE. Upon calling {@link #start()}
 * it first goes to INITIALIZING and then RUNNING, after successful initialization. While the search
 * is running, it iteratively calls {@link #searchStep()} to perform a search step as defined in each
 * specific search implementation.
 * </p>
 * <p>
 * Whenever a search is requested to stop, by calling {@link #stop()}, it goes to status TERMINATING. A terminating
 * search will stop after it has completed its current step, and then its status goes back to status IDLE. A search
 * may also terminate itself by calling {@link #stop()} internally, when it has come to its natural end.
 * In particular, a single step algorithm can be implemented by calling {@link #stop()} immediately at the end of
 * the first and only step, which guarantees that only one single step will be executed.
 * </p>
 * <p>
 * An idle search may be restarted at any time. The search state is retained across subsequent runs, including the best
 * solution found so far and any search specific state elements, unless explicitely stated otherwise. On the other hand,
 * the following metadata applies to the current run only:
 * </p>
 * <ul>
 *  <li>current runtime</li>
 *  <li>current number of steps</li>
 *  <li>time without improvement</li>
 *  <li>steps without improvement</li>
 *  <li>minimum delta</li>
 * </ul>
 * <p>
 * This might also be the case for additional metadata in specific searches, which should be clearly indicated in their
 * documentation. Note that stop criteria relying on such metadata will operate on a per-run basis.
 * </p>
 * <p>
 * An idle search can also be disposed (see {@link #dispose()}), upon which it will release all of its resources.
 * A disposed search can never be restarted. Note that it is important to always dispose a search after its last
 * run so that it does not hold on to any of its resources. Not disposing a search may prevent termination of the
 * application.
 * </p>
 * 
 * @param <SolutionType> solution type of the problems that may be solved using this search,
 *                       required to extend {@link Solution}
 * @author <a href="mailto:herman.debeukelaer@ugent.be">Herman De Beukelaer</a>
 */
public abstract class Search<SolutionType extends Solution> implements Runnable {
    
    /*********************/
    /* UNIQUE ID COUNTER */
    /*********************/

    private static int nextID = 0;
    
    /**********/
    /* LOGGER */
    /**********/
    
    private static final Logger LOGGER = LoggerFactory.getLogger(Search.class);

    /******************/
    /* PRIVATE FIELDS */
    /******************/
    
    // timestamp indicating when the current (or last) run was started
    private long startTime;
    // timestamp indicating when the last run finished
    private long stopTime;
    
    // number of steps completed in the current (or last) run
    private long currentSteps;
    
    // timestamp indicating when the last improvement was made during the current (or last) run
    private long lastImprovementTime;
    
    // steps completed since last improvement during current (or last) run
    private long stepsSinceLastImprovement;
    // flags improvement during current step, used to update steps since last improvement at the end of each step
    private boolean improvementDuringCurrentStep;
    
    // minimum improvement in evaluation of a newly found best solution
    // over the previously known best solution, during the current (or last) run
    private double minDelta;
    
    // best solution found so far and its corresponding evaluation/validation
    private SolutionType bestSolution;
    private Evaluation bestSolutionEvaluation;
    private Validation bestSolutionValidation;
    
    // search status
    private SearchStatus status;
    
    // dedicated random generator
    private Random rnd;
    
    /************************/
    /* PRIVATE FINAL FIELDS */
    /************************/
    
    // search name & ID
    private final String name;
    private final int id;
    
    // problem being solved
    private final Problem<SolutionType> problem;
    
    // search listeners attached to this search (+ unmodifiable view)
    private final List<SearchListener<? super SolutionType>> searchListeners;
    private final List<SearchListener<? super SolutionType>> searchListenersView;
    
    // stop criterion checker dedicated to checking the stop criteria attached to this search
    private final StopCriterionChecker stopCriterionChecker;
    
    /*********/
    /* LOCKS */
    /*********/
    
    // lock acquired when updating the search status and when executing a block of code during which
    // the status is not allowed to change
    private final Object statusLock = new Object();
    
    /****************/
    /* CONSTRUCTORS */
    /****************/
    
    /**
     * Creates a search to solve the given problem, with default search name "Search".
     * 
     * @param problem problem to solve
     * @throws NullPointerException if <code>problem</code> is <code>null</code>
     */
    public Search(Problem<SolutionType> problem){
        this(null, problem);
    }
    
    /**
     * Creates a search to solve the given problem, with a custom search name. If
     * <code>name</code> is <code>null</code>, a default search name "Search" will
     * be assigned.
     * 
     * @throws NullPointerException if <code>problem</code> is <code>null</code>
     * @param problem problem to solve
     * @param name custom search name
     */
    @SuppressWarnings("LeakingThisInConstructor")
    public Search(String name, Problem<SolutionType> problem){
        // check problem
        if(problem == null){
            throw new NullPointerException("Error while creating search: problem can not be null.");
        }
        // store problem reference
        this.problem = problem;
        // store name
        if(name != null){
            this.name = name;
        } else {
            // no name given: default to "Search"
            this.name = "Search";
        }
        // assign next unique id
        id = getNextUniqueID();
        // initialize search listener list (+ unmodifiable view)
        searchListeners = new ArrayList<>();
        searchListenersView = Collections.unmodifiableList(searchListeners);
        // create dedicated stop criterion checker
        stopCriterionChecker = new StopCriterionChecker(this);
        // create dedicated random generator
        rnd = new Random();
        // set initial status to idle
        status = SearchStatus.IDLE;
        // initially, best solution and its evaluation/validation are null
        bestSolution = null;
        bestSolutionEvaluation = null;
        bestSolutionValidation = null;
        // initialize per-run metadata
        startTime = JamesConstants.INVALID_TIMESTAMP;
        stopTime = JamesConstants.INVALID_TIMESTAMP;
        currentSteps = JamesConstants.INVALID_STEP_COUNT;
        lastImprovementTime = JamesConstants.INVALID_TIMESTAMP;
        stepsSinceLastImprovement = JamesConstants.INVALID_STEP_COUNT;
        minDelta = JamesConstants.INVALID_DELTA;
        // initialize utility variables
        improvementDuringCurrentStep = false;
        // log search creation
        LOGGER.info("Created search {}", this);
    }
    
    /**
     * Get the next unique ID to be assigned to this search. Synchronized to ensure
     * uniqueness of IDs in multi threaded environments.
     * 
     * @return next unique ID
     */
    private synchronized int getNextUniqueID(){
        return nextID++;
    }
    
    /***********/
    /* PROBLEM */
    /***********/
    
    /**
     * Get the problem being solved, as specified at construction.
     * 
     * @return problem being solved
     */
    public Problem<SolutionType> getProblem(){
        return problem;
    }
    
    /********************/
    /* RANDOM GENERATOR */
    /********************/
    
    /**
     * Get the dedicated random generator used by this search. A custom random
     * generator can be set using {@link #setRandom(Random)}. The search should
     * pass this source of randomness to all applied randomized components.
     * 
     * @return dedicated random generator
     */
    public Random getRandom(){
        return rnd;
    }
    
    /**
     * Set custom random generator.
     * 
     * @param rnd new dedicated random generator
     */
    public void setRandom(Random rnd){
        this.rnd = rnd;
    }
    
    /***************************/
    /* NAME, ID & STRING VALUE */
    /***************************/
    
    /**
     * Get the name that has been assigned to this search.
     * 
     * @return search name
     */
    public String getName(){
        return name;
    }
    
    /**
     * Get the unique ID that has been assigned to this search.
     * 
     * @return unique search ID
     */
    public int getID(){
        return id;
    }
    
    /**
     * Returns a string representation of the search, formatted as "%name(%id)".
     * 
     * @return string representation containing name and id
     */
    @Override
    public String toString(){
        return name + "(" + id + ")";
    }
    
    /***********************/
    /* CONTROLLING METHODS */
    /***********************/
    
    /**
     * <p>
     * Initializes and/or validates the search.
     * It is not required to manually initialize a search before it is started since {@link #init()} is called from
     * within {@link #searchStarted()}. However, it may also be called prior to starting the search. For example, a
     * search composed of multiple subsearches may choose to initialize these searches prior to execution.
     * </p>
     * <p>
     * A {@link SearchException} may be thrown if initialization fails because the search has not been configured
     * validly. Moreover, any {@link JamesRuntimeException} could be thrown when initialization depends on
     * malfunctioning components.
     * </p>
     * <p>
     * When overriding this method, always call <code>super.init()</code> and take into account that the
     * method may be called multiple times prior to execution. For example, consider to perform computationally
     * expensive initializations only once, upon the first call. The default implementation is empty.
     * </p>
     * 
     * @throws SearchException if initialization fails, e.g. because the search has not been configured validly
     * @throws JamesRuntimeException in general, any {@link JamesRuntimeException} may be thrown
     *                               in case of a malfunctioning component used during initialization
     */
    public void init(){}
    
    /**
     * <p>
     * Starts a search run and returns when this run has finished. The search run may either complete internally, i.e.
     * come to its natural end, or be terminated by a stop criterion (see {@link #addStopCriterion(StopCriterion)}).
     * This method does not return anything; the best solution found during search can be obtained by calling
     * {@link #getBestSolution()} and its corresponding evaluation and validation with
     * {@link #getBestSolutionEvaluation()} and {@link #getBestSolutionValidation()}, respectively.
     * </p>
     * <p>
     * Note that a search can only be (re)started when it is idle (see {@link #getStatus()}). When attempting to start
     * a search which is already running, terminating or disposed, an exception will be thrown.
     * </p>
     * <p>
     * Before the search is actually started, some initialization may take place. This initialization can also include
     * a verification of the search configuration and in case of an invalid configuration, an exception may be thrown.
     * </p>
     * 
     * @throws SearchException if the search is currently not idle, or if initialization fails because of an invalid
     *                         search configuration
     * @throws JamesRuntimeException in general, any {@link JamesRuntimeException} may be thrown
     *                               in case of a malfunctioning component used during initialization,
     *                               execution or finalization
     */
    public void start(){
                
        // acquire status lock
        synchronized(statusLock) {
            // verify that search is idle
            assertIdle("Cannot start search.");
            // log
            LOGGER.debug("Search {} changed status: {} --> {}", this, status, SearchStatus.INITIALIZING);
            // set status to INITIALIZING
            status = SearchStatus.INITIALIZING;
            // fire status update
            fireStatusChanged(status);
        }
        
        LOGGER.info("Search {} started", this);
        
        // fire callback
        fireSearchStarted();
        
        // initialization and/or validation
        searchStarted();
        
        // check if search should be continued (may already
        // have been stopped during initialization)
        if(continueSearch()){

            // instruct stop criterion checker to start checking
            stopCriterionChecker.startChecking();

            // initialization finished: update status
            synchronized(statusLock){
                // log
                LOGGER.debug("Search {} changed status: {} --> {}", this, status, SearchStatus.RUNNING);
                // update
                status = SearchStatus.RUNNING;
                // fire status update
                fireStatusChanged(status);
            }

            // enter search loop
            while(continueSearch()){
                // reset improvement flag (automatically flipped by
                // updateBestSolution if improvement found during step)
                improvementDuringCurrentStep = false;
                // perform search step
                searchStep();
                // update step count
                currentSteps++;
                // update steps since last improvement
                if(improvementDuringCurrentStep){
                    // improvement made
                    stepsSinceLastImprovement = 0;
                } else if (stepsSinceLastImprovement != JamesConstants.INVALID_STEP_COUNT) {
                    // no improvement made now, but found improvement before in current run
                    stepsSinceLastImprovement++;
                }
                // fire callback
                fireStepCompleted(currentSteps);
                // check stop criteria
                // if(stopCriterionChecker.stopCriterionSatisfied()){
                //    stop();
                // }
            }
        
            // instruct stop criterion checker to stop checking
            stopCriterionChecker.stopChecking();
            
        }
        
        // finalization
        searchStopped();
                
        // fire callback
        fireSearchStopped();

        LOGGER.info("Search {} stopped (runtime: {} ms, steps: {})", this, getRuntime(), getSteps());
        
        // search run is complete: update status
        synchronized(statusLock){
            // log
            LOGGER.debug("Search {} changed status: {} --> {}", this, status, SearchStatus.IDLE);
            // update
            status = SearchStatus.IDLE;
            // fire status update
            fireStatusChanged(status);
        }
        
    }
   
    /**
     * <p>
     * Requests the search to stop. May be called from outside the search, e.g. by a stop criterion,
     * as well as internally, when the search comes to its natural end. In the latter case, it is
     * absolutely guaranteed that the step from which the search was requested to stop will be the
     * last step executed during the current run. If the current search status is not
     * {@link SearchStatus#INITIALIZING} or {@link SearchStatus#RUNNING}, calling this method has
     * no effect. Else, it changes the search status to {@link SearchStatus#TERMINATING}.
     * </p>
     * <p>
     * In case the search is already requested to terminate during initialization, it will complete
     * initialization, but is guaranteed to stop before executing any search steps.
     * </p>
     */
    public void stop(){
        // acquire status lock
        synchronized(statusLock){
            // check current status
            if(status == SearchStatus.INITIALIZING || status == SearchStatus.RUNNING){
                // log
                LOGGER.debug("Search {} changed status: {} --> {}", this, status, SearchStatus.TERMINATING);
                // update status
                status = SearchStatus.TERMINATING;
                // fire status update
                fireStatusChanged(status);
            }
        }
    }
    
    /**
     * Dispose this search, upon which all of its resources are released. Note that only idle
     * searches may be disposed and that a disposed search can never be restarted. Sets the search
     * status to {@link SearchStatus#DISPOSED}. When trying to dispose an already disposed search,
     * nothing happens, i.e. calling this method on a disposed search has no effect.
     * 
     * @throws SearchException if the search is currently not idle (and not already disposed)
     */
    public void dispose(){
        // acquire status lock
        synchronized(statusLock){
            // abort if already disposed
            if(status == SearchStatus.DISPOSED){
                return;
            }
            // assert idle
            assertIdle("Cannot dispose search.");
            // all good, handle disposed
            searchDisposed();
            // log
            LOGGER.debug("Search {} changed status: {} --> {}", this, status, SearchStatus.DISPOSED);
            // update status
            status = SearchStatus.DISPOSED;
            // fire status update
            fireStatusChanged(status);
        }
    }
    
    /***************************/
    /* RUNNABLE IMPLEMENTATION */
    /***************************/
    
    /**
     * Equivalent to calling {@link #start()}. Through this method searches implement the {@link Runnable} interface
     * so that they can easily be executed in a separate thread.
     */
    @Override
    public void run(){
        start();
    }
    
    /*********************************************************/
    /* METHODS FOR ADDING STOP CRITERIA AND SEARCH LISTENERS */
    /*********************************************************/
    
    /**
     * Adds a stop criterion used to decide when the search should stop running. It is verified whether the given
     * stop criterion is compatible with the search and if not, an exception is thrown. Note that this method can
     * only be called when the search is idle.
     * 
     * @param stopCriterion stop criterion used to decide when the search should stop running
     * @throws IncompatibleStopCriterionException if the given stop criterion is incompatible with the search
     * @throws SearchException if the search is not idle
     */
    public void addStopCriterion(StopCriterion stopCriterion){
        // acquire status lock
        synchronized(statusLock){
            // assert idle
            assertIdle("Cannot add stop criterion.");
            // pass stop criterion to checker (throws error if incompatible)
            stopCriterionChecker.add(stopCriterion);
            // log
            LOGGER.debug("{}: added stop criterion {}", this, stopCriterion);
        }
    }
    
    /**
     * Removes a stop criterion. In case this stop criterion had not been added, <code>false</code> is returned.
     * Note that this method may only be called when the search is idle.
     * 
     * @param stopCriterion stop criterion to be removed
     * @return <code>true</code> if the stop criterion has been successfully removed
     * @throws SearchException if the search is not idle
     */
    public boolean removeStopCriterion(StopCriterion stopCriterion){
        // acquire status lock
        synchronized(statusLock){
            // assert idle
            assertIdle("Cannot remove stop criterion.");
            // remove from checker
            if (stopCriterionChecker.remove(stopCriterion)){
                // log
                LOGGER.debug("{}: removed stop criterion {}", this, stopCriterion);
                return true;
            } else {
                return false;
            }
        }
    }
    
    /**
     * Removes all stop criteria.
     * Note that this method may only be called when the search is idle.
     * 
     * @throws SearchException if the search is not idle
     */
    public void clearStopCriteria(){
        // acquire status lock
        synchronized(statusLock){
            // assert idle
            assertIdle("Cannot clear stop criteria.");
            // remove all stop criteria from checker
            stopCriterionChecker.clear();
            // log
            LOGGER.debug("{}: cleared stop criteria", this);
        }
    }
    
    /**
     * Instructs the search to check its stop criteria at regular intervals separated by the given period.
     * For the default period, see {@link StopCriterionChecker}, which is used internally for this purpose.
     * The period should be at least 1 millisecond, else the stop criterion checker may thrown an exception
     * when the search is started. Note that this method may only be called when the search is idle.
     * <p>
     * Regardless of the specified period, the stop criteria are also checked after each search step.
     * 
     * @param period time between subsequent stop criterion checks (&gt; 0)
     * @param timeUnit corresponding time unit
     * @throws SearchException if the search is not idle
     * @throws IllegalArgumentException if the given period is not strictly positive
     */
    public void setStopCriterionCheckPeriod(long period, TimeUnit timeUnit){
        // acquire status lock
        synchronized(statusLock){
            // assert idle
            assertIdle("Cannot modify stop criterion check period.");
            // pass new settings to checker
            stopCriterionChecker.setPeriod(period, timeUnit);
            // log
            LOGGER.debug("{}: set stop criterion check period to {} ms", this, timeUnit.toMillis(period));
        }
    }
    
    /**
     * Add a search listener. Any search listener with a matching solution type (or a more general solution type)
     * may be added. Note that this method can only be called when the search is idle.
     * 
     * @param listener search listener to add to the search
     * @throws SearchException if the search is not idle
     */
    public void addSearchListener(SearchListener<? super SolutionType> listener){
        // acquire status lock
        synchronized(statusLock){
            // assert idle
            assertIdle("Cannot add search listener.");
            // add listener
            searchListeners.add(listener);
            // log
            LOGGER.debug("{}: added search listener {}", this, listener);
        }
    }
    
    /**
     * Remove the given search listener. If the search listener had not been added, <code>false</code> is returned.
     * Note that this method may only be called when the search is idle.
     * 
     * @param listener search listener to be removed
     * @return <code>true</code> if the listener was present and has now been removed
     * @throws SearchException if the search is not idle
     */
    public boolean removeSearchListener(SearchListener<? super SolutionType> listener){
        // acquire status lock
        synchronized(statusLock){
            // assert idle
            assertIdle("Cannot remove search listener.");
            // remove listener
            if (searchListeners.remove(listener)){
                // log
                LOGGER.debug("{}: removed search listener {}", this, listener);
                return true;
            } else {
                return false;
            }
        }
    }
    
    /**
     * Remove all search listeners.
     * Note that this method may only be called when the search is idle.
     */
    public void clearSearchListeners(){
        // acquire status lock
        synchronized(statusLock){
            // assert idle
            assertIdle("Cannot clear search listeners.");
            // clear listeners
            searchListeners.clear();
            // log
            LOGGER.debug("{}: cleared search listeners", this);
        }
    }
    
    /********************************************************/
    /* PRIVATE METHODS FOR FIRING SEARCH LISTENER CALLBACKS */
    /********************************************************/

    /**
     * Calls {@link SearchListener#searchStarted(Search)} on every attached search listener.
     * Should only be executed when search is active (initializing, running or terminating).
     */
    private void fireSearchStarted(){
        for(SearchListener<? super SolutionType> l : searchListeners){
            l.searchStarted(this);
        }
    }
    
    /**
     * Calls {@link SearchListener#searchStopped(Search)} on every attached search listener.
     * Should only be executed when search is active (initializing, running or terminating).
     */
    private void fireSearchStopped(){
        for(SearchListener<? super SolutionType> l : searchListeners){
            l.searchStopped(this);
        }
    }
    
    /**
     * Calls {@link SearchListener#newBestSolution(Search, Solution, Evaluation, Validation)}
     * on every attached search listener. Should only be executed when search is active
     * (initializing, running or terminating).
     * 
     * @param newBestSolution new best solution
     * @param newBestSolutionEvaluation evaluation of new best solution
     * @param newBestSolutionValidation validation of new best solution
     */
    private void fireNewBestSolution(SolutionType newBestSolution,
                                     Evaluation newBestSolutionEvaluation,
                                     Validation newBestSolutionValidation){
        for(SearchListener<? super SolutionType> l : searchListeners){
            l.newBestSolution(this, newBestSolution,
                                    newBestSolutionEvaluation,
                                    newBestSolutionValidation);
        }
    }
    
    /**
     * Calls {@link SearchListener#stepCompleted(Search, long)} on every attached search listener.
     * Should only be executed when search is active (initializing, running or terminating).
     * 
     * @param numSteps number of steps completed so far (during the current run)
     */
    private void fireStepCompleted(long numSteps){
        for(SearchListener<? super SolutionType> l : searchListeners){
            l.stepCompleted(this, numSteps);
        }
    }
    
    /**
     * Calls {@link SearchListener#statusChanged(Search, SearchStatus)} on every attached search listener.
     * Should only be called exactly once for every status update.
     * 
     * @param newStatus new search status
     */
    private void fireStatusChanged(SearchStatus newStatus){
        for(SearchListener<? super SolutionType> l : searchListeners){
            l.statusChanged(this, newStatus);
        }
    }
    
    /************************************/
    /* PROTECTED SEARCH LISTENER ACCESS */
    /************************************/
    
    /**
     * Retrieve search listeners (unmodifiable view).
     * 
     * @return attached search listeners (unmodifiable list view)
     */
    protected List<SearchListener<? super SolutionType>> getSearchListeners(){
        return searchListenersView;
    }
    
    /*****************/
    /* SEARCH STATUS */
    /*****************/

    /**
     * Get the current search status. The status may be IDLE, INITIALIZING, RUNNING, TERMINATING or DISPOSED.
     * 
     * @return current search status
     */
    public SearchStatus getStatus(){
        // synchronize with status updates
        synchronized(statusLock){
            return status;
        }
    }
    
    /**
     * Returns a lock to be acquired when executing blocks of code that can not be interrupted by a status update.
     * All status updates are synchronized using this lock. Whenever a fixed status is required during execution of
     * a code block, this can be obtained by synchronizing the block using this status lock.
     * 
     * @return status lock used for synchronization with status updates
     */
    protected Object getStatusLock(){
        return statusLock;
    }
    
    /**
     * Asserts that the search is currently idle, more precisely that its status is equal to {@link SearchStatus#IDLE}.
     * If not, a {@link SearchException} is thrown, which includes the given <code>errorMessage</code> and the current
     * search status (different from IDLE).
     * 
     * @param errorMessage message to be included in the {@link SearchException} thrown if the search is not idle
     * @throws SearchException if the search is not idle
     */
    protected void assertIdle(String errorMessage){
        // synchronize with status updates
        synchronized(statusLock){
            if(status != SearchStatus.IDLE){
                // not idle, throw exception
                throw new SearchException(errorMessage + " (current status: " + status  + "; required: IDLE)");
            }
        }
    }
    
    /******************************************/
    /* STATE ACCESSORS (RETAINED ACROSS RUNS) */
    /******************************************/
    
    /**
     * Returns the best solution found so far. It is guaranteed that this solution is valid.
     * The best solution is <b>retained</b> across subsequent runs of the same search. May
     * return <code>null</code> if no valid solutions have been evaluated yet, for example
     * when the search has just been created.
     * 
     * @return best solution found so far, if already defined; <code>null</code> otherwise
     */
    public SolutionType getBestSolution(){
        return bestSolution;
    }
    
    /**
     * Get the evaluation of the best solution found so far. The best solution is <b>retained</b>
     * across subsequent runs of the same search. May return <code>null</code> if no best solution
     * has yet been set, for example when the search has just been created.
     * 
     * @return evaluation of best solution, if already defined; <code>null</code> otherwise
     */
    public Evaluation getBestSolutionEvaluation(){
        return bestSolutionEvaluation;
    }
    
    /**
     * Get the validation of the best solution found so far. The best solution is <b>retained</b>
     * across subsequent runs of the same search. May return <code>null</code> if no best solution
     * has yet been set, for example when the search has just been created.
     * 
     * @return validation of best solution, if already defined; <code>null</code> otherwise
     */
    public Validation getBestSolutionValidation(){
        return bestSolutionValidation;
    }
    
    /****************************/
    /* PROTECTED STATE MUTATORS */
    /****************************/
    
    /**
     * <p>
     * Checks whether a new best solution has been found and updates it accordingly.
     * The given solution is evaluated and validated, after which the best solution
     * is updated only if the solution is valid and
     * </p>
     * <ul>
     *  <li>no best solution had been set before, or</li>
     *  <li>the new solution has a better evaluation</li>
     * </ul>
     * <p>
     * If the new solution is invalid or has a worse evaluation than the current best
     * solution, calling this method has no effect. Note that the best solution is
     * <b>retained</b> across subsequent runs of the same search.
     * </p>
     * 
     * @param newSolution newly presented solution
     * @return <code>true</code> if the given solution is accepted as the new best solution
     */
    protected boolean updateBestSolution(SolutionType newSolution){
        // validate solution
        Validation validation = getProblem().validate(newSolution);
        if(validation.passed()){
            // passed validation: evaluate
            Evaluation evaluation = getProblem().evaluate(newSolution);
            // update if better
            return updateBestSolution(newSolution, evaluation, validation);
        } else {
            // invalid solution
            return false;
        }
    }
    
    /**
     * <p>
     * Checks whether a new best solution has been found and updates it accordingly,
     * given that the solution has already been evaluated and validated. The best
     * solution is updated only if the presented solution is valid and
     * </p>
     * <ul>
     *  <li>no best solution had been set before, or</li>
     *  <li>the new solution has a better evaluation</li>
     * </ul>
     * <p>
     * If the new solution is invalid or has a worse evaluation than the current best
     * solution, calling this method has no effect. Note that the best solution is
     * <b>retained</b> across subsequent runs of the same search.
     * </p>
     * 
     * @param newSolution newly presented solution
     * @param newSolutionEvaluation evaluation of given solution
     * @param newSolutionValidation validation of given solution
     * @return <code>true</code> if the given solution is accepted as the new best solution
     */
    protected boolean updateBestSolution(SolutionType newSolution,
                                         Evaluation newSolutionEvaluation,
                                         Validation newSolutionValidation){
        // check: valid solution
        if(newSolutionValidation.passed()){
            // check: improvement or no best solution set
            Double delta = null;
            if(bestSolution == null
                    || (delta = computeDelta(newSolutionEvaluation, getBestSolutionEvaluation())) > 0){
                // flag improvement
                improvementDuringCurrentStep = true;
                // store last improvement time
                lastImprovementTime = System.currentTimeMillis();
                // update minimum delta if applicable (only if first delta or smaller than current minimum delta)
                if(delta != null && (minDelta == JamesConstants.INVALID_DELTA || delta < minDelta)){
                    minDelta = delta;
                }
                // update best solution (create copy!)
                bestSolution = Solution.checkedCopy(newSolution);
                bestSolutionEvaluation = newSolutionEvaluation;
                bestSolutionValidation = newSolutionValidation;
                // fire callback
                fireNewBestSolution(bestSolution, bestSolutionEvaluation, bestSolutionValidation);
                // found improvement
                return true;
            } else {
                // no improvement
                return false;
            }
        } else {
            // invalid solution
            return false;
        }
    }

    /*****************************************/
    /* METADATA APPLYING TO CURRENT RUN ONLY */
    /*****************************************/
    
    /**
     * <p>
     * Get the runtime of the <i>current</i> (or last) run, in milliseconds. The precise return value
     * depends on the status of the search:
     * </p>
     * <ul>
     *  <li>
     *   If the search is either RUNNING or TERMINATING, this method returns the time elapsed since
     *   the current run was started.
     *  </li>
     *  <li>
     *   If the search is IDLE or DISPOSED, the total runtime of the last run is returned, if any. Before
     *   the first run, {@link JamesConstants#INVALID_TIME_SPAN} is returned.
     *  </li>
     *  <li>
     *   While INITIALIZING the current run, {@link JamesConstants#INVALID_TIME_SPAN} is returned.
     *  </li>
     * </ul>
     * <p>
     * The return value is always positive, except in those cases when
     * {@link JamesConstants#INVALID_TIME_SPAN} is returned.
     * </p>
     * 
     * @return runtime of the current (or last) run, in milliseconds
     */
    public long getRuntime(){
        // depends on status: synchronize with status updates
        synchronized(statusLock){
            if(status == SearchStatus.INITIALIZING){
                // initializing
                return JamesConstants.INVALID_TIME_SPAN;
            } else if (status == SearchStatus.IDLE || status == SearchStatus.DISPOSED){
                // idle or disposed: check if ran before
                if(stopTime == JamesConstants.INVALID_TIMESTAMP){
                    // did not run before
                    return JamesConstants.INVALID_TIME_SPAN;
                } else {
                    // return total runtime of last run
                    return stopTime - startTime;
                }
            } else {
                // running or terminating
                return System.currentTimeMillis() - startTime;
            }
        }
    }
    
    /**
     * <p>
     * Get the number of completed steps during the <i>current</i> (or last) run. The precise return value
     * depends on the status of the search:
     * </p>
     * <ul>
     *  <li>
     *   If the search is either RUNNING or TERMINATING, this method returns the number of steps completed
     *   since the current run was started.
     *  </li>
     *  <li>
     *   If the search is IDLE or DISPOSED, the total number of steps completed during the last run is returned,
     *   if any. Before the first run, {@link JamesConstants#INVALID_STEP_COUNT}.
     *  </li>
     *  <li>
     *   While INITIALIZING the current run, {@link JamesConstants#INVALID_STEP_COUNT} is returned.
     *  </li>
     * </ul>
     * <p>
     * The return value is always positive, except in those cases when {@link JamesConstants#INVALID_STEP_COUNT}
     * is returned.
     * </p>
     * 
     * @return number of steps completed during the current (or last) run
     */
    public long getSteps(){
        // depends on status: synchronize with status updates
        synchronized(statusLock){
            if(status == SearchStatus.INITIALIZING){
                // initializing
                return JamesConstants.INVALID_STEP_COUNT;
            } else {
                // idle, running, terminating or disposed
                return currentSteps;
            }
        }
    }
    
    /**
     * <p>
     * Get the amount of time elapsed during the <i>current</i> (or last) run, without finding any further improvement
     * (in milliseconds). The precise return value depends on the status of the search:
     * </p>
     * <ul>
     *  <li>
     *   If the search is either RUNNING or TERMINATING, but no improvements have yet been made during the current
     *   run, the returned value is equal to the current runtime; else it reflects the time elapsed since the last
     *   improvement during the current run.
     *  </li>
     *  <li>
     *   If the search is IDLE or DISPOSED, but has been run before, the time without improvement observed during the
     *   last run, up to the point when this run completed, is returned. Before the first run, the return value is
     *   {@link JamesConstants#INVALID_TIME_SPAN}.
     *  </li>
     *  <li>
     *   While INITIALIZING the current run, {@link JamesConstants#INVALID_TIME_SPAN} is returned.
     *  </li>
     * </ul>
     * <p>
     * The return value is always positive, except in those cases when {@link JamesConstants#INVALID_TIME_SPAN}
     * is returned.
     * </p>
     * 
     * @return time without finding improvements during the current (or last) run, in milliseconds
     */
    public long getTimeWithoutImprovement(){
        // depends on status: synchronize with status updates
        synchronized(statusLock){
            if(status == SearchStatus.INITIALIZING){
                // initializing
                return JamesConstants.INVALID_TIME_SPAN;
            } else {
                // idle, running, terminating or disposed: check last improvement time
                if(lastImprovementTime == JamesConstants.INVALID_TIMESTAMP){
                    // no improvement made during current/last run, or did not yet run: equal to total runtime
                    return getRuntime();
                } else {
                    // running or ran before, and improvement made during current/last run
                    if(status == SearchStatus.IDLE || status == SearchStatus.DISPOSED){
                        // idle or disposed: return last time without improvement of previous run
                        return stopTime - lastImprovementTime;
                    } else {
                        // running or terminating: return time elapsed since last improvement
                        return System.currentTimeMillis() - lastImprovementTime;
                    }
                }
            }
        }
    }
    
    /**
     * <p>
     * Get the number of consecutive steps completed during the <i>current</i> (or last) run, without finding
     * any further improvement. The precise return value depends on the status of the search:
     * </p>
     * <ul>
     *  <li>
     *   If the search is either RUNNING or TERMINATING, but no improvements have yet been made during the current
     *   run, the returned value is equal to the total number of steps completed so far; else it reflects the number
     *   of steps completed since the last improvement during the current run.
     *  </li>
     *  <li>
     *   If the search is IDLE or DISPOSED, but has been run before, the number of steps without improvement observed
     *   during the last run, up to the point when this run completed, is returned. Before the first run, the return
     *   value is {@link JamesConstants#INVALID_STEP_COUNT}.
     *  </li>
     *  <li>
     *   While INITIALIZING the current run, {@link JamesConstants#INVALID_STEP_COUNT} is returned.
     *  </li>
     * </ul>
     * <p>
     * The return value is always positive, except in those cases when {@link JamesConstants#INVALID_STEP_COUNT}
     * is returned.
     * </p>
     * 
     * @return number of consecutive completed steps without finding improvements during the current (or last) run
     */
    public long getStepsWithoutImprovement(){
        // depends on status: synchronize with status updates
        synchronized(statusLock){
            if(status == SearchStatus.INITIALIZING){
                // initializing
                return JamesConstants.INVALID_STEP_COUNT;
            } else {
                if(stepsSinceLastImprovement == JamesConstants.INVALID_STEP_COUNT){
                    // no improvement made during current/last run, or not yet run: equal to total step count
                    return getSteps();
                } else {
                    // running or ran before, and improvement made during current/last run
                    return stepsSinceLastImprovement;
                }
            }
        }
    }
    
    /**
     * <p>
     * Get the minimum improvement in evaluation of a new best known solution over the previous best known solution,
     * found during the <i>current</i> (or last) run. The precise return value depends on the status of the search:
     * </p>
     * <ul>
     *  <li>
     *   If the search is either RUNNING or TERMINATING, but no improvements have yet been made during the current
     *   run, {@link JamesConstants#INVALID_DELTA} is returned. Else, the minimum observed delta over all improvements
     *   made during the current run is returned.
     *  </li>
     *  <li>
     *  If the search is IDLE or DISPOSED, but has been run before, the minimum delta observed during the last
     *  run is returned. Before the first run, the return value is {@link JamesConstants#INVALID_DELTA}.
     *  </li>
     *  <li>
     *   While INITIALIZING the current run, {@link JamesConstants#INVALID_DELTA} is returned.
     *  </li>
     * </ul>
     * <p>
     * The return value is always positive, except in those cases when {@link JamesConstants#INVALID_DELTA} is returned.
     * It corresponds to increase when solving a maximization problem, and decrease in case of a minimization problem.
     * </p>
     * 
     * @return minimum delta of improvements observed during current (or last) run
     */
    public double getMinDelta(){
        // depends on status: synchronize with status updates
        synchronized(statusLock){
            if(status == SearchStatus.INITIALIZING){
                // initializing
                return JamesConstants.INVALID_DELTA;
            } else {
                // idle, running or terminating
                return minDelta;
            }
        }
    }
    
    /*********************/
    /* PRIVATE UTILITIES */
    /*********************/
    
    /**
     * Indicates whether the search should continue, by verifying whether its status is not yet set to
     * {@link SearchStatus#TERMINATING}. Once the search has been started, this method will return
     * <code>true</code> as long as {@link #stop()} has not been called. During that time, {@link #searchStep()}
     * will be repeatedly called from the main search loop which uses {@link #continueSearch()} as its
     * stop condition.
     * 
     * @return <code>true</code> if the search status is not {@link SearchStatus#TERMINATING}
     */
    private boolean continueSearch(){
        return status != SearchStatus.TERMINATING;
    }
    
    /***********************/
    /* PROTECTED UTILITIES */
    /***********************/
    
    /**
     * Computes the amount of improvement of <code>currentEvaluation</code> over <code>previousEvaluation</code>,
     * taking into account whether evaluations are being maximized or minimized. Positive deltas indicate improvement.
     * In case of maximization the amount of increase is returned, which is equal to
     *  <pre> currentEvaluation - previousEvaluation </pre>
     * while the amount of decrease, equal to
     *  <pre> previousEvaluation - currentEvaluation </pre>
     * is returned in case of minimization.
     * 
     * @param currentEvaluation evaluation to be compared with previous evaluation
     * @param previousEvaluation previous evaluation
     * @return amount of improvement of current evaluation over previous evaluation
     */
    protected double computeDelta(Evaluation currentEvaluation, Evaluation previousEvaluation){
        if(problem.isMinimizing()){
            // minimization: return decrease
            return previousEvaluation.getValue() - currentEvaluation.getValue();
        } else {
            // maximization: return increase
            return currentEvaluation.getValue() - previousEvaluation.getValue();
        }
    }
        
    /*****************************************************************************/
    /* PROTECTED METHODS CALLED WHEN THE SEARCH IS STARTED, STOPPED OR DISPOSED  */
    /*****************************************************************************/
    
    /**
     * <p>
     * This method is called when a search run is started.
     * It first initializes and validates the search by calling {@link #init()}, which may throw a
     * {@link SearchException} if initialization fails because the search has not been configured validly.
     * Moreover, any {@link JamesRuntimeException} could be thrown in case initialization depends on
     * malfunctioning components.
     * After initialization, all general per run metadata is (re)set, such as the step count and execution time.
     * </p>
     * <p>
     * Always call <code>super.searchStarted()</code> when overriding this method.
     * Note however that it is preferred to override {@link #init()} instead.
     * </p>
     * 
     * @throws SearchException if initialization fails, e.g. because the search has not been configured validly
     * @throws JamesRuntimeException in general, any {@link JamesRuntimeException} may be thrown
     *                               in case of a malfunctioning component used during initialization
     */
    protected void searchStarted() {
        // initialize
        init();
        // (re)set metadata
        startTime = System.currentTimeMillis();
        stopTime = JamesConstants.INVALID_TIMESTAMP;
        currentSteps = 0;
        lastImprovementTime = JamesConstants.INVALID_TIMESTAMP;
        stepsSinceLastImprovement = JamesConstants.INVALID_STEP_COUNT;
        minDelta = JamesConstants.INVALID_DELTA;
    }
    
    /**
     * <p>
     * This method is called when a search run has completed. It may be used to perform some finalization. Any
     * {@link JamesRuntimeException} may be thrown when finalization depends on malfunctioning search components.
     * The default implementation ensures that the total runtime of the last run, if applicable, will be returned
     * when calling {@link #getRuntime()} on an idle search.
     * </p>
     * <p>
     * Always call <code>super.searchStopped()</code> when overriding this method. 
     * </p>
     * 
     * @throws JamesRuntimeException in general, any {@link JamesRuntimeException} may be thrown
     *                               in case of a malfunctioning component used during finalization
     */
    protected void searchStopped() {
        stopTime = System.currentTimeMillis();
    }
    
    /**
     * <p>
     * This method is called when a search is disposed, immediately before the search status is updated.
     * The default implementation is empty but should be overridden when a specific search uses resources that have to
     * be released (e.g. an active thread pool) when the search is no longer used. Any {@link JamesRuntimeException} may
     * be thrown when trying to release malfunctioning resources.
     * </p>
     * <p>
     * Always call <code>super.searchDisposed()</code> when overriding this method. 
     * </p>
     * 
     * @throws JamesRuntimeException in general, any {@link JamesRuntimeException} may be thrown
     *                               when trying to release malfunctioning resources
     */
    protected void searchDisposed(){}
    
    /************************************************************************/
    /* ABSTRACT PROTECTED METHOD TO BE IMPLEMENTED IN EVERY SPECIFIC SEARCH */
    /************************************************************************/

    /**
     * This method is iteratively called while the search is running and should be implemented in every specific
     * search according to the corresponding search strategy. When a search comes to its natural end, it should call
     * {@link #stop()} from within this method, which will cause the search loop to terminate and prevent further
     * steps to be executed. Searches consisting of a single step may simply implement their strategy here and
     * immediately call {@link #stop()} at the end of the execution of the step, so that it will be executed
     * exactly once.
     * 
     * @throws JamesRuntimeException any {@link JamesRuntimeException} may be thrown during a search step, when
     *                               the algorithm has been supplied with a malfunctioning component
     */
    abstract protected void searchStep();
    
}
