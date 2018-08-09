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

package org.jamesframework.core.problems.objectives;

import org.jamesframework.core.problems.objectives.evaluations.Evaluation;
import org.jamesframework.core.exceptions.IncompatibleDeltaEvaluationException;
import org.jamesframework.core.problems.sol.Solution;
import org.jamesframework.core.search.neigh.Move;

/**
 * <p>
 * Interface of an objective function that evaluates solutions using underlying data.
 * An objective can be either maximizing or minimizing; in the former case increasing scores
 * indicate improvement, while in the latter case decreasing scores indicate improvement.
 * </p>
 * <p>
 * It is required to provide a full evaluation by implementing {@link #evaluate(Solution, Object)}.
 * If desired, an efficient delta evaluation can also be provided by overriding the default behaviour
 * of {@link #evaluate(Move, Solution, Evaluation, Object)} which (1) applies the move, (2) performs
 * a full evaluation and (3) undoes the move.
 * </p>
 * 
 * @author <a href="mailto:herman.debeukelaer@ugent.be">Herman De Beukelaer</a>
 * @param <SolutionType> solution type to be evaluated, required to extend {@link Solution}
 * @param <DataType> underlying data type
 */
public interface Objective<SolutionType extends Solution, DataType> {
    
    /**
     * Evaluates a given solution using the given data. Returns an object of type {@link Evaluation}.
     * The corresponding double value can be obtained by calling {@link Evaluation#getValue()} on this
     * evaluation object.
     * 
     * @param solution solution to evaluate
     * @param data underlying data used for evaluation
     * @return evaluation of the given solution
     */
    public Evaluation evaluate(SolutionType solution, DataType data);
    
    /**
     * <p>
     * Evaluates a move that will be applied to the current solution of a local search (delta evaluation).
     * The result corresponds to the evaluation of the modified solution that would be obtained by applying
     * the given move to the current solution. A default implementation is provided that (1) applies the move,
     * (2) computes a full evaluation by calling {@link #evaluate(Solution, Object)} and (3) undoes the applied move.
     * </p>
     * <p>
     * It is often possible to provide a custom, much more efficient delta evaluation that computes
     * the modified evaluation based on the current evaluation and the changes that will be made
     * when applying the move to the current solution. This can be done by overriding this method.
     * It is usually required to cast the received move to a specific type so that this objective
     * can only be used in combination with neighbourhoods that generate moves of this type (or a
     * subtype). If an incompatible move type is received, an {@link IncompatibleDeltaEvaluationException}
     * may be thrown.
     * </p>
     * <p>
     * Given that both this method and the full evaluation ({@link #evaluate(Solution, Object)}) return
     * evaluations of the same type, it is guaranteed that <code>curEvaluation</code> will also be of
     * this specific type and it is safe to perform a cast, if required.
     * </p>
     * 
     * @param move move to evaluate
     * @param curSolution current solution
     * @param curEvaluation evaluation of current solution
     * @param data underlying data used for evaluation
     * @param <ActualSolutionType> the actual solution type of the problem that is being solved;
     *                             a subtype of the solution types of both the objective and the applied move
     * @return evaluation of modified solution obtained when applying the move to the current solution
     * @throws IncompatibleDeltaEvaluationException if the provided delta evaluation is not compatible
     *                                              with the received move type
     */
    default public <ActualSolutionType extends SolutionType> Evaluation evaluate(Move<? super ActualSolutionType> move,
                                                                                 ActualSolutionType curSolution,
                                                                                 Evaluation curEvaluation,
                                                                                 DataType data){
        // apply move
        move.apply(curSolution);
        // full evaluation
        Evaluation e = evaluate(curSolution, data);
        // undo move
        move.undo(curSolution);
        // return evaluation
        return e;
    }
    
    /**
     * Check whether the produced evaluations are to be minimized.
     * 
     * @return <code>true</code> if evaluations are to be minimized,
     *         <code>false</code> if they are to be maximized
     */
    public boolean isMinimizing();

}
