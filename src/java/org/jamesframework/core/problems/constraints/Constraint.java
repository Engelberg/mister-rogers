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

package org.jamesframework.core.problems.constraints;

import org.jamesframework.core.problems.constraints.validations.Validation;
import org.jamesframework.core.exceptions.IncompatibleDeltaValidationException;
import org.jamesframework.core.problems.sol.Solution;
import org.jamesframework.core.search.neigh.Move;

/**
 * <p>
 * Interface of a constraint that can be imposed on the solutions of a problem.
 * </p>
 * <p>
 * It is required to provide a full validation by implementing {@link #validate(Solution, Object)}.
 * If desired, an efficient delta validation can also be provided by overriding the default behaviour
 * of {@link #validate(Move, Solution, Validation, Object)} which (1) applies the move, (2) performs
 * a full validation and (3) undoes the move.
 * </p>
 * 
 * @author <a href="mailto:herman.debeukelaer@ugent.be">Herman De Beukelaer</a>
 * @param <SolutionType> solution type to which the constraint is applied, required to extend {@link Solution}
 * @param <DataType> underlying data type
 */
public interface Constraint<SolutionType extends Solution, DataType> {

    /**
     * Validates a solution given the underlying data. Returns an object of type {@link Validation}.
     * It can be checked wether the solution passed validation by calling {@link Validation#passed()}
     * on this validation object.
     * 
     * @param solution solution to validate
     * @param data underlying data used for validation
     * @return a validation object that indicates wether the solution passed validation
     */
    public Validation validate(SolutionType solution, DataType data);
    
    /**
     * <p>
     * Validates a move that will be applied to the current solution of a local search (delta validation).
     * The result corresponds to the validation of the modified solution that would be obtained by applying
     * the given move to the current solution. A default implementation is provided that (1) applies the move,
     * (2) performs a full validation by calling {@link #validate(Solution, Object)} and (3) undoes the applied move.
     * </p>
     * <p>
     * It is often possible to provide a custom, much more efficient delta validation based on the
     * current validation and the changes that will be made when applying the move to the current
     * solution. This can be done by overriding this method. It is usually required to cast the
     * received move to a specific type so that this constraint can only be used in combination
     * with neighbourhoods that generate moves of this type (or a subtype). If an incompatible
     * move type is received, an {@link IncompatibleDeltaValidationException} may be thrown.
     * </p>
     * <p>
     * Given that both this method and the full validation ({@link #validate(Solution, Object)}) return
     * validations of the same type, it is guaranteed that <code>curValidation</code> will also be of
     * this specific type and it is safe to perform a cast, if required.
     * </p>
     * 
     * @param move move to validate
     * @param curSolution current solution
     * @param curValidation validation of current solution
     * @param data underlying data used for validation
     * @param <ActualSolutionType> the actual solution type of the problem that is being solved;
     *                             a subtype of the solution types of both the constraint and the applied move
     * @return validation of modified solution obtained when applying the move to the current solution
     * @throws IncompatibleDeltaValidationException if the provided delta validation is not compatible
     *                                              with the received move type
     */
    default public <ActualSolutionType extends SolutionType> Validation validate(Move<? super ActualSolutionType> move,
                                                                                 ActualSolutionType curSolution,
                                                                                 Validation curValidation,
                                                                                 DataType data){
        // apply move
        move.apply(curSolution);
        // full validation
        Validation v = validate(curSolution, data);
        // undo move
        move.undo(curSolution);
        // return validation
        return v;
    }
    
}
