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

package org.jamesframework.core.problems.constraints.validations;

/**
 * A simple penalizing validation object that wraps a boolean value,
 * indicating whether the corresponding solution passed validation,
 * and a double value, indicating the assigned penalty if the solution
 * did not pass validation.
 * 
 * @author <a href="mailto:herman.debeukelaer@ugent.be">Herman De Beukelaer</a>
 */
public class SimplePenalizingValidation implements PenalizingValidation {

    /**
     * Constant simple penalizing validation indicating that the validation passed successfully
     * ({@link #passed()} returns <code>true</code> and no penalty is assigned).
     */
    public static final SimplePenalizingValidation PASSED = new SimplePenalizingValidation(true, -1);
    
    /**
     * Creates a simple penalizing validation indicating that the validation failed, where
     * the given strictly positive penalty is assigned.
     * 
     * @param penalty assigned penalty
     * @return simple penalizing validation where {@link #passed()} returns <code>false</code>
     *         and {@link #getPenalty()} returns the given strictly positive penalty
     * @throws IllegalArgumentException if <code>penalty</code> is not strictly positive
     */
    public static final SimplePenalizingValidation FAILED(double penalty){
        return new SimplePenalizingValidation(false, penalty);
    }
    
    // contained boolean value
    private final boolean passed;
    // assigned penalty
    private final double penalty;

    /**
     * Create a simple penalizing validation with a given boolean value, indicating
     * whether the corresponding solution passed validation, and an assigned penalty.
     * If <code>passed</code> is <code>true</code> the value of <code>penalty</code>
     * is ignored and the penalty is set to 0. In such case, <code>penalty</code> can
     * be set to any value, e.g. -1. If <code>passed</code> is <code>false</code> it is
     * required that <code>penalty</code> is a strictly positive value.
     * 
     * @param passed boolean value, <code>true</code> if the corresponding solution is valid
     * @param penalty assigned penalty, ignored if <code>passed</code> is <code>true</code>
     * @throws IllegalArgumentException if <code>passed</code> is <code>false</code> but
     *                                  <code>penalty</code> is not strictly positive
     */
    public SimplePenalizingValidation(boolean passed, double penalty) {
        this.passed = passed;
        if(passed){
            this.penalty = 0.0;
        } else {
            if(penalty > 0.0){
                this.penalty = penalty;
            } else {
                // invalid penalty
                throw new IllegalArgumentException("The assigned penalty should be strictly positive.");
            }
        }
    }
    
    /**
     * Get the boolean value specified at construction, indicating whether the corresponding solution
     * passed validation. If this method returns <code>true</code>, {@link #getPenalty()} returns 0.0.
     * 
     * @return boolean value, <code>true</code> if the corresponding solution is valid
     */
    @Override
    public boolean passed() {
        return passed;
    }
    
    /**
     * Get the assigned penalty. If {@link #passed()} returns <code>false</code> this method returns
     * the strictly positive penalty specified at construction. Else, it returns 0.0.
     * 
     * @return assigned penalty
     */
    @Override
    public double getPenalty(){
        return penalty;
    }
    
    /**
     * Get a string representation of the validation object. Indicates
     * whether the the solution passed validation and the assigned penalty
     * (only if any).
     * 
     * @return string representation
     */
    @Override
    public String toString(){
        if(passed()){
            return "valid";
        } else {
            return "invalid (penalty: " + getPenalty() + ")";
        }        
    }

}
