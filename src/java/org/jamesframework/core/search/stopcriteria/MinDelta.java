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

import org.jamesframework.core.search.Search;
import org.jamesframework.core.util.JamesConstants;

/**
 * Stop criterion that imposes a minimum delta (amount of improvement in evaluation)
 * when finding a new best solution during a search run, compared to the previous
 * best known solution. Note that it is <i>not</i> absolutely guaranteed that a search
 * will ever stop when using only this stop criterion, as it may happen that the last
 * improvement is still above the required minimum and no more improvements are found
 * from that point in time.
 * 
 * @author <a href="mailto:herman.debeukelaer@ugent.be">Herman De Beukelaer</a>
 */
public class MinDelta implements StopCriterion {

    // minimum delta
    private final double minDelta;
    
    /**
     * Create a stop criterion that imposes a minimum evaluation delta when
     * finding a new best solution during a search run. The given minimum
     * delta should be strictly positive.
     * 
     * @param minDelta minimum evaluation delta (&gt; 0.0)
     * @throws IllegalArgumentException if <code>minDelta</code> is not &gt; 0.0
     */
    public MinDelta(double minDelta){
        // check value
        if(minDelta <= 0.0){
            throw new IllegalArgumentException("Error while creating stop criterion: minimum delta should be > 0.0.");
        }
        this.minDelta = minDelta;
    }
    
    /**
     * Checks whether the minimum delta observed during the current run of the given
     * search is still above the required minimum.
     * 
     * @param search search for which the minimum delta has to be checked
     * @return <code>true</code> in case of a minimum delta below the required minimum
     */
    @Override
    public boolean searchShouldStop(Search<?> search) {
        return search.getMinDelta() != JamesConstants.INVALID_DELTA && search.getMinDelta() < minDelta;
    }
    
    @Override
    public String toString(){
        return "{min delta: " + minDelta + "}";
    }

}
