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

package org.jamesframework.core.subset.neigh;

import org.jamesframework.core.subset.neigh.moves.SubsetMove;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import org.jamesframework.core.subset.SubsetSolution;
import org.jamesframework.core.subset.neigh.moves.AdditionMove;
import org.jamesframework.core.util.SetUtilities;

/**
 * <p>
 * A subset neighbourhood that generates addition moves only (see {@link AdditionMove}). An addition move is a subtype
 * of {@link SubsetMove} that adds a single ID to the selection of a subset solution. If desired, a set of fixed IDs
 * can be provided which are not allowed to be added to the selection. Also, a size limit can be imposed so that
 * no moves are generated when the current solution has maximum size.
 * </p>
 * <p>
 * Note that this neighbourhood is thread-safe: it can be safely used to concurrently generate moves in different
 * searches running in separate threads.
 * </p>
 * 
 * @author <a href="mailto:herman.debeukelaer@ugent.be">Herman De Beukelaer</a>
 */
public class SingleAdditionNeighbourhood extends SubsetNeighbourhood {
    
    // maximum subset size
    private final int maxSubsetSize;
    
    /**
     * Create a single addition neighbourhood without maximum subset size.
     * All items are candidates to be selected.
     */
    public SingleAdditionNeighbourhood(){
        this(Integer.MAX_VALUE);
    }
    
    /**
     * Create a single addition neighbourhood with a given limit on the number of selected items
     * (maximum subset size). No moves will be generated if the maximum subset size would be exceeded.
     * All items are candidates to be selected.
     * 
     * @param maxSubsetSize maximum subset size (&gt; 0)
     * @throws IllegalArgumentException if maximum subset size is not strictly positive
     */
    public SingleAdditionNeighbourhood(int maxSubsetSize){
        this(maxSubsetSize, null);
    }
    
    /**
     * Create a single addition neighbourhood with a limit on the number of selected
     * items (subset size) and a given set of fixed IDs which are not allowed to be
     * selected. None of the generated addition moves will add any of these IDs.
     * 
     * @param maxSubsetSize maximum subset size (&gt; 0)
     * @param fixedIDs set of fixed IDs which are not allowed to be added to the selection
     * @throws IllegalArgumentException if maximum subset size is not strictly positive
     */
    public SingleAdditionNeighbourhood(int maxSubsetSize, Set<Integer> fixedIDs){
        super(fixedIDs);
        // check maximum subset size
        if(maxSubsetSize <= 0){
            throw new IllegalArgumentException("Error while creating single addition neighbourhood: maximum subset size should be strictly positive.");
        }
        this.maxSubsetSize = maxSubsetSize;
    }
    
    /**
     * Get the maximum subset size. If no size limit has been applied this method returns {@link Integer#MAX_VALUE}.
     * 
     * @return maximum subset size
     */
    public int getMaxSubsetSize() {
        return maxSubsetSize;
    }
    
    /**
     * Generates a random addition move for the given subset solution that adds a single ID to the selection.
     * Possible fixed IDs are not considered to be added and the maximum subset size is taken into account.
     * If no addition move can be generated, <code>null</code> is returned.
     * 
     * @param solution solution for which a random addition move is generated
     * @param rnd source of randomness used to generate random move
     * @return random addition move, <code>null</code> if no move can be generated
     */
    @Override
    public SubsetMove getRandomMove(SubsetSolution solution, Random rnd) {
        // check size limit
        if(maxSizeReached(solution)){
            // size limit would be exceeded
            return null;
        }
        // get set of candidate IDs for addition (possibly fixed IDs are discarded)
        Set<Integer> addCandidates = getAddCandidates(solution);
        // check if addition is possible
        if(addCandidates.isEmpty()){
            return null;
        }
        // select random ID to add to selection
        int add = SetUtilities.getRandomElement(addCandidates, rnd);
        // create and return addition move
        return new AdditionMove(add);
    }

    /**
     * Generates a list of all possible addition moves that add a single ID to the selection of a given
     * subset solution. Possible fixed IDs are not considered to be added and the maximum subset size
     * is taken into account. May return an empty list if no addition moves can be generated.
     * 
     * @param solution solution for which all possible addition moves are generated
     * @return list of all addition moves, may be empty
     */
    @Override
    public List<SubsetMove> getAllMoves(SubsetSolution solution) {
        // check size limit
        if(maxSizeReached(solution)){
            return Collections.emptyList();
        }
        // get set of candidate IDs for addition (possibly fixed IDs are discarded)
        Set<Integer> addCandidates = getAddCandidates(solution);
        // check if there are any candidates to be added
        if(addCandidates.isEmpty()){
            return Collections.emptyList();
        }
        // create addition move for all add candidates
        return addCandidates.stream()
                            .map(add -> new AdditionMove(add))
                            .collect(Collectors.toList());
    }
    
    /**
     * Check whether the maximum subset size has been reached (or exceeded).
     * 
     * @param sol subset solution
     * @return <code>true</code> if the maximum size has been reached
     */
    private boolean maxSizeReached(SubsetSolution sol){
        return sol.getNumSelectedIDs() >= maxSubsetSize;
    }

}
