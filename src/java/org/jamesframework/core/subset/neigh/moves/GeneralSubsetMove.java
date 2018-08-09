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

package org.jamesframework.core.subset.neigh.moves;

import java.util.Collections;
import java.util.Set;
import org.jamesframework.core.exceptions.SolutionModificationException;
import org.jamesframework.core.subset.SubsetSolution;

/**
 * A general subset move may simultaneously add and/or remove
 * several IDs to/from the current selection of a subset solution.
 * 
 * @author <a href="mailto:herman.debeukelaer@ugent.be">Herman De Beukelaer</a>
 */
public class GeneralSubsetMove extends AbstractSubsetMove {

    // set of IDs to add and remove
    private final Set<Integer> add;
    private final Set<Integer> delete;
    
    /**
     * Creates a general subset move, indicating which IDs are to be added to and deleted from
     * the current selection when being applied to a given subset solution. Note that the sets
     * <code>add</code> and <code>delete</code> are <b>not</b> copied: a reference to the given
     * sets is stored in the subset move. If any of both sets is <code>null</code>, it will be
     * replaced with an empty set.
     * 
     * @param add set of IDs to add to the selection
     * @param delete set of IDs to delete from the selection
     */
    public GeneralSubsetMove(Set<Integer> add, Set<Integer> delete){
        // store sets of added/deleted IDs (empty set in case of null)
        this.add = (add == null ? Collections.emptySet() : add);
        this.delete = (delete == null ? Collections.emptySet() : delete);
    }
    
    /**
     * Returns an unmodifiable view of the set of IDs added to the selection when applying
     * this move to a subset solution. The returned set may be empty.
     * 
     * @return set of added IDs
     */
    @Override
    public Set<Integer> getAddedIDs() {
        return Collections.unmodifiableSet(add);
    }

    /**
     * Returns an unmodifiable view of the set of IDs removed from the selection when applying
     * this move to a subset solution. The returned set may be empty.
     * 
     * @return set of removed IDs
     */
    @Override
    public Set<Integer> getDeletedIDs() {
        return Collections.unmodifiableSet(delete);
    }

    /**
     * Returns the number of added IDs. May be zero.
     * 
     * @return number of added IDs
     */
    @Override
    public int getNumAdded() {
        return add.size();
    }

    /**
     * Returns the number of removed IDs. May be zero.
     * 
     * @return number of removed IDs
     */
    @Override
    public int getNumDeleted() {
        return delete.size();
    }

    /**
     * Apply this move to a given subset solution. The move can only be applied to a solution
     * for which none of the added IDs are currently already selected and none of the removed
     * IDs are currently not selected. This guarantees that calling {@link #undo(SubsetSolution)}
     * will correctly undo the move.
     * 
     * @throws SolutionModificationException if some added ID is already selected, some removed ID is currently
     *                                       not selected, or any ID does not correspond to an underlying entity
     * @param solution solution to which to move will be applied
     */
    @Override
    public void apply(SubsetSolution solution) {
        // add IDs
        for(int ID : add){
            if(!solution.select(ID)){
                throw new SolutionModificationException("Cannot add ID " + ID + " to selection (already selected).", solution);
            }
        }
        // remove IDs
        for(int ID : delete){
            if(!solution.deselect(ID)){
                throw new SolutionModificationException("Cannot remove ID " + ID + " from selection (currently not selected).", solution);
            }
        }
    }

    /**
     * Undo this move after it has been successfully applied to the given subset solution,
     * by removing all added IDs and adding all removed IDs. If the subset solution has been
     * modified since successful application of this move, the result of this operation is
     * undefined.
     * 
     * @param solution solution to which the move has been successfully applied
     */
    @Override
    public void undo(SubsetSolution solution) {
        // remove all added IDs
        solution.deselectAll(add);
        // add all removed IDs
        solution.selectAll(delete);
    }

}
