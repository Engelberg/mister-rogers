package io.github.engelberg.mister_rogers;
import org.jamesframework.core.problems.objectives.evaluations.Evaluation;
import org.jamesframework.core.problems.objectives.Objective;
import org.jamesframework.core.problems.objectives.evaluations.SimpleEvaluation;
import org.jamesframework.core.subset.SubsetSolution;

public class CoreSubset {
    static public Evaluation evaluate(SubsetSolution solution, double[][] data) {
	double value = 0.0;
	if(solution.getNumSelectedIDs() >= 2){
            // at least two items selected: compute average distance
	    int numDist = 0;
	    double sumDist = 0.0;
	    Integer[] selected = new Integer[solution.getNumSelectedIDs()];
	    solution.getSelectedIDs().toArray(selected);
	    for(int i=0; i<selected.length; i++){
		for(int j=i+1; j<selected.length; j++){
		    sumDist += data[selected[i]][selected[j]];
		    numDist++;
		}
	    }
	    value = sumDist/numDist;
	}
	return SimpleEvaluation.WITH_VALUE(value);
    }	
}
