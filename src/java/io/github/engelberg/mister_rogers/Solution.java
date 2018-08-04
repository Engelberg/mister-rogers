package io.github.engelberg.mister_rogers;
import clojure.lang.IHashEq;
import clojure.lang.Util;

public class Solution extends org.jamesframework.core.problems.sol.Solution {
    public Object o;
    public Object undo;

    public Solution(Object obj) {
	o = obj;
	undo = null;
    }
    
    public Solution copy() {
	return this;
    }
    
    public boolean equals(Object other) {
	Solution s = (Solution) other;
	return Util.equiv(o,s.o);
    }
    
    public int hashCode() {
	return Util.hasheq(o);
    }    
}
