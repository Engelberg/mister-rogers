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
	if (o instanceof IHashEq) {
	    return Util.equiv(o,s.o);
	}
	else {
	    return o.equals(s.o);
	}
    }
    
    public int hashCode() {
	if (o instanceof IHashEq) {
	    return (IHashEq)o.hasheq();
	}
	else {
	    return o.hashCode();
	}
    }    
}
