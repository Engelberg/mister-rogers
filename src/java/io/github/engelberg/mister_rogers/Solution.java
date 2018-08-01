package io.github.engelberg.mister_rogers;

public class Solution extends org.jamesframework.core.problems.sol.Solution {
    public Object o;

    public Solution(Object obj) {
	o = obj;
    }
    
    public Solution copy() {
	return this;
    }
    
    public boolean equals(Object other) {
	Solution s = (Solution) other;
	return o.equals(s.o);
    }
    
    public int hashCode() {
	return o.hashCode();
    }    
}
