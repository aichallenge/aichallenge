package com.aicontest.visualizer.js.tasks;

import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;

public class FunctionExecutionUnit extends EventExecutionUnit {

	public FunctionExecutionUnit(Object thiz, String function, Object[] args) {
		super(thiz, function, args);
	}

	@Override
	protected Function getFunction(Scriptable scriptable) {
		Function func = super.getFunction(scriptable);
		if (func == null) {
			throw new IllegalArgumentException(property + " is not a callable of " + thiz);
		}
		return func;
	}
}
