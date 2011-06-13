package com.aicontest.visualizer.js.tasks;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

public class TimeoutExecutionUnit extends DelayedExecutionUnit {

	private final Object code;
	private Scriptable thiz;
	private Object[] args;

	public TimeoutExecutionUnit(Function function, long delay) {
		super(delay);
		code = function;
	}

	public TimeoutExecutionUnit(String javaScript, long delay) {
		super(delay);
		code = javaScript;
	}

	public TimeoutExecutionUnit(Scriptable thiz, String function, Object[] args, long delay) {
		super(delay);
		this.thiz = thiz;
		this.code = (Function) ScriptableObject.getProperty(thiz, function);
		this.args = args;
	}

	public void execute(Context cx, ScriptableObject global) {
		if ((code instanceof Function))
			((Function) code).call(cx, global, thiz == null ? global : thiz, args);
		else
			cx.evaluateString(global, (String) code, "<anonymous>", 1, null);
	}
}
