package com.aicontest.visualizer.js.tasks;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

public class EventExecutionUnit implements IExecutionUnit {
	private Object thiz;
	private String property;
	private Object[] args;

	public EventExecutionUnit(Object thiz, String function, Object[] args) {
		if (thiz == null) {
			throw new IllegalArgumentException("thiz must not be null.");
		}
		this.thiz = thiz;
		property = function;
		this.args = args;
	}

	public void execute(Context cx, ScriptableObject global) {
		Scriptable scriptable;
		if ((thiz instanceof Scriptable))
			scriptable = (Scriptable) thiz;
		else {
			scriptable = (Scriptable) cx.getWrapFactory().wrap(cx, global, thiz, null);
		}
		Object obj = scriptable.get(property, scriptable);
		if ((obj instanceof Function))
			((Function) obj).call(cx, global, scriptable, args);
	}

	public boolean matches(Object thiz, String function) {
		return (this.thiz == thiz) && (property.equals(function));
	}
}