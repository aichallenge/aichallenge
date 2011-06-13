package com.aicontest.visualizer.js.tasks;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

public class EventExecutionUnit implements IExecutionUnit {

	protected Object thiz;
	protected String property;
	private Object[] args;

	public EventExecutionUnit(Object thiz, String function, Object[] args) {
		if (thiz == null) {
			throw new IllegalArgumentException("thiz must not be null.");
		}
		this.thiz = thiz;
		property = function;
		this.args = args;
	}

	protected Scriptable getThiz(Context cx, ScriptableObject global) {
		if (thiz instanceof Scriptable) {
			return (Scriptable) thiz;
		}
		return (Scriptable) cx.getWrapFactory().wrap(cx, global, thiz, null);
	}

	protected Function getFunction(Scriptable scriptable) {
		Object obj = ScriptableObject.getProperty(scriptable, property);
		if (obj instanceof Function) {
			return (Function) obj;
		}
		return null;
	}

	public void execute(Context cx, ScriptableObject global) {
		Scriptable scriptable = getThiz(cx, global);
		Function func = getFunction(scriptable);
		if (func != null) {
			func.call(cx, global, scriptable, args == null ? Context.emptyArgs : args);
		}
	}

	public boolean matches(Object thiz, String function) {
		return (this.thiz == thiz) && (property.equals(function));
	}

	@Override
	public String toString() {
		String result = thiz + "." + property + "(";
		if (args != null) {
			for (int i = 0; i < args.length; i++) {
				if (i > 0) {
					result += ", ";
				}
				result += args[i];
			}
		}
		return result + ")";
	}
}
