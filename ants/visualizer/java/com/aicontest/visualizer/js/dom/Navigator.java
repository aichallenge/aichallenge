package com.aicontest.visualizer.js.dom;

import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

@SuppressWarnings("serial")
public class Navigator extends ScriptableObject {

	public Navigator(Scriptable scope, String name) {
		setPrototype(getObjectPrototype(scope));
		setParentScope(scope);
		ScriptableObject.defineProperty(scope, name, this, ScriptableObject.DONTENUM);
		ScriptableObject.defineProperty(this, "userAgent", "Java wrapper", ScriptableObject.CONST);
		ScriptableObject.defineProperty(this, "appVersion", "1", ScriptableObject.CONST);
	}

	@Override
	public String getClassName() {
		return "Navigator";
	}
}
