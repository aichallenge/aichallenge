package com.aicontest.visualizer.js.dom;

import org.mozilla.javascript.ScriptableObject;

public abstract class LocalStorage extends ScriptableObject {

	private static final long serialVersionUID = -346676629272965584L;

	public String getClassName() {
		return "Storage";
	}
}