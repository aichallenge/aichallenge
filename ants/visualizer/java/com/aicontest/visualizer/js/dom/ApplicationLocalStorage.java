package com.aicontest.visualizer.js.dom;

import java.util.prefs.Preferences;
import org.mozilla.javascript.Scriptable;

public class ApplicationLocalStorage extends LocalStorage {

	private static final long serialVersionUID = 1683737711225680165L;
	private Preferences storage;

	public ApplicationLocalStorage() {
		storage = Preferences.userRoot();
	}

	public void put(String name, Scriptable start, Object value) {
		storage.put(name, (String) value);
	}

	public Object get(String name, Scriptable start) {
		return storage.get(name, null);
	}
}
