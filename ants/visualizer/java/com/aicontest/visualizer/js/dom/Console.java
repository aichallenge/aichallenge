package com.aicontest.visualizer.js.dom;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.IdFunctionObject;
import org.mozilla.javascript.IdScriptableObject;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;
import org.mozilla.javascript.Undefined;

@SuppressWarnings("serial")
public class Console extends IdScriptableObject {

	private static final int Id_log = 1, MAX_ID = 1;

	public Console(Scriptable scope, String name) {
		activatePrototypeMap(MAX_ID);
		setPrototype(getObjectPrototype(scope));
		setParentScope(scope);
		ScriptableObject.defineProperty(scope, name, this, ScriptableObject.DONTENUM);
	}

	@Override
	public String getClassName() {
		return "Console";
	}

	public void log(Object[] args) {
		if (args.length > 0) {
			if (args[0] == null) {
				System.out.println("null");
			} else {
				System.out.println(args[0].toString());
			}
		}
	}

	@Override
	protected int findPrototypeId(String s) {
		if ("log".equals(s)) {
			return Id_log;
		}
		return 0;
	}

	@Override
	protected void initPrototypeId(int id) {
		String name;
		int arity;
		switch (id) {
		case Id_log:
			arity = 1;
			name = "log";
			break;
		default:
			throw new IllegalStateException(String.valueOf(id));
		}
		initPrototypeMethod(null, id, name, arity);
	}

	@Override
	public Object execIdCall(IdFunctionObject f, Context cx, Scriptable scope, Scriptable thisObj, Object[] args) {
		int methodId = f.methodId();
		switch (methodId) {
		case Id_log:
			log(args);
			return Undefined.instance;
		default:
			throw new IllegalStateException(String.valueOf(methodId));
		}
	}
}
