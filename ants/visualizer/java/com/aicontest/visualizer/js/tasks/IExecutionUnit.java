package com.aicontest.visualizer.js.tasks;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.ScriptableObject;

public abstract interface IExecutionUnit
{
  public abstract void execute(Context paramContext, ScriptableObject paramScriptableObject);
}