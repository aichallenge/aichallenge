package com.aicontest.visualizer.js.tasks;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.ScriptableObject;

public class StringExecutionUnit
  implements IExecutionUnit
{
  private String javaScript;

  public StringExecutionUnit(String javaScript)
  {
    this.javaScript = javaScript;
  }

  public void execute(Context cx, ScriptableObject global)
  {
    cx.evaluateString(global, javaScript, "<anonymous>", 1, null);
  }
}