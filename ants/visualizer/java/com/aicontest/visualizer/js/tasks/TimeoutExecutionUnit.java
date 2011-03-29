package com.aicontest.visualizer.js.tasks;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.ScriptableObject;

public class TimeoutExecutionUnit extends DelayedExecutionUnit
{
  private final Object code;

  public TimeoutExecutionUnit(Function function, long delay)
  {
    super(delay);
    code = function;
  }

  public TimeoutExecutionUnit(String javaScript, long delay) {
    super(delay);
    code = javaScript;
  }

  public void execute(Context cx, ScriptableObject global)
  {
    if ((code instanceof Function))
      ((Function)code).call(cx, global, global, null);
    else
      cx.evaluateString(global, (String)code, "<anonymous>", 1, null);
  }
}