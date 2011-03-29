package com.aicontest.visualizer.js.tasks;

import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;

public abstract class DelayedExecutionUnit
  implements IExecutionUnit, Delayed
{
  private long time;

  public DelayedExecutionUnit(long delay)
  {
    time = (System.currentTimeMillis() + delay);
  }

  public int compareTo(Delayed o)
  {
    if ((o instanceof DelayedExecutionUnit)) {
      return (int)(time - ((DelayedExecutionUnit)o).time);
    }
    return -1;
  }

  public long getDelay(TimeUnit unit)
  {
    return unit.convert(time - System.currentTimeMillis(), TimeUnit.MILLISECONDS);
  }
}