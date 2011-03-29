package com.aicontest.visualizer;

import java.io.IOException;
import java.util.ArrayList;

import com.aicontest.visualizer.js.WebWrapper;

public class VisualizerWebWrapper extends WebWrapper
{
  private ArrayList<WebWrapper.Script> scripts = new ArrayList<WebWrapper.Script>();

  VisualizerWebWrapper(String baseDir) throws IOException, InstantiationException, IllegalAccessException, ClassNotFoundException {
    super(baseDir);
    scripts.add(loadJs("Const.js"));
    scripts.add(loadJs("Ant.js"));
    scripts.add(loadJs("Buttons.js"));
    scripts.add(loadJs("Config.js"));
    scripts.add(loadJs("Director.js"));
    scripts.add(loadJs("ImageManager.js"));
    scripts.add(loadJs("Random.js"));
    scripts.add(loadJs("Replay.js"));
    scripts.add(loadJs("Application.js"));
  }

  void runScripts() {
    for (WebWrapper.Script script : scripts)
      script.run();
  }
}