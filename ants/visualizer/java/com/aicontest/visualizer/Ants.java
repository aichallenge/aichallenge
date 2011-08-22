package com.aicontest.visualizer;

public class Ants implements IProgram {

	static final private String[] files = { "Util.js", "Const.js", "Ant.js", "Buttons.js", "Config.js", "Director.js", "ImageManager.js", "Replay.js", "Application.js", "CanvasElement.js" };

	@Override
	public String[] getFiles() {
		return files;
	}
}
