package com.aicontest.visualizer;

public class Ants implements IProgram {

	static final private String[] files = { "Const.js", "Ant.js", "Buttons.js", "Config.js", "Director.js", "ImageManager.js", "Util.js", "Replay.js", "Application.js" };

	@Override
	public String[] getFiles() {
		return files;
	}
}
