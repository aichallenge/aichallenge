package com.aicontest.visualizer;

import java.awt.Panel;

public interface IVisualizerUser {

	/**
	 * If this visualizer should be allowed to compile the JavaScript sources at
	 * runtime, it needs the source directory.
	 * 
	 * @return the JavaScript source directory or null
	 */
	String getJavaScriptPath();

	/**
	 * Returns the program the visualizer should load. A program is a list of
	 * JavaScript files.
	 * 
	 * @return the program to load
	 */
	IProgram getProgram();

	/**
	 * This sets the panel on which the visualization will be displayed. This
	 * method will be called only once at construction time of the visualizer,
	 * since the panel never changes.
	 * 
	 * @param visualizerPanel
	 *            the panel
	 */
	void setVisualizerPanel(Panel visualizerPanel);

	/**
	 * Enables or disables fullscreen mode.
	 * 
	 * @param enable
	 *            if true, you should go into fullscreen mode and resize the
	 *            draw panel accordingly.
	 * @return whether the switch succeeded
	 */
	boolean setFullScreen(boolean enable);

}
