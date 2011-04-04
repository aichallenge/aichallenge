package com.aicontest.visualizer;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Panel;

@SuppressWarnings("serial")
public class VisualizerPanel extends Panel {

	private Visualizer webWrapper;

	public VisualizerPanel(Visualizer webWrapper, int width,
			int height) {
		this.webWrapper = webWrapper;
		setPreferredSize(new Dimension(width, height));
		setFocusable(true);
	}

	public void update(Graphics g) {
		paint(g);
	}

	public synchronized void paint(Graphics g) {
		if (webWrapper != null) {
			webWrapper.paint(g);
		}
	}
}