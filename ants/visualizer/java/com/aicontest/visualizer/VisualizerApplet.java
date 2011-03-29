package com.aicontest.visualizer;

import java.applet.Applet;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.net.MalformedURLException;
import java.net.URL;

import org.mozilla.javascript.ScriptableObject;

import com.aicontest.visualizer.js.WebWrapper;
import com.aicontest.visualizer.js.dom.HTMLCanvasElement;
import com.aicontest.visualizer.js.dom.HTMLDocument;

@SuppressWarnings("serial")
public class VisualizerApplet extends Applet implements Runnable {

	private URL replay;
	private Image offscreen;
	private Graphics offscreenGraphics;

	public void init() {
		try {
			replay = new URL(getDocumentBase(), getParameter("replay"));
			setBackground(Color.WHITE);
			setFocusable(true);
			new Thread(this).start();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
	}

	public synchronized void setBounds(int x, int y, int width, int height) {
		if ((width > 0) && (height > 0)) {
			offscreen = createImage(width, height);
			if (offscreen != null) {
				offscreenGraphics = offscreen.getGraphics();
			}
		}
		super.setBounds(x, y, width, height);
	}

	public void update(Graphics g) {
		paint(g);
	}

	public synchronized void paint(Graphics g) {
		g.drawImage(offscreen, 0, 0, this);
		getToolkit().sync();
	}

	public synchronized void repaint() {
		WebWrapper webWrapper = WebWrapper.getInstance();

		if (webWrapper != null) {
			HTMLCanvasElement mainCanvas = webWrapper.getMainCanvas();
			if ((mainCanvas != null) && (mainCanvas.getContext("2d").isDrawn())) {
				BufferedImage pixmap = mainCanvas.getPixmap();
				offscreenGraphics.clearRect(0, 0, getWidth(), getHeight());
				offscreenGraphics.drawImage(pixmap, 0, 0, this);
			}
		}
		super.repaint();
	}

	public String getAppletInfo() {
		return "This applet simulates a JavaScript web environment to run an aichallenge visualizer.";
	}

	public void run() {
		try {
			VisualizerWebWrapper webWrapper = new VisualizerWebWrapper(null);
			webWrapper.setDrawPanel(this);
			webWrapper.runScripts();
			HTMLDocument document = webWrapper.getDomWindow().getDocument();
			ScriptableObject vis = webWrapper.construct("Visualizer", new Object[] { document, "/" });
			webWrapper.invoke(vis, "loadReplayDataFromURI", new Object[] { replay });
			webWrapper.loop();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}