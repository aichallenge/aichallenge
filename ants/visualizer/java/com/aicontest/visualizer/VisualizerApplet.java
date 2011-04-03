package com.aicontest.visualizer;

import java.applet.Applet;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Panel;
import java.net.MalformedURLException;
import java.net.URL;

import netscape.javascript.JSObject;

import org.mozilla.javascript.ScriptableObject;

import com.aicontest.visualizer.js.dom.HTMLDocument;

@SuppressWarnings("serial")
public class VisualizerApplet extends Applet implements Runnable,
		IVisualizerUser {

	private URL replay;
	private Thread thread;
	private Visualizer webWrapper;

	@Override
	public void init() {
		try {
			replay = new URL(getDocumentBase(), getParameter("replay"));
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
		setBackground(Color.WHITE);
		setFocusable(true);
	}

	@Override
	public void start() {
		if (thread == null) {
			thread = new Thread(this);
			thread.start();
		}
	}

	public void update(Graphics g) {
		paint(g);
	}

	public synchronized void paint(Graphics g) {
		if (webWrapper != null) {
			webWrapper.paint(g);
		}
	}

	public String getAppletInfo() {
		return "This applet simulates a JavaScript web environment to run an aichallenge visualizer.";
	}

	public void run() {
		try {
			Thread.interrupted();
			final JSObject jsRoot = JSObject.getWindow(this);
			webWrapper = new Visualizer(this, getWidth(), getHeight());
			webWrapper.setJsRoot(jsRoot);
			HTMLDocument document = webWrapper.getDomWindow().getDocument();
			ScriptableObject vis = webWrapper.construct("Visualizer",
					new Object[] { document, "/" });
			webWrapper.invoke(vis, "loadReplayDataFromURI",
					new Object[] { replay });
			addKeyListener(webWrapper);
			webWrapper.loop();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public String getJavaScriptPath() {
		return null;
	}

	@Override
	public IProgram getProgram() {
		final Ants ants = new Ants();
		return ants;
	}

	@Override
	public void setVisualizerPanel(Panel visualizerPanel) {
		visualizerPanel.setSize(getWidth(), getHeight());
		if (Thread.currentThread().isInterrupted()) {
			System.out.println("pre add");
		}
		add(visualizerPanel);
	}

	@Override
	public boolean isFullScreenSupported() {
		return false;
	}

	@Override
	public boolean setFullScreen(boolean enable) {
		return false;
	}

}