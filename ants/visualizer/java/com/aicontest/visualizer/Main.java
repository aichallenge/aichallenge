package com.aicontest.visualizer;

import java.awt.Color;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.Panel;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.mozilla.javascript.ScriptableObject;

import com.aicontest.visualizer.js.dom.HTMLDocument;

public class Main implements IVisualizerUser, WindowListener {

	private Panel visualizerPanel;
	private Frame frame;
	private Visualizer visualizer;

	public static void main(String[] args) {
		try {
			if (args.length > 1) {
				System.out.println("The visualizer takes a replay file as its only argument. If no argument is given, stdin is used.");
			} else if (args.length == 0) {
				new Main();
			} else {
				new Main(args[0]);
			}
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
	}

	private ScriptableObject init() throws InstantiationException, IllegalAccessException, IOException {
		visualizer = new Visualizer(this, 640, 640);
		HTMLDocument document = visualizer.getDomWindow().getDocument();
		ScriptableObject options = visualizer.construct("Options", null);
		options.put("data_dir", options, "/");
		return visualizer.construct("Visualizer", new Object[] { document, options });
	}

	public Main(String replay) throws InstantiationException, IllegalAccessException, IOException, URISyntaxException {
		URI uri = null;
		try {
			uri = new URI(replay);
		} catch (URISyntaxException e) {}
		if (uri == null || uri.getScheme() == null) {
			uri = new URI("file", replay, null);
		}
		ScriptableObject vis = init();
		visualizer.invoke(vis, "loadReplayDataFromURI", new Object[] { uri });
		visualizer.loop();
	}

	public Main() throws IOException, InstantiationException, IllegalAccessException {
		new Stream(init(), visualizer, visualizer.getGlobal(), "stream");
		visualizer.loop();
	}

	@Override
	public String getJavaScriptPath() {
		return "../../js";
	}

	@Override
	public IProgram getProgram() {
		final Ants ants = new Ants();
		return ants;
	}

	@Override
	public void setVisualizerPanel(Panel visualizerPanel) {
		this.visualizerPanel = visualizerPanel;
		frame = new Frame("Ants Visualizer");
		frame.setBackground(Color.WHITE);
		frame.add(visualizerPanel);
		frame.pack();
		frame.setLocationByPlatform(true);
		frame.addWindowListener(this);
		frame.setVisible(true);
	}

	@Override
	public boolean setFullScreen(boolean enable) {
		GraphicsConfiguration winGfxConf = frame.getGraphicsConfiguration();
		GraphicsDevice dev = winGfxConf.getDevice();
		Frame fsWin = (Frame) dev.getFullScreenWindow();
		if (fsWin == null && enable) {
			frame.setVisible(false);
			fsWin = new Frame();
			fsWin.setUndecorated(true);
			fsWin.add(visualizerPanel);
			fsWin.setVisible(true);
			winGfxConf.getDevice().setFullScreenWindow(fsWin);
		} else if (fsWin != null && !enable) {
			Thread.interrupted();
			fsWin.dispose();
			winGfxConf.getDevice().setFullScreenWindow(null);
			frame.add(visualizerPanel);
			frame.setVisible(true);
		}
		return enable;
	}

	@Override
	public void windowOpened(WindowEvent e) {}

	@Override
	public void windowClosing(WindowEvent e) {
		visualizer.exit();
		e.getWindow().dispose();
	}

	@Override
	public void windowClosed(WindowEvent e) {}

	@Override
	public void windowIconified(WindowEvent e) {}

	@Override
	public void windowDeiconified(WindowEvent e) {}

	@Override
	public void windowActivated(WindowEvent e) {}

	@Override
	public void windowDeactivated(WindowEvent e) {}
}
