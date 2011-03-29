package com.aicontest.visualizer;

import java.awt.Color;
import java.awt.Dimension;
import java.net.URI;
import java.net.URISyntaxException;

import javax.swing.JFrame;

import org.mozilla.javascript.ScriptableObject;

import com.aicontest.visualizer.js.DrawPanel;
import com.aicontest.visualizer.js.dom.HTMLDocument;

public class Main {

	public static void main(String[] args) {
		try {
			if (args.length != 1) {
				System.out.println("The visualizer takes a replay file as its only argument.");
			} else {
				String replay = args[0];
				JFrame frame = new JFrame("Ants Visualizer");
				frame.setBackground(Color.WHITE);
				frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
				DrawPanel drawPanel = new DrawPanel();
				drawPanel.setPreferredSize(new Dimension(640, 640));
				drawPanel.setFocusable(true);
				frame.getContentPane().add(drawPanel, "Center");
				frame.pack();
				frame.setLocationByPlatform(true);
				frame.setVisible(true);
				VisualizerWebWrapper webWrapper = new VisualizerWebWrapper("../../js");
				webWrapper.setDrawPanel(drawPanel);
				webWrapper.runScripts();
				HTMLDocument document = webWrapper.getDomWindow().getDocument();
				ScriptableObject vis = webWrapper.construct("Visualizer", new Object[] { document, "/" });
				URI uri = null;
				try {
					uri = new URI(replay);
				} catch (URISyntaxException e) {
				}
				if (uri == null || uri.getScheme() == null) {
					uri = new URI("file", replay, null);
				}
				webWrapper.invoke(vis, "loadReplayDataFromURI", new Object[] { uri });
				webWrapper.loop();
			}
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
	}
	
}