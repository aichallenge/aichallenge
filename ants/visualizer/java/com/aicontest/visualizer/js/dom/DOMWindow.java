package com.aicontest.visualizer.js.dom;

import java.applet.Applet;
import java.awt.Container;

import org.mozilla.javascript.Function;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.aicontest.visualizer.Visualizer;
import com.aicontest.visualizer.js.tasks.DelayedExecutionUnit;
import com.aicontest.visualizer.js.tasks.TimeoutExecutionUnit;

public class DOMWindow {
	private String output = "";
	private Visualizer webWrapper;
	private Location location;
	private HTMLDocument document;
	private Function onresize;
	private LocalStorage localStorage;

	public DOMWindow(String name, Visualizer webWrapper) {
		this.webWrapper = webWrapper;
		location = new Location(webWrapper);
		document = new HTMLDocument(webWrapper);
	}

	void contentChanged() {
		final String RED_MARKER = "<font color=\"red\">";
		final String BLACK_MARKER = "</font>";
		NodeList nodes = document.getChildNodes();
		for (int i = 0; i < nodes.getLength(); i++) {
			Node node = nodes.item(i);
			if ((node instanceof HTMLDivElement)) {
				String msg = ((HTMLElement) node).getInnerHTML();
				if (msg.startsWith(output)) {
					int l = output.length();
					output = msg;
					msg = msg.substring(l);
				} else {
					output = msg;
				}
				msg = msg.replaceAll("\n", "");
				msg = msg.replaceAll("<br>|<table[^>]*>|</tr>", "\n");
				msg = msg.replaceAll("</td>", "\t");
				int redIdx;
				int blackIdx;
				do {
					redIdx = msg.indexOf(RED_MARKER);
					String black = redIdx == -1 ? msg : msg
							.substring(0, redIdx);
					System.out.print(unHtml(black));
					if (redIdx != -1) {
						msg = msg.substring(Math.min(msg.length(), redIdx
								+ RED_MARKER.length()));
						blackIdx = msg.indexOf(BLACK_MARKER);
						String red = blackIdx == -1 ? msg : msg.substring(0,
								blackIdx);
						System.err.print(unHtml(red));
						if (blackIdx != -1)
							msg = msg.substring(Math.min(msg.length(), blackIdx
									+ BLACK_MARKER.length()));
					} else {
						blackIdx = -1;
					}
				} while ((redIdx >= 0) || (blackIdx >= 0));
			}
		}
	}

	private String unHtml(String red) {
		red = red.replaceAll("<[^<>]*>", "").replaceAll("&nbsp;", " ");
		red = red.replaceAll("&gt;", ">").replace("&lt;", "<");
		return red;
	}

	public Location getLocation() {
		return location;
	}

	public HTMLDocument getDocument() {
		return document;
	}

	public void scrollTo(int x, int y) {
	}

	public int getInnerWidth() {
		return webWrapper.getDrawPanel().getWidth();
	}

	public int getInnerHeight() {
		return webWrapper.getDrawPanel().getHeight();
	}

	public void setOnresize(Function onresize) {
		this.onresize = onresize;
	}

	public Function getOnresize() {
		return onresize;
	}

	public void clearTimeout(Object timeout) {
		if ((timeout instanceof TimeoutExecutionUnit))
			webWrapper.removeTask((DelayedExecutionUnit) timeout);
	}

	public TimeoutExecutionUnit setTimeout(Object obj, int milliseconds) {
		TimeoutExecutionUnit task = null;
		if ((obj instanceof Function)) {
			task = new TimeoutExecutionUnit((Function) obj, milliseconds);
			webWrapper.addTask(task);
		} else if ((obj instanceof String)) {
			task = new TimeoutExecutionUnit((String) obj, milliseconds);
			webWrapper.addTask(task);
		}
		return task;
	}

	public int getScrollX() {
		return 0;
	}

	public int getScrollY() {
		return 0;
	}

	public boolean setFullscreen(boolean enable) {
		return webWrapper.setFullscreen(enable);
	}

	public LocalStorage getLocalStorage() {
		if (localStorage == null) {
			Container container = webWrapper.getContainer();
			if ((container instanceof Applet))
				localStorage = new AppletLocalStorage((Applet) container);
			else
				localStorage = new ApplicationLocalStorage();
		}
		return localStorage;
	}

}