package com.aicontest.visualizer.js.dom;

import java.awt.Desktop;
import java.net.URI;

import netscape.javascript.JSObject;

import com.aicontest.visualizer.Visualizer;

public class Location {

	private Visualizer webWrapper;

	public Location(Visualizer webWrapper) {
		this.webWrapper = webWrapper;
	}

	private JSObject getJSLocation() {
		JSObject jsRoot = webWrapper.getJsRoot();
		if (jsRoot != null) {
			Thread.interrupted();
			JSObject document = (JSObject) jsRoot.getMember("document");
			return (JSObject) document.getMember("location");
		}
		return null;
	}

	public String getHref() {
		JSObject jsLoc = getJSLocation();
		if (jsLoc == null) {
			return "";
		}
		return jsLoc.getMember("href").toString();
	}

	public void setHref(String href) {
		JSObject jsLoc = getJSLocation();
		if (jsLoc == null)
			try {
				Desktop.getDesktop().browse(URI.create(href));
			} catch (Exception e) {
				e.printStackTrace();
			}
		else
			jsLoc.setMember("href", href);
	}
}