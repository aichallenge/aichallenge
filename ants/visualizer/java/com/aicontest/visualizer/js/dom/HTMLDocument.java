package com.aicontest.visualizer.js.dom;

import java.applet.Applet;
import java.awt.Container;
import java.awt.Frame;

import netscape.javascript.JSObject;

import org.w3c.dom.DOMException;
import org.w3c.dom.html.HTMLCollection;
import org.w3c.dom.html.HTMLElement;

import com.aicontest.visualizer.Visualizer;

public class HTMLDocument extends Document implements
		org.w3c.dom.html.HTMLDocument {
	public Object onkeydown;
	public Object onkeyup;
	public Object onkeypress;

	public HTMLDocument(Visualizer webWrapper) {
		super(webWrapper);
		this.webWrapper = webWrapper;
	}

	public String getTitle() {
		Container container = webWrapper.getContainer();
		if ((container instanceof Frame))
			return ((Frame) container).getTitle();
		if ((container instanceof Applet)) {
			((JSObject) JSObject.getWindow((Applet) container).getMember(
					"document")).getMember("title");
		}
		return "";
	}

	public void setTitle(String title) {
		Container container = webWrapper.getContainer();
		if ((container instanceof Frame))
			((Frame) container).setTitle(title);
		else if ((container instanceof Applet))
			((JSObject) JSObject.getWindow((Applet) container).getMember(
					"document")).setMember("title", title);
	}

	public String getReferrer() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getDomain() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getURL() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public HTMLElement getBody() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setBody(HTMLElement body) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public HTMLCollection getImages() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public HTMLCollection getApplets() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public HTMLCollection getLinks() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public HTMLCollection getForms() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public HTMLCollection getAnchors() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getCookie() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setCookie(String cookie) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void open() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void close() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void write(String text) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void writeln(String text) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Node.NodeList getElementsByName(String elementName) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	void contentChanged() {
		webWrapper.getDomWindow().contentChanged();
	}

	protected void childAdded(Node child) {
		super.childAdded(child);
		if ((child instanceof HTMLCanvasElement)){
			webWrapper.setCanvas((HTMLCanvasElement) child);
		}
	}
}