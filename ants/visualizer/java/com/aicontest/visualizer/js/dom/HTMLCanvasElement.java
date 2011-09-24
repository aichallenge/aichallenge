package com.aicontest.visualizer.js.dom;

import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import org.mozilla.javascript.Function;
import org.w3c.dom.DOMException;

import com.aicontest.visualizer.Visualizer;
import com.aicontest.visualizer.WebWrapper;

public class HTMLCanvasElement extends HTMLElement {
	private BufferedImage pixmap;
	private CanvasRenderingContext2d context2d;
	private final ImageObserver imageObserver;
	private Function onmousemove;
	private Function onmouseout;
	private Function onmousedown;
	private Function onmouseup;
	private Function ondblclick;

	public HTMLCanvasElement(ImageObserver imageObsever) {
		imageObserver = imageObsever;
		pixmap = new BufferedImage(1, 1, 6);
	}

	public BufferedImage getPixmap() {
		return pixmap;
	}

	public CanvasRenderingContext2d getContext(String type) {
		if ("2d".equals(type)) {
			if (context2d == null) {
				context2d = new CanvasRenderingContext2d(this, pixmap);
			}
			return context2d;
		}
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, type);
	}

	ImageObserver getImageObserver() {
		return imageObserver;
	}

	public void setWidth(int width) {
		if (width != pixmap.getWidth()) {
			pixmap = new BufferedImage(width, pixmap.getHeight(),
					pixmap.getType());
			context2d.updatePixmap(pixmap);
			Visualizer visualizer = (Visualizer) WebWrapper.getInstance();
			visualizer.canvasResized();
		}
	}

	public int getWidth() {
		return pixmap.getWidth();
	}

	public void setHeight(int height) {
		if (height != pixmap.getHeight()) {
			pixmap = new BufferedImage(pixmap.getWidth(), height,
					pixmap.getType());
			context2d.updatePixmap(pixmap);
			Visualizer visualizer = (Visualizer) WebWrapper.getInstance();
			visualizer.canvasResized();
		}
	}

	public int getHeight() {
		return pixmap.getHeight();
	}

	public void setOnmousemove(Function onmousemove) {
		this.onmousemove = onmousemove;
	}

	public Function getOnmousemove() {
		return onmousemove;
	}

	public Function getOnmouseout() {
		return onmouseout;
	}

	public void setOnmouseout(Function onmouseout) {
		this.onmouseout = onmouseout;
	}

	public Function getOnmousedown() {
		return onmousedown;
	}

	public void setOnmousedown(Function onmousedown) {
		this.onmousedown = onmousedown;
	}

	public Function getOnmouseup() {
		return onmouseup;
	}

	public void setOnmouseup(Function onmouseup) {
		this.onmouseup = onmouseup;
	}

	public Function getOndblclick() {
		return ondblclick;
	}

	public void setOndblclick(Function ondblclick) {
		this.ondblclick = ondblclick;
	}
}