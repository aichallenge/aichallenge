package com.aicontest.visualizer.js.dom;

import java.awt.Component;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.net.URL;

import org.w3c.dom.DOMException;

import com.aicontest.visualizer.WebWrapper;
import com.aicontest.visualizer.js.tasks.EventExecutionUnit;

public class HTMLImageElement extends HTMLElement {
	private Image image;
	private String src;
	public Object onload;
	public Object onerror;
	public Object onabort;

	public HTMLImageElement() {
		src = "";
	}

	public Image getImage() {
		return image;
	}

	public void setImage(Image image) {
		this.image = image;
	}

	public String getLowSrc() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setLowSrc(String lowSrc) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getName() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setName(String name) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getAlign() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setAlign(String align) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getAlt() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setAlt(String alt) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getBorder() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setBorder(String border) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public int getHeight() {
		return image.getHeight(null);
	}

	public void setHeight(int height) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getHspace() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setHspace(String hspace) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public boolean getIsMap() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setIsMap(boolean isMap) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getLongDesc() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setLongDesc(String longDesc) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getSrc() {
		return src;
	}

	public void setSrc(String src) {
		this.src = src;
		try {
			URL imgURL = getClass().getResource(src);
			Toolkit tk = Toolkit.getDefaultToolkit();
			image = tk.getImage(imgURL);
			new ImageThread(this, WebWrapper.getInstance());
		} catch (Exception e) {
			WebWrapper.getInstance().addTask(
					new EventExecutionUnit(this, "onerror", new Object[0]));
		}
	}

	public String getUseMap() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setUseMap(String useMap) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getVspace() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setVspace(String vspace) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public int getWidth() {
		return image.getWidth(null);
	}

	public void setWidth(int width) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	private class ImageThread extends Thread {
		private final HTMLImageElement hie;
		private final WebWrapper webWrapper;

		public ImageThread(HTMLImageElement hie, WebWrapper webWrapper) {
			this.hie = hie;
			this.webWrapper = webWrapper;
			start();
		}

		public void run() {
			try {
				@SuppressWarnings("serial")
				MediaTracker m = new MediaTracker(new Component() {
				});
				m.addImage(image, 0);
				try {
					m.waitForAll();
					webWrapper.addTask(new EventExecutionUnit(hie, "onload",
							new Object[0]));
				} catch (InterruptedException e) {
					webWrapper.addTask(new EventExecutionUnit(hie, "onabort",
							new Object[0]));
				}
			} catch (Exception e) {
				webWrapper.addTask(new EventExecutionUnit(hie, "onerror",
						new Object[0]));
			}
		}
	}

}