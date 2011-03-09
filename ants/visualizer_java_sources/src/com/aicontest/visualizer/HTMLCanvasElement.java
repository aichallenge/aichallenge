package com.aicontest.visualizer;

import java.awt.image.BufferedImage;

public class HTMLCanvasElement {
	
	private BufferedImage pixmap;
	private CanvasRenderingContext2d context;
	
	public int width;
	public int height;
	public CanvasApplet applet;
	
	public HTMLCanvasElement(CanvasApplet canvasApplet) {
		this(canvasApplet, canvasApplet.getWidth(), canvasApplet.getHeight());
	}

	public HTMLCanvasElement(CanvasApplet canvasApplet, int w, int h) {
		applet = canvasApplet;
		width = w;
		height = h;
		pixmap = new BufferedImage(w, h, BufferedImage.TYPE_4BYTE_ABGR);
		context = new CanvasRenderingContext2d(this, pixmap);
	}
	
	BufferedImage getPixmap() {
		return pixmap;
	}
	
	public CanvasRenderingContext2d getContext(String type) {
		return context;
	}

	public void checkSize() {
		if (width != pixmap.getWidth() || height != pixmap.getHeight()) {
			pixmap = new BufferedImage(width, height, pixmap.getType());
			context.updatePixmap(pixmap);
		}
	}

}
