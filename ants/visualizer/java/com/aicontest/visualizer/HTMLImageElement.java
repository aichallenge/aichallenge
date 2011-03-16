package com.aicontest.visualizer;

import java.awt.Image;

public class HTMLImageElement {
	
	private Image image;
	
	public int width;
	public int height;
	public String src;
	
	public HTMLImageElement(Image img, String src) {
		this.image = img;
		this.src = src;
	}
	
	public Image getImage() {
		return image;
	}
	
	public void setImage(Image image) {
		this.image = image;
	}

}
