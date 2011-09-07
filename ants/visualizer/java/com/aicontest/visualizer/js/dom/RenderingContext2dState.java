package com.aicontest.visualizer.js.dom;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Paint;
import java.awt.geom.AffineTransform;
import java.awt.geom.Path2D;

class RenderingContext2dState {

	protected AffineTransform transform;
	protected Path2D.Double path;
	protected Path2D.Double clip;
	protected double x;
	protected double y;
	protected Paint paint;
	protected Object strokeStyle = Color.BLACK;
	protected Object fillStyle = Color.BLACK;
	protected float lineWidth = 1.0F;
	protected String font = "10px sans-serif";
	protected String textAlign = "start";
	protected String textBaseline = "alphabetic";
	protected double shadowOffsetX = 0.0D;
	protected double shadowOffsetY = 0.0D;
	protected double shadowBlur = 0.0D;
	protected Object shadowColor = new Color(0, 0, 0, 0);
	protected AlphaComposite globalCompositeOperation = AlphaComposite.SrcOver;
	
	public RenderingContext2dState() {
		transform = new AffineTransform();
	}

	protected RenderingContext2dState(RenderingContext2dState other) {
		transform = new AffineTransform();
		setFrom(other);
	}

	protected void setFrom(RenderingContext2dState state) {
		transform.setTransform(state.transform);
		x = state.x;
		y = state.y;
		if (state.path == null)
			path = null;
		else {
			path = ((Path2D.Double) state.path.clone());
		}
		if (state.clip == null)
			clip = null;
		else {
			clip = ((Path2D.Double) state.clip.clone());
		}
		paint = state.paint;
		strokeStyle = state.strokeStyle;
		fillStyle = state.fillStyle;
		lineWidth = state.lineWidth;
		font = state.font;
		textAlign = state.textAlign;
		textBaseline = state.textBaseline;
		shadowBlur = state.shadowBlur;
		shadowColor = state.shadowColor;
		shadowOffsetX = state.shadowOffsetX;
		shadowOffsetY = state.shadowOffsetY;
		globalCompositeOperation = state.globalCompositeOperation;
	}
}