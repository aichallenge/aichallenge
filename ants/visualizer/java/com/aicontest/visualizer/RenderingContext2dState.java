package com.aicontest.visualizer;

import java.awt.Color;
import java.awt.Paint;
import java.awt.geom.AffineTransform;
import java.awt.geom.Path2D;

public class RenderingContext2dState {

	protected AffineTransform transform;
	protected Path2D.Double path;
	protected Path2D.Double clip;
	protected double x;
	protected double y;
	protected Paint paint;
	
	public Object strokeStyle = Color.BLACK;
	public Object fillStyle = Color.BLACK;
	public float lineWidth = 1;
	public String font = "10px sans-serif";
	public String textAlign = "start"; // "start", "end", "left", "right",
										// "center"
	public String textBaseLine = "alphabetic"; // "top", "hanging", "middle",
												// "alphabetic", "ideographic",
												// "bottom"
	// shadows
	public double shadowOffsetX = 0;
	public double shadowOffsetY = 0;
	public double shadowBlur = 0;
	public Object shadowColor = new Color(0, 0, 0, 0);

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
		if (state.path == null) {
			path = null;
		} else {
			path = (Path2D.Double) state.path.clone();
		}
		if (state.clip == null) {
			clip = null;
		} else {
			clip = (Path2D.Double) state.clip.clone();
		}
		paint = state.paint;
		strokeStyle = state.strokeStyle;
		fillStyle = state.fillStyle;
		lineWidth = state.lineWidth;
		font = state.font;
		textAlign = state.textAlign;
		textBaseLine = state.textBaseLine;
		shadowBlur = state.shadowBlur;
		shadowColor = state.shadowColor;
		shadowOffsetX = state.shadowOffsetX;
		shadowOffsetY = state.shadowOffsetY;
	}

}
