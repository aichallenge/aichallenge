package com.aicontest.visualizer;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.TexturePaint;
import java.awt.font.LineMetrics;
import java.awt.geom.Arc2D;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.Raster;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.StringTokenizer;

public class CanvasRenderingContext2d extends RenderingContext2dState {

	private HTMLCanvasElement canvas;
	private Graphics2D gfx;
	private Deque<RenderingContext2dState> stack;
	private static final Point2D ONE_PIXEL = new Point2D.Double(0, -1);

	public CanvasRenderingContext2d(HTMLCanvasElement canvas,
			BufferedImage pixmap) {
		this.canvas = canvas;
		updatePixmap(pixmap);
		stack = new ArrayDeque<RenderingContext2dState>();
	}

	// back-reference to the canvas
	public HTMLCanvasElement getCanvas() {
		return canvas;
	}

	// state
	public void restore() {
		setFrom(stack.removeLast());
		gfx.setTransform(transform);
		gfx.setClip(clip);
	}

	public void save() {
		stack.add(new RenderingContext2dState(this));
	}

	/*
	 * // transformations (default transform is the identity matrix) void
	 * scale(double x, double y) {
	 * 
	 * }
	 */
	public void rotate(double angle) {
		canvas.checkSize();
		gfx.rotate(angle);
		transform = gfx.getTransform();
	}

	public void translate(double x, double y) {
		canvas.checkSize();
		gfx.translate(x, y);
		transform = gfx.getTransform();
	}

	/*
	 * void transform(double a, double b, double c, double d, double e, double
	 * f) {
	 * 
	 * }
	 * 
	 * void setTransform(double a, double b, double c, double d, double e,
	 * double f) {
	 * 
	 * }
	 * 
	 * // compositing void setGlobalAlpha(double a) { // (default 1.0)
	 * 
	 * }
	 * 
	 * void setGlobalCompositeOperation(String op) { // (default source-over)
	 * 
	 * }
	 */
	// colors and styles
	private Object setStyle(Object style) throws Exception {
		if (style instanceof String) {
			String color = (String) style;
			if (color.startsWith("rgb(")) {
				color = color.substring(4, color.length() - 1);
				StringTokenizer st = new StringTokenizer(color, ",");
				int r = Integer.parseInt(st.nextToken().trim());
				int g = Integer.parseInt(st.nextToken().trim());
				int b = Integer.parseInt(st.nextToken().trim());
				style = new Color(r, g, b);
			} else if (color.startsWith("rgba(")) {
				color = color.substring(5, color.length() - 1);
				StringTokenizer st = new StringTokenizer(color, ",");
				float r = Integer.parseInt(st.nextToken().trim()) / 255.0f;
				float g = Integer.parseInt(st.nextToken().trim()) / 255.0f;
				float b = Integer.parseInt(st.nextToken().trim()) / 255.0f;
				float a = Float.parseFloat(st.nextToken().trim());
				style = new Color(r, g, b, a);
			} else if (color.length() == 7) {
				int r = Integer.parseInt(color.substring(1, 3), 16);
				int g = Integer.parseInt(color.substring(3, 5), 16);
				int b = Integer.parseInt(color.substring(5, 7), 16);
				style = new Color(r, g, b);
			} else if (color.length() == 4) {
				int r = 17 * Character.digit(color.charAt(1), 16);
				int g = 17 * Character.digit(color.charAt(2), 16);
				int b = 17 * Character.digit(color.charAt(3), 16);
				style = new Color(r, g, b);
			} else {
				throw new Exception("cannot parse paint style: " + style);
			}
		}
		return style;
	}

	private void setStrokeStyle() throws Exception {
		strokeStyle = setStyle(strokeStyle);
		if (strokeStyle instanceof Color) {
			gfx.setColor((Color) strokeStyle);
			gfx.setStroke(new BasicStroke(lineWidth));
		} else {
			gfx.setStroke((Stroke) strokeStyle);
		}
	}

	private void setFillStyle() throws Exception {
		fillStyle = setStyle(fillStyle);
		if (fillStyle instanceof CanvasPattern) {
			BufferedImage cp = ((CanvasPattern) fillStyle).getPattern();
			// Java2D seems to shift rotated patterns by 1 pixel
			Point2D p = transform.transform(ONE_PIXEL, null);
			Rectangle2D.Double anchor = new Rectangle2D.Double(0.0, p.getX(),
					cp.getWidth(), cp.getHeight());
			gfx.setPaint(new TexturePaint(cp, anchor));
		} else {
			gfx.setPaint((Paint) fillStyle);
		}
	}

	/*
	 * CanvasGradient createLinearGradient(double x0, double y0, double x1,
	 * double y1) { return null; }
	 * 
	 * CanvasGradient createRadialGradient(double x0, double y0, double r0,
	 * double x1, double y1, double r1) { return null; }
	 */
	public CanvasPattern createPattern(HTMLImageElement image, String repetition) {
		return new CanvasPattern(image.getImage(), repetition, canvas.applet);
	}

	public CanvasPattern createPattern(HTMLCanvasElement cvs, String repetition) {
		return new CanvasPattern(cvs.getPixmap(), repetition, canvas.applet);
	}

	/*
	 * CanvasPattern createPattern(HTMLVideoElement image, String repetition) {
	 * return null; }
	 * 
	 * void setLineCap(String lineCap) { // "butt", "round", "square" (default
	 * "butt")
	 * 
	 * }
	 * 
	 * void setLineJoin(String lineJoin) { // "round", "bevel", "miter" (default
	 * "miter")
	 * 
	 * }
	 * 
	 * void setMiterLimit(double miterLimit) { // (default 10)
	 * 
	 * }
	 */

	// rects
	public void clearRect(double x, double y, double w, double h) {
		canvas.checkSize();
		gfx.clearRect((int) x, (int) y, (int) w, (int) h);
	}

	public void fillRect(double x, double y, double w, double h) throws Exception {
		canvas.checkSize();
		setFillStyle();
		Path2D.Double oldPath = path;
		beginPath();
		rect(x, y, w, h);
		gfx.fill(path);
		path = oldPath;
	}

	/*
	 * void strokeRect(double x, double y, double w, double h) {
	 * 
	 * }
	 */
	// path API
	public void beginPath() {
		path = new Path2D.Double();
	}

	public void closePath() {
		if (path != null) {
			path.closePath();
		}
	}

	public void moveTo(double x, double y) {
		if (path != null) {
			path.moveTo(x, y);
		}
		this.x = x;
		this.y = y;
	}

	public void lineTo(double x, double y) throws Exception {
		if (path == null) {
			canvas.checkSize();
			setStrokeStyle();
			gfx.drawLine((int) this.x, (int) this.y, (int) x, (int) y);
		} else {
			path.lineTo(x, y);
		}
		this.x = x;
		this.y = y;
	}

	/*
	 * void quadraticCurveTo(double cpx, double cpy, double x, double y) {
	 * 
	 * }
	 * 
	 * void bezierCurveTo(double cp1x, double cp1y, double cp2x, double cp2y,
	 * double x, double y) {
	 * 
	 * }
	 * 
	 * void arcTo(double x1, double y1, double x2, double y2, double radius) {
	 * 
	 * }
	 */
	public void rect(double x, double y, double w, double h) {
		if (path != null) {
			path.moveTo(x, y);
			path.lineTo(x, y + h);
			path.lineTo(x + w, y + h);
			path.lineTo(x + w, y);
			path.lineTo(x, y);
		}
	}

	public void arc(double x, double y, double radius, double startAngle,
			double endAngle) {
		arc(x, y, radius, startAngle, endAngle, false);
	}

	public void arc(double x, double y, double radius, double startAngle,
			double endAngle, boolean antiClockwise) {
		if (path != null) {
			if (antiClockwise) {
			} else {
				path.append(new Arc2D.Double(x - radius, y - radius,
						2 * radius, 2 * radius, -startAngle * 180 / Math.PI,
						(startAngle - endAngle) * 180 / Math.PI, Arc2D.OPEN),
						true);
			}
		}
	}

	public void fill() throws Exception {
		canvas.checkSize();
		setFillStyle();
		gfx.fill(path);
	}

	public void stroke() throws Exception {
		canvas.checkSize();
		setStrokeStyle();
		gfx.draw(path);
	}

	public void clip() {
		gfx.clip(path);
		if (path != null) {
			clip = (Path2D.Double) path.clone();
		}
	}

	/*
	 * boolean isPointInPath(double x, double y) { return false; }
	 * 
	 * // focus management boolean drawFocusRing(Element element, double xCaret,
	 * double yCaret) { return drawFocusRing(element, xCaret, yCaret, false); }
	 * 
	 * boolean drawFocusRing(Element element, double xCaret, double yCaret,
	 * boolean canDrawCustom) { return false; }
	 */
	
	private void setFont() {
		String name = "sans-serif";
		int style = Font.PLAIN;
		int pt = 7;
		StringTokenizer st = new StringTokenizer(font);
		int ct = st.countTokens();
		for (int i = 0; i < ct; i++) {
			String token = st.nextToken();
			if (i == ct - 1) {
				name = token;
			} else if (i == ct - 2) {
				if (token.endsWith("px")) {
					pt = Integer
							.parseInt(token.substring(0, token.length() - 2));
				}
			} else if (token.equals("bold")) {
				style |= Font.BOLD;
			} else if (token.equals("italic")) {
				style |= Font.ITALIC;
			}
		}
		gfx.setFont(new Font(name, style, pt));
	}

	// text
	public void fillText(String text, float x, float y) throws Exception {
		canvas.checkSize();
		setFillStyle();
		setFont();
		LineMetrics metrics = gfx.getFontMetrics().getLineMetrics(text, gfx);
		if (textBaseLine.equals("top")) {
			y += metrics.getAscent();
		} else if (textBaseLine.equals("middle")) {
			y += metrics.getAscent() - metrics.getHeight() / 2;
		} else if (textBaseLine.equals("bottom")) {
			y -= metrics.getDescent();
		}
		int w = gfx.getFontMetrics().stringWidth(text);
		if (textAlign.equals("end") || textAlign.equals("right")) {
			x -= w;
		} else if (textAlign.equals("center")) {
			x -= 0.5 * w;
		}
		gfx.drawString(text, x, y);
	}

	/*
	 * void fillText(String text, double x, double y, double maxWidth) {
	 * 
	 * }
	 * 
	 * void strokeText(String text, double x, double y) { fillText(text, x, y,
	 * Double.MAX_VALUE); }
	 * 
	 * void strokeText(String text, double x, double y, double maxWidth) {
	 * 
	 * }
	 */
	public TextMetrics measureText(String text) {
		setFont();
		return new TextMetrics(gfx.getFontMetrics().stringWidth(text));
	}

	/*
	 * // drawing images void drawImage(HTMLImageElement image, double dx,
	 * double dy) {
	 * 
	 * }
	 * 
	 * void drawImage(HTMLImageElement image, double dx, double dy, double dw,
	 * double dh) {
	 * 
	 * }
	 */
	public void drawImage(HTMLImageElement image, int sx, int sy, int sw,
			int sh, int dx, int dy, int dw, int dh) {
		canvas.checkSize();
		gfx.drawImage(image.getImage(), dx, dy, dx + dw, dy + dh, sx, sy, sx
				+ sw, sy + sh, canvas.applet);
	}

	public void drawImage(HTMLCanvasElement image, int dx, int dy) {
		canvas.checkSize();
		gfx.drawImage(image.getPixmap(), dx, dy, canvas.applet);
	}

	public void drawImage(HTMLCanvasElement image, int sx, int sy, int sw,
			int sh, int dx, int dy, int dw, int dh) {
		canvas.checkSize();
		gfx.drawImage(image.getPixmap(), dx, dy, dx + dw, dy + dh, sx, sy, sx
				+ sw, sy + sh, canvas.applet);
	}

	public void updatePixmap(BufferedImage pixmap) {
		gfx = pixmap.createGraphics();
		gfx.setBackground(new Color(0, 0, 0, 0));
		gfx.clearRect(0, 0, pixmap.getWidth(), pixmap.getHeight());
		// allows for anti-aliased text, if enabled
		gfx.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
		// this makes 2 pixel wide line appear how I want them to
		gfx.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL,
				RenderingHints.VALUE_STROKE_PURE);
		gfx.setTransform(transform);
		gfx.setClip(clip);
	}

	public ImageData getImageData(int sx, int sy, int sw, int sh) {
		canvas.checkSize();
		Raster data = canvas.getPixmap().getData(new Rectangle(sx, sy, sw, sh));
		return new ImageData(data);
	}

	public void putImageData(ImageData imagedata, int dx, int dy) {
		canvas.checkSize();
		int[] data = new int[imagedata.data.length / 4];
		int p = 0;
		for (int i = 0; i < imagedata.data.length; i += 4, p++) {
			data[p] = imagedata.data[i] << 16;
			data[p] |= imagedata.data[i + 1] << 8;
			data[p] |= imagedata.data[i + 2] << 0;
			data[p] |= imagedata.data[i + 3] << 24;
		}
		canvas.getPixmap().setRGB(dx, dy, imagedata.width, imagedata.height,
				data, 0, imagedata.width);
	}

}
