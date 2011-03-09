package com.aicontest.visualizer;

import java.applet.Applet;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import netscape.javascript.JSObject;

@SuppressWarnings("serial")
public class CanvasApplet extends Applet implements MouseListener,
		MouseMotionListener, KeyListener {

	private HTMLCanvasElement mainCanvas;
	private Image offscreen;
	private Graphics offscreenGraphics;
	private JSObject inputHandler;
	private boolean debug;

	public ArrayList<HTMLImageElement> images;
	public HashMap<String, Integer> imageLookup;
	public ImageOps imageOps;

	public void init() {
		mainCanvas = createCanvas();
		handleResize();
		images = new ArrayList<HTMLImageElement>();
		imageLookup = new HashMap<String, Integer>();
		imageOps = new ImageOps();
		String token = this.getParameter("token");
		debug = "true".equals(this.getParameter("debug"));
		JSObject win = JSObject.getWindow(this);
		JSObject appletManager = ((JSObject) win.getMember("appletManager"));
		setFocusable(true);
		addKeyListener(this);
		addMouseListener(this);
		addMouseMotionListener(this);
		System.out.println("Applet got token: " + token);
		js(appletManager, "appletInitialized", new Object[] { token });
		System.out.println("Applet waits for commands");
	}

	private void handleResize() {
		offscreen = createImage(getWidth(), getHeight());
		offscreenGraphics = offscreen.getGraphics();
		mainCanvas.width = getWidth();
		mainCanvas.height = getHeight();
		mainCanvas.checkSize();
	}

	@Override
	public void update(Graphics g) {
		paint(g);
	}

	@Override
	public synchronized void paint(Graphics g) {
		g.drawImage(offscreen, 0, 0, this);
		getToolkit().sync();
	}

	@Override
	public synchronized void repaint() {
		offscreenGraphics.clearRect(0, 0, getWidth(), getHeight());
		offscreenGraphics.drawImage(mainCanvas.getPixmap(), 0, 0, this);
		super.repaint();
	}

	@Override
	public String getAppletInfo() {
		return "This applet is a wrapper around the html canvas element for browsers, which don't support that feature.";
	}

	public HTMLCanvasElement getMainCanvas() {
		return mainCanvas;
	}

	public HTMLCanvasElement createCanvas() {
		return new HTMLCanvasElement(this, 1, 1);
	}

	public HTMLImageElement imgRequest(String src) throws MalformedURLException {
		Integer id = imageLookup.get(src);
		if (id != null) {
			HTMLImageElement img = images.get(id);
			if (img != null) {
				return img;
			}
		} else {
			id = images.size();
			images.add(null);
			imageLookup.put(src, id);
		}
		Image img;
		HTMLImageElement image = null;
		try {
			URL url = new URL(getDocumentBase(), src);
			if (debug) {
				System.out
						.println("debug mode, fetching without cache: " + url);
				img = getToolkit().createImage(url);
			} else {
				img = getImage(getDocumentBase(), src);
			}
			System.out.println("image " + src + " requested");
			image = new HTMLImageElement(img, src);
		} catch (SecurityException e) {
			System.out.println("image " + src
					+ " is blocked by security restrictions:");
			e.printStackTrace();
		} catch (MalformedURLException e) {
			System.out.println("malformed URL: " + getDocumentBase());
			System.out.println("in connection with " + src + ":");
			e.printStackTrace();
		}
		images.set(id, image);
		return image;
	}

	public synchronized void setInputHandler(JSObject handler) {
		inputHandler = handler;
	}

	public void imgWaitFor(JSObject manager) {
		MediaTracker mediaTracker = new MediaTracker(this);
		for (int id = 0; id < images.size(); id++) {
			HTMLImageElement image = images.get(id);
			if (image != null) {
				mediaTracker.addImage(image.getImage(), id);
				try {
					mediaTracker.waitForID(id);
				} catch (InterruptedException e) {
					// this will render the image in an incomplete state
				}
				if ((mediaTracker.statusID(id, false) & MediaTracker.COMPLETE) != 0) {
					image.width = image.getImage().getWidth(this);
					image.height = image.getImage().getHeight(this);
					manager.call("imgHandler", new Object[] { images.get(id),
							true });
					continue;
				}
				images.set(id, null);
			}
			manager.call("imgHandler", new Object[] { images.get(id), false });
		}
	}

	@Override
	public synchronized void mouseDragged(MouseEvent e) {
	}

	@Override
	public synchronized void mouseMoved(MouseEvent e) {
		if (inputHandler != null) {
			js(inputHandler, "mouseMoved", new Object[] { e.getX(), e.getY() });
			e.consume();
		}
	}

	@Override
	public synchronized void mouseClicked(MouseEvent e) {
	}

	@Override
	public synchronized void mousePressed(MouseEvent e) {
		if (inputHandler != null && e.getButton() == MouseEvent.BUTTON1) {
			js(inputHandler, "mousePressed", null);
			e.consume();
		}
	}

	@Override
	public synchronized void mouseReleased(MouseEvent e) {
		if (inputHandler != null && e.getButton() == MouseEvent.BUTTON1) {
			js(inputHandler, "mouseReleased", null);
			e.consume();
		}
	}

	@Override
	public synchronized void mouseEntered(MouseEvent e) {
		if (inputHandler != null) {
			js(inputHandler, "mouseEntered", new Object[] { e.getX(), e.getY(),
					(e.getModifiersEx() & MouseEvent.BUTTON1_DOWN_MASK) != 0 });
		}
	}

	@Override
	public synchronized void mouseExited(MouseEvent e) {
		if (inputHandler != null) {
			js(inputHandler, "mouseExited", null);
		}
	}

	@Override
	public synchronized void keyTyped(KeyEvent e) {
	}

	@Override
	public synchronized void keyPressed(KeyEvent e) {
		System.out.println("key pressed");
		if (inputHandler != null) {
			js(inputHandler, "keyPressed", new Object[] { e.getKeyCode() });
			e.consume();
		}
	}

	@Override
	public synchronized void keyReleased(KeyEvent e) {
		if (inputHandler != null) {
			js(inputHandler, "keyReleased", new Object[] { e.getKeyCode() });
			e.consume();
		}
	}

	@Override
	public synchronized void setSize(int width, int height) {
		System.out.println("x: " + getWidth() + ", y: " + getHeight()
				+ " -> x: " + width + ", y: " + height);
		if (width != getWidth() || height != getHeight()) {
			super.setSize(width, height);
			handleResize();
		}
	}

	/**
	 * Opera has a lose understanding of JavaScript's single-threadedness. A
	 * call from an applet halts the currently executing code, executes the code
	 * called by the applet, then resumes with the previous operation. Since
	 * this results in race conditions I defer the applet's call until any
	 * active operation is completed. The arguments will be passed using
	 * func.apply(thisArg, argArray)
	 * 
	 * @param thisArg
	 *            the object the function schould be called on; will be 'this'
	 *            in the function
	 * @param func
	 *            the function to be called
	 * @param argArray
	 *            an array of arguments that will be expanded
	 */
	private void js(JSObject thisArg, String func, Object[] argArray) {
		final JSObject appletManager = ((JSObject) JSObject.getWindow(this)
				.getMember("appletManager"));
		Object[] args;
		if (argArray != null) {
			 args = new Object[argArray.length + 2];
			System.arraycopy(argArray, 0, args, 2, argArray.length);
		} else {
			args = new Object[2];
		}
		args[0] = thisArg;
		args[1] = func;
		appletManager.call("callAfterExecutionUnit", args);
	}

	public String getAttribute(String name) {
		if ("width".equals(name)) {
			return Integer.toString(getWidth());
		} else if ("height".equals(name)) {
			return Integer.toString(getHeight());
		}
		return null;
	}

	public void setAttribute(String name, int value) {
		if ("width".equals(name)) {
			setSize(value, getHeight());
		} else if ("height".equals(name)) {
			setSize(getWidth(), value);
		}
	}

}
