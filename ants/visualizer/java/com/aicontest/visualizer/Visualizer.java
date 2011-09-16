package com.aicontest.visualizer;

import java.applet.Applet;
import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Iterator;

import javax.swing.event.MouseInputListener;

import netscape.javascript.JSObject;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.ScriptRuntime;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

import com.aicontest.visualizer.js.dom.DOMWindow;
import com.aicontest.visualizer.js.dom.HTMLCanvasElement;
import com.aicontest.visualizer.js.tasks.EventExecutionUnit;
import com.aicontest.visualizer.js.tasks.IExecutionUnit;

public class Visualizer extends WebWrapper implements MouseInputListener,
		KeyListener, ComponentListener {
	private Image offscreen;
	private Graphics offscreenGraphics;
	private final DOMWindow domWindow;
	private final VisualizerPanel drawPanel;
	private final IVisualizerUser app;
	private JSObject jsRoot;

	public Visualizer(IVisualizerUser app, int width, int height)
			throws InstantiationException, IllegalAccessException, IOException {
		super(app.getJavaScriptPath());
		this.app = app;
		domWindow = new DOMWindow("Visualizer", this);
		Object window = Context.javaToJS(domWindow, global);
		ScriptableObject.putProperty(global, "window", window);
		Object document = Context.javaToJS(domWindow.getDocument(), global);
		ScriptableObject.putProperty(global, "document", document);
		loadProgram(app.getProgram());
		for (WebWrapper.Script script : scripts) {
			script.run();
		}
		drawPanel = new VisualizerPanel(this, width, height);
		drawPanel.addComponentListener(this);
		drawPanel.addMouseMotionListener(this);
		drawPanel.addMouseListener(this);
		drawPanel.addKeyListener(this);
		app.setVisualizerPanel(drawPanel);
	}

	public synchronized void paint(Graphics g) {
		g.drawImage(offscreen, 0, 0, null);
		Toolkit.getDefaultToolkit().sync();
	}

	@Override
	protected synchronized void postExecute() {
		if ((canvas != null) && (canvas.getContext("2d").isDrawn())) {
			BufferedImage pixmap = canvas.getPixmap();
			offscreenGraphics.clearRect(0, 0, offscreen.getWidth(drawPanel),
					offscreen.getHeight(drawPanel));
			offscreenGraphics.drawImage(pixmap, 0, 0, null);
			drawPanel.repaint();
		}
	}

	public void setCanvas(HTMLCanvasElement canvas) {
		this.canvas = canvas;
		canvasResized();
	}

	public synchronized void canvasResized() {
		if (canvas != null && canvas.getWidth() > 0 && canvas.getHeight() > 0 && (offscreen == null || offscreen.getWidth(drawPanel) != canvas.getWidth() ||offscreen.getHeight(drawPanel) != canvas.getHeight())) {
			offscreen = drawPanel.createImage(canvas.getWidth(),
					canvas.getHeight());
			if (offscreen != null) {
				offscreenGraphics = offscreen.getGraphics();
			}
		}
	}

	public VisualizerPanel getDrawPanel() {
		return drawPanel;
	}

	public DOMWindow getDomWindow() {
		return domWindow;
	}

	public Container getContainer() {
		Component result = drawPanel;
		while ((!(result instanceof Frame)) && (!(result instanceof Applet))) {
			result = result.getParent();
		}
		return (Container) result;
	}

	protected EventExecutionUnit createEventObject(InputEvent e, String type) {
		EventExecutionUnit task = null;
		if (canvas != null) {
			Scriptable event = cx.newObject(global);
			event.put("type", event, type);
			event.put("altKey", event, Boolean.valueOf(e.isAltDown()));
			event.put("ctrlKey", event, Boolean.valueOf(e.isControlDown()));
			event.put("shiftKey", event, Boolean.valueOf(e.isShiftDown()));
			event.put("button", event,
					Integer.valueOf(e.getModifiersEx() >> 10 & 0x7));
			if ((e instanceof MouseEvent)) {
				MouseEvent me = (MouseEvent) e;
				event.put("clientX", event, Integer.valueOf(me.getX()));
				event.put("clientY", event, Integer.valueOf(me.getY()));
				event.put("layerX", event, Integer.valueOf(me.getX()));
				event.put("layerY", event, Integer.valueOf(me.getY()));
				event.put("offsetX", event, Integer.valueOf(me.getX()));
				event.put("offsetY", event, Integer.valueOf(me.getY()));
				event.put("pageX", event, Integer.valueOf(me.getX()));
				event.put("pageY", event, Integer.valueOf(me.getY()));
				event.put("screenX", event, Integer.valueOf(me.getXOnScreen()));
				event.put("screenY", event, Integer.valueOf(me.getYOnScreen()));
				event.put("x", event, Integer.valueOf(me.getX()));
				event.put("y", event, Integer.valueOf(me.getY()));
				switch (me.getButton()) {
				case 2:
					event.put("which", event, Integer.valueOf(3));
					break;
				case 3:
					event.put("which", event, Integer.valueOf(2));
					break;
				default:
					event.put("which", event, Integer.valueOf(me.getButton()));
				}
				task = new EventExecutionUnit(canvas, "on" + type,
						new Object[] { event });
			} else if ((e instanceof KeyEvent)) {
				KeyEvent ke = (KeyEvent) e;
				System.out.println(Integer.valueOf(ke.getKeyCode()));
				event.put("keyCode", event, Integer.valueOf(ke.getKeyCode()));
				event.put("which", event, Integer.valueOf(ke.getKeyChar()));
				task = new EventExecutionUnit(domWindow.getDocument(), "on"
						+ type, new Object[] { event });
			}
			addTask(task);
			e.consume();
		}
		return task;
	}

	public boolean setFullscreen(boolean enable) {
		return app.setFullScreen(enable);
	}

	@Override
	public void keyPressed(KeyEvent e) {
		createEventObject(e, "keydown");
	}

	@Override
	public void keyReleased(KeyEvent e) {
		createEventObject(e, "keyup");
	}

	@Override
	public void keyTyped(KeyEvent e) {
	}

	@Override
	public void mouseClicked(MouseEvent e) {
	}

	@Override
	public void mouseEntered(MouseEvent e) {
	}

	@Override
	public void mouseExited(MouseEvent e) {
		createEventObject(e, "mouseout");
	}

	@Override
	public void mousePressed(MouseEvent e) {
		createEventObject(e, "mousedown");
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		createEventObject(e, "mouseup");
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		mouseMoved(e);
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		EventExecutionUnit task = createEventObject(e, "mousemove");
		if (task != null) {
			Iterator<IExecutionUnit> it = immediateQueue.iterator();
			while (it.hasNext()) {
				IExecutionUnit eu = (IExecutionUnit) it.next();
				if ((eu instanceof EventExecutionUnit)) {
					EventExecutionUnit eeu = (EventExecutionUnit) eu;
					if ((eeu != task) && (eeu.matches(canvas, "onmousemove")))
						it.remove();
				}
			}
		}
	}

	@Override
	public void componentHidden(ComponentEvent e) {
	}

	@Override
	public void componentMoved(ComponentEvent e) {
	}

	@Override
	public void componentResized(ComponentEvent e) {
		addTask(new EventExecutionUnit(domWindow, "onresize",
				ScriptRuntime.emptyArgs));
	}

	@Override
	public void componentShown(ComponentEvent e) {
	}

	public void setJsRoot(JSObject jsRoot) {
		this.jsRoot = jsRoot;
	}

	public JSObject getJsRoot() {
		return jsRoot;
	}

}