package com.aicontest.visualizer.js;

import java.applet.Applet;
import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.security.AccessControlException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.DelayQueue;

import org.mozilla.javascript.CompilerEnvirons;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.ErrorReporter;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.GeneratedClassLoader;
import org.mozilla.javascript.IRFactory;
import org.mozilla.javascript.NativeJavaClass;
import org.mozilla.javascript.Parser;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;
import org.mozilla.javascript.SecurityController;
import org.mozilla.javascript.ast.AstRoot;
import org.mozilla.javascript.ast.ScriptNode;
import org.mozilla.javascript.optimizer.Codegen;

import com.aicontest.visualizer.js.dom.DOMWindow;
import com.aicontest.visualizer.js.dom.HTMLCanvasElement;
import com.aicontest.visualizer.js.dom.HTMLImageElement;
import com.aicontest.visualizer.js.dom.XMLHttpRequest;
import com.aicontest.visualizer.js.tasks.DelayedExecutionUnit;
import com.aicontest.visualizer.js.tasks.EventExecutionUnit;
import com.aicontest.visualizer.js.tasks.IExecutionUnit;

public class WebWrapper {
	private File baseDir;
	private final DelayQueue<DelayedExecutionUnit> delayedQueue = new DelayQueue<DelayedExecutionUnit>();
	private final ConcurrentLinkedQueue<IExecutionUnit> immediateQueue = new ConcurrentLinkedQueue<IExecutionUnit>();
	private Map<String, String> precompiled = new HashMap<String, String>();
	private Map<String, String> inUse = new HashMap<String, String>();
	private Context cx;
	private ScriptableObject global;
	private Thread jsThread = Thread.currentThread();
	private DOMWindow domWindow;
	private HTMLCanvasElement canvas;
	private Codegen compiler;
	private CompilerEnvirons compilerEnv;
	private ErrorReporter compilationErrorReporter;
	private Component drawPanel;
	private GeneratedClassLoader loader;

	public WebWrapper(String baseDir) {
		cx = Context.enter();
		cx.setOptimizationLevel(-1);
		cx.setLanguageVersion(150);
		this.baseDir = (baseDir == null) ? null : new File(baseDir);
		try {
			InputStream is = getClass().getResourceAsStream("/precompiled");
			if (is == null) {
				is = new FileInputStream("precompiled");
			}
			BufferedReader br = new BufferedReader(new InputStreamReader(is));
			try {
				String js;
				while ((js = br.readLine()) != null) {
					String clazz = br.readLine();
					precompiled.put(js, clazz);
				}
			} finally {
				br.close();
			}
		} catch (Exception e) {
		}
	}

	public void setDrawPanel(Component drawPanel) {
		this.drawPanel = drawPanel;
		global = cx.initStandardObjects();
		cx.putThreadLocal(WebWrapper.class, this);
		cx.evaluateString(global, "alert = function(x) { java.lang.System.out.println(x) }", "<web-wrapper>", 1, null);
		domWindow = new DOMWindow("Visualizer", this);
		Object window = Context.javaToJS(domWindow, global);
		ScriptableObject.putProperty(global, "window", window);
		Object document = Context.javaToJS(domWindow.getDocument(), global);
		ScriptableObject.putProperty(global, "document", document);
		Object image = new NativeJavaClass(global, HTMLImageElement.class);
		global.put("Image", global, image);
		Object xmlHttpRequest = new NativeJavaClass(global, XMLHttpRequest.class);
		global.put("XMLHttpRequest", global, xmlHttpRequest);
		drawPanel.addComponentListener(new ComponentAdapter() {
			public void componentResized(ComponentEvent e) {
				addTask(new EventExecutionUnit(domWindow, "onresize", new Object[0]));
			}
		});
		MouseAdapter ma = new MouseAdapter() {
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

			public void mouseDragged(MouseEvent e) {
				mouseMoved(e);
			}

			public void mousePressed(MouseEvent e) {
				createEventObject(e, "mousedown");
			}

			public void mouseReleased(MouseEvent e) {
				createEventObject(e, "mouseup");
			}

			public void mouseExited(MouseEvent e) {
				createEventObject(e, "mouseout");
			}
		};
		drawPanel.addMouseMotionListener(ma);
		drawPanel.addMouseListener(ma);
		KeyAdapter ka = new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				createEventObject(e, "keydown");
			}

			public void keyReleased(KeyEvent e) {
				createEventObject(e, "keyup");
			}
		};
		drawPanel.addKeyListener(ka);
	}

	protected EventExecutionUnit createEventObject(InputEvent e, String type) {
		EventExecutionUnit task = null;
		if (canvas != null) {
			Scriptable event = cx.newObject(global);
			event.put("type", event, type);
			event.put("altKey", event, Boolean.valueOf(e.isAltDown()));
			event.put("ctrlKey", event, Boolean.valueOf(e.isControlDown()));
			event.put("shiftKey", event, Boolean.valueOf(e.isShiftDown()));
			event.put("button", event, Integer.valueOf(e.getModifiersEx() >> 10 & 0x7));
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
				task = new EventExecutionUnit(canvas, "on" + type, new Object[] { event });
			} else if ((e instanceof KeyEvent)) {
				KeyEvent ke = (KeyEvent) e;
				event.put("keyCode", event, Integer.valueOf(ke.getKeyCode()));
				event.put("which", event, Integer.valueOf(ke.getKeyChar()));
				task = new EventExecutionUnit(domWindow.getDocument(), "on" + type, new Object[] { event });
			}
			addTask(task);
			e.consume();
		}
		return task;
	}

	private Class<?> recompile(String file) throws FileNotFoundException, IOException {
		if (compiler == null) {
			compiler = new Codegen();
		}
		FileReader in = new FileReader(new File(baseDir, file));
		try {
			if (compilerEnv == null) {
				compilerEnv = new CompilerEnvirons();
				compilerEnv.initFromContext(cx);
				compilerEnv.setOptimizationLevel(0);
				compilerEnv.setGeneratingSource(false);
			}
			if (compilationErrorReporter == null) {
				compilationErrorReporter = compilerEnv.getErrorReporter();
			}
			Parser p = new Parser(compilerEnv, compilationErrorReporter);
			AstRoot ast = p.parse(in, file.toString(), 1);
			IRFactory irf = new IRFactory(compilerEnv, compilationErrorReporter);
			ScriptNode tree = irf.transformTree(ast);
			Object[] nameBytesPair = (Object[]) (Object[]) compiler.compile(compilerEnv, tree, tree.getEncodedSource(), false);
			String className = (String) nameBytesPair[0];
			byte[] classBytes = (byte[]) (byte[]) nameBytesPair[1];
			File outFile = new File(className.replace('.', File.separatorChar) + ".class");
			String oldClassName = (String) precompiled.get(file);
			if (oldClassName != null) {
				new File(oldClassName.replace('.', File.separatorChar) + ".class").delete();
			}
			System.out.println("Compiling " + file + " -> " + outFile);
			outFile.getParentFile().mkdirs();
			FileOutputStream fos = new FileOutputStream(outFile);
			try {
				fos.write(classBytes);
			} finally {
				fos.close();
			}
			if (loader == null) {
				loader = SecurityController.createLoader(getClass().getClassLoader(), null);
			}
			Class<?> clazz = loader.defineClass(className, classBytes);
			loader.linkClass(clazz);
			return clazz;
		} finally {
			in.close();
		}
	}

	public Script loadJs(String file) throws IOException, InstantiationException, IllegalAccessException, ClassNotFoundException {
		Class<?> clazz = null;
		String objClassName = (String) precompiled.get(file);
		if (objClassName == null) {
			clazz = recompile(file);
			objClassName = clazz.getCanonicalName();
		} else {
			File objFile = new File(objClassName.replace('.', File.separatorChar) + ".class");
			boolean useExisting = baseDir == null;
			if (!useExisting) {
				File jsFile = new File(baseDir, file);
				try {
					useExisting = !jsFile.canRead() || (jsFile.lastModified() <= objFile.lastModified());
				} catch (AccessControlException e) {
				}
			}
			if (useExisting) {
				try {
					clazz = getClass().getClassLoader().loadClass(objClassName);
				} catch (ClassFormatError e) {
					System.err.println("bundled class " + objClassName + " has invalid format");
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
			if (clazz == null) {
				clazz = recompile(file);
				objClassName = clazz.getCanonicalName();
			}
		}
		inUse.put(file, objClassName);
		return new Script((org.mozilla.javascript.Script) clazz.newInstance());
	}

	public ScriptableObject construct(String functor, Object[] args) {
		return (ScriptableObject) cx.newObject(global, functor, args);
	}

	public void loop() throws Exception {
		while (true) {
			try {
				while ((!Thread.interrupted()) && (immediateQueue.size() == 0)) {
					IExecutionUnit task = (IExecutionUnit) delayedQueue.take();
					execute(task);
				}
			} catch (InterruptedException e) {
				Thread.interrupted();
			}
			IExecutionUnit task = (IExecutionUnit) immediateQueue.poll();
			while (task != null) {
				execute(task);
				task = (IExecutionUnit) immediateQueue.poll();
			}
		}
	}

	public Object eval(String script) {
		return cx.evaluateString(global, script, "<anonymous>", 1, null);
	}

	public void invoke(Scriptable thiz, String functionName, Object[] args) {
		Function f = (Function) ScriptableObject.getProperty(thiz, functionName);
		f.call(cx, global, thiz, args);
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

	public static WebWrapper getInstance() {
		Context cx = Context.enter();
		try {
			WebWrapper localWebWrapper = (WebWrapper) cx.getThreadLocal(WebWrapper.class);
			return localWebWrapper;
		} finally {
			Context.exit();
		}
	}

	private void execute(IExecutionUnit task) throws Exception {
		task.execute(cx, global);
		drawPanel.repaint();
	}

	public void addTask(IExecutionUnit task) {
		immediateQueue.add(task);
		jsThread.interrupt();
	}

	public void addTask(DelayedExecutionUnit task) {
		delayedQueue.add(task);
	}

	public void removeTask(DelayedExecutionUnit task) {
		delayedQueue.remove(task);
	}

	public URL getBaseURL() {
		return null;
	}

	public Component getDrawPanel() {
		return drawPanel;
	}

	public void setCanvas(HTMLCanvasElement canvas) {
		this.canvas = canvas;
	}

	public HTMLCanvasElement getMainCanvas() {
		return canvas;
	}

	public void savePrecompiledList() throws Throwable {
		if (!precompiled.equals(inUse)) {
			BufferedWriter fw = new BufferedWriter(new FileWriter("precompiled"));
			try {
				for (Entry<String, String> entry : inUse.entrySet()) {
					fw.write((String) entry.getKey());
					fw.newLine();
					fw.write((String) entry.getValue());
					fw.newLine();
				}
			} finally {
				fw.close();
			}
		}
	}

	public class Script {
		private org.mozilla.javascript.Script script;

		private Script(org.mozilla.javascript.Script script) {
			this.script = script;
		}

		public void run() {
			script.exec(cx, global);
		}
	}
}