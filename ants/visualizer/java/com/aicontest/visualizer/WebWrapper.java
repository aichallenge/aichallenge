package com.aicontest.visualizer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.security.AccessControlException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.locks.ReentrantLock;

import javax.swing.JOptionPane;
import javax.swing.UIManager;

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
import org.mozilla.javascript.json.JsonParser;
import org.mozilla.javascript.json.JsonParser.ParseException;
import org.mozilla.javascript.optimizer.Codegen;

import com.aicontest.visualizer.js.dom.Console;
import com.aicontest.visualizer.js.dom.HTMLCanvasElement;
import com.aicontest.visualizer.js.dom.HTMLImageElement;
import com.aicontest.visualizer.js.dom.Navigator;
import com.aicontest.visualizer.js.dom.XMLHttpRequest;
import com.aicontest.visualizer.js.tasks.DelayedExecutionUnit;
import com.aicontest.visualizer.js.tasks.EventExecutionUnit;
import com.aicontest.visualizer.js.tasks.IExecutionUnit;

public class WebWrapper {

	private File baseDir;
	private final ReentrantLock lock = new ReentrantLock();
	private final ExecutionUnitQueue queue = new ExecutionUnitQueue();
	private Map<String, String> precompiled = new HashMap<String, String>();
	private Map<String, String> inUse = new HashMap<String, String>();
	private Thread jsThread = Thread.currentThread();
	private Codegen compiler;
	private CompilerEnvirons compilerEnv;
	private ErrorReporter compilationErrorReporter;
	private GeneratedClassLoader loader;
	private volatile boolean running = true;
	private JsonParser jsonParser;
	protected ArrayList<WebWrapper.Script> scripts = new ArrayList<WebWrapper.Script>();
	protected Context cx;
	protected ScriptableObject global;
	protected HTMLCanvasElement canvas;

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
		global = cx.initStandardObjects();
		cx.putThreadLocal(WebWrapper.class, this);
		Object image = new NativeJavaClass(global, HTMLImageElement.class);
		global.put("Image", global, image);
		Object xmlHttpRequest = new NativeJavaClass(global,
				XMLHttpRequest.class);
		global.put("XMLHttpRequest", global, xmlHttpRequest);
		new Console(global, "console");
		new Navigator(global, "navigator");
		jsonParser = new JsonParser(cx, global);
		String[] names = { "alert", "prompt", "confirm" };
		global.defineFunctionProperties(names, WebWrapper.class,
				ScriptableObject.DONTENUM);
	}

	public static void alert(Object o) {
		String message = o.toString();
		JOptionPane.showMessageDialog(null, message);
	}

	public static boolean confirm(Object o) {
		String message = o.toString();
		return JOptionPane.showConfirmDialog(null, message,
				UIManager.getString("OptionPane.titleText"),
				JOptionPane.OK_CANCEL_OPTION) == JOptionPane.OK_OPTION;
	}

	public static String prompt(Object o, Object def) {
		String message = o.toString();
		return JOptionPane.showInputDialog(message, def.toString());
	}

	private Class<?> recompile(String file) throws IOException {
		if (compiler == null) {
			compiler = new Codegen();
		}
		FileReader in = new FileReader(new File(baseDir, file));
		try {
			if (compilerEnv == null) {
				compilerEnv = new CompilerEnvirons();
				compilerEnv.initFromContext(cx);
				compilerEnv.setOptimizationLevel(1);
				compilerEnv.setGeneratingSource(false);
			}
			if (compilationErrorReporter == null) {
				compilationErrorReporter = compilerEnv.getErrorReporter();
			}
			Parser p = new Parser(compilerEnv, compilationErrorReporter);
			AstRoot ast = p.parse(in, file.toString(), 1);
			IRFactory irf = new IRFactory(compilerEnv, compilationErrorReporter);
			ScriptNode tree = irf.transformTree(ast);
			Object[] nameBytesPair = (Object[]) (Object[]) compiler.compile(
					compilerEnv, tree, tree.getEncodedSource(), false);
			String className = (String) nameBytesPair[0];
			byte[] classBytes = (byte[]) (byte[]) nameBytesPair[1];
			File outFile = new File(className.replace('.', File.separatorChar)
					+ ".class");
			String oldClassName = (String) precompiled.get(file);
			if (oldClassName != null) {
				new File(oldClassName.replace('.', File.separatorChar)
						+ ".class").delete();
			}
			System.out.println("Saving compiled " + file + " -> " + outFile);
			outFile.getParentFile().mkdirs();
			FileOutputStream fos = new FileOutputStream(outFile);
			try {
				fos.write(classBytes);
			} finally {
				fos.close();
			}
			if (loader == null) {
				loader = SecurityController.createLoader(getClass()
						.getClassLoader(), null);
			}
			Class<?> clazz = loader.defineClass(className, classBytes);
			loader.linkClass(clazz);
			return clazz;
		} finally {
			in.close();
		}
	}

	public Script loadJs(String file) throws InstantiationException,
			IllegalAccessException, IOException {
		Class<?> clazz = null;
		String objClassName = (String) precompiled.get(file);
		if (objClassName == null) {
			clazz = recompile(file);
			objClassName = clazz.getCanonicalName();
		} else {
			File objFile = new File(objClassName.replace('.',
					File.separatorChar) + ".class");
			boolean useExisting = baseDir == null;
			if (!useExisting) {
				File jsFile = new File(baseDir, file);
				try {
					useExisting = !jsFile.canRead()
							|| (jsFile.lastModified() <= objFile.lastModified());
				} catch (AccessControlException e) {
				}
			}
			if (useExisting) {
				try {
					clazz = getClass().getClassLoader().loadClass(objClassName);
				} catch (ClassFormatError e) {
					System.err.println("bundled class " + objClassName
							+ " has invalid format");
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

	public void loop() {
		while (running) {
			try {
				// the thread must not be interrupted during execution
				lock.lock();
				try {
					queue.take().execute(cx, global);
					postExecute();
				} finally {
					lock.unlock();
				}
			} catch (InterruptedException e) {
				Thread.interrupted();
			}
		}
	}

	public Object eval(String script) {
		return cx.evaluateString(global, script, "<anonymous>", 1, null);
	}

	public Object invoke(Scriptable thiz, String functionName, Object[] args) {
		Function f = (Function) ScriptableObject
				.getProperty(thiz, functionName);
		return f.call(cx, global, thiz, args);
	}

	public Object parseJSON(String json) throws ParseException {
		return jsonParser.parseValue(json);
	}

	public static WebWrapper getInstance() {
		Context cx = Context.enter();
		try {
			WebWrapper localWebWrapper = (WebWrapper) cx
					.getThreadLocal(WebWrapper.class);
			return localWebWrapper;
		} finally {
			Context.exit();
		}
	}

	protected void postExecute() {
	}

	public void addTask(IExecutionUnit task) {
		queue.add(task);
	}

	public void addCompressedEventTask(EventExecutionUnit task) {
		queue.compressEvent(task);
	}

	public void removeTask(DelayedExecutionUnit task) {
		queue.remove(task);
	}

	public URL getBaseURL() {
		return null;
	}

	public HTMLCanvasElement getMainCanvas() {
		return canvas;
	}

	public void savePrecompiledList() throws Throwable {
		if (!precompiled.equals(inUse)) {
			BufferedWriter fw = new BufferedWriter(
					new FileWriter("precompiled"));
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

	public void loadProgram(IProgram program) throws InstantiationException,
			IllegalAccessException, IOException {
		String[] files = program.getFiles();
		for (String file : files) {
			scripts.add(loadJs(file));
		}
	}

	public void exit() {
		synchronized (lock) {
			running = false;
			jsThread.interrupt();
		}
		try {
			jsThread.join();
		} catch (InterruptedException e) {
			e.printStackTrace();
			System.exit(1);
		}
	}

	public ScriptableObject getGlobal() {
		return global;
	}

	protected void runProgram() {
		for (WebWrapper.Script script : scripts) {
			script.run();
		}
	}
}
