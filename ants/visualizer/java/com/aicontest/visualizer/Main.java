package com.aicontest.visualizer;

import java.awt.Color;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.Panel;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URI;
import java.net.URISyntaxException;

import org.mozilla.javascript.NativeArray;
import org.mozilla.javascript.ScriptableObject;
import org.mozilla.javascript.Undefined;

import com.aicontest.visualizer.js.dom.HTMLDocument;
import com.aicontest.visualizer.js.dom.XMLHttpRequest;

public class Main implements IVisualizerUser, WindowListener {

	private static Boolean decorated;
	private static int width = 640;
	private static int height = 640;
	private Panel visualizerPanel;
	private Frame frame;
	private Visualizer visualizer;
	private HTMLDocument document;
	private VideoCapture videoCapture;

	public static void main(String[] args) {
		try {
			if (args.length == 0) {
				System.out.println("Reading live stream from stdin...");
				new Main();
			} else {
				String fileName = null;
				VideoOptions video = null;
				boolean moreOptions = true;
				BotInputOptions botInput = null;
				for (String arg : args) {
					if (moreOptions && arg.startsWith("--")) {
						try {
							if (arg.equals("--")) {
								moreOptions = false;
							} else if (arg.equals("--help")) {
								printHelp(System.out);
								System.exit(0);
							} else if (arg.startsWith("--video=")) {
								String vidArgString = arg.substring(8);
								video = new VideoOptions(vidArgString);
							} else if (arg.startsWith("--botinput=")) {
								String argString = arg.substring(11);
								botInput = new BotInputOptions(argString);
							} else if (arg.startsWith("--decorated=")) {
								decorated = Boolean.parseBoolean(arg
										.substring(12));
							} else if (arg.startsWith("--width=")) {
								width = Math.max(1,
										Integer.parseInt(arg.substring(8)));
							} else if (arg.startsWith("--height=")) {
								height = Math.max(1,
										Integer.parseInt(arg.substring(9)));
							} else {
								invalidOption(arg);
							}
						} catch (Exception e) {
							invalidOption(arg);
						}
					} else if (moreOptions && arg.startsWith("-")) {
						if (arg.equals("-h")) {
							printHelp(System.out);
							System.exit(0);
						} else {
							invalidOption(arg);
						}
					} else if (fileName == null) {
						fileName = arg;
					} else {
						printHelp(System.err);
						System.err
								.println("(You seem to have tried more than one file name.)");
						System.exit(1);
					}
				}
				if (fileName == null) {
					printHelp(System.err);
					System.err.println("(The visualizer needs a file name.)");
					System.exit(1);
				}
				new Main(fileName, video, botInput);
			}
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
	}

	private static void invalidOption(String arg) {
		printHelp(System.err);
		System.err.println("(Invalid option: " + arg + ")");
		System.exit(1);
	}

	private static void printHelp(PrintStream out) {
		out.println("Syntax: <options> filename");
		out.println("Options:");
		out.println("                  -h / --help : Prints this help.");
		out.println("                --width=<int> : Set the display width in pixels.");
		out.println("               --height=<int> : Set the display height in pixels.");
		out.println("     --decorated=[true/false] : Enables or disable GUI elements around the visualizer.");
		out.println("          --video=<fpt>,<fmt> : Enable video output mode at");
		out.println("                                <fpt> frames per turn using");
		out.println("                                <ext> as file extension.");
		out.println("--botinput=<player>,<turn(s)> : Generate bot intput for a replay.");
		out.println("                                <player> name of the bot");
		out.println("                                <turn(s)> A single turn number or a range like 1-1000.");
		out.println();
		out.println("Video images will be numbered 00000000.<fmt> to 99999999.<fmt>.");
		out.println("Example call that generates ten frames per turn without user interface:");
		out.println("  --width=1280 --height=720 --decorated=false --video=10,png example.replay");
		out.println("MPEG-4 encoding with ffmpeg at the default 25 FPS (results in 2.5 turns per second):");
		out.println("  ffmpeg -i %08d.png movie.mp4");
	}

	private ScriptableObject init(boolean resizable)
			throws InstantiationException, IllegalAccessException, IOException {
		visualizer = new Visualizer(this, width, height, resizable);
		document = visualizer.getDomWindow().getDocument();
		ScriptableObject options = visualizer.construct("Options", null);
		options.put("data_dir", options, "/");
		if (decorated != null) {
			options.put("decorated", options, decorated);
		}
		return options;
	}

	private ScriptableObject construct(ScriptableObject options, Object config,
			VideoOptions video) throws IOException {
		// in video recording mode we want no user interaction
		if (video != null) {
			options.put("interactive", options, false);
			options.put("loop", options, false);
		}
		ScriptableObject vis = visualizer.construct("Visualizer", new Object[] {
				document, options, Undefined.instance, Undefined.instance,
				config });
		if (video != null) {
			videoCapture = new VideoCapture(visualizer, frame,
					video.getFormat());
			ScriptableObject.defineProperty(visualizer.global, "video",
					videoCapture, ScriptableObject.DONTENUM);
			visualizer.invoke(vis, "javaVideoOutput",
					new Object[] { video.getFpt() });
		}
		return vis;
	}

	public Main(String replay, VideoOptions video, BotInputOptions botInput)
			throws InstantiationException, IllegalAccessException, IOException,
			URISyntaxException {
		if (replay.endsWith(".stream")) {
			if (video != null) {
				System.err
						.println("Cannot record videos for logged streaming replays. Use a storage format replay instead.");
				System.exit(1);
			} else if (botInput != null) {
				System.err
						.println("Cannot create bot input from logged streaming replays. Use a storage format replay instead.");
				System.exit(1);
			} else {
				startStream(new FileInputStream(replay));
			}
		} else {
			if (botInput != null) {
				startBotInput(replay, botInput);
			} else if (botInput == null || video != null) {
				startReplay(replay, video);
			}
		}
	}

	private void startStream(InputStream inputStream)
			throws InstantiationException, IllegalAccessException, IOException {
		ScriptableObject options = init(true);
		ScriptableObject vis = construct(options, Undefined.instance, null);
		new Stream(vis, visualizer, inputStream, "stream");
		visualizer.loop();
	}

	private void startReplay(String replay, VideoOptions video)
			throws URISyntaxException, InstantiationException,
			IllegalAccessException, IOException {
		URI uri = replayStringToUri(replay);
		ScriptableObject options = init(video == null);
		ScriptableObject vis = construct(options, Undefined.instance, video);
		visualizer.invoke(vis, "loadReplayDataFromURI", new Object[] { uri });
		visualizer.loop();
	}

	private void startBotInput(String replaySource, BotInputOptions botInput)
			throws URISyntaxException, InstantiationException,
			IllegalAccessException, IOException {
		WebWrapper ww = new WebWrapper(getJavaScriptPath());
		ww.loadProgram(getProgram());
		ww.runProgram();
		URI uri = replayStringToUri(replaySource);
		XMLHttpRequest xhr = new XMLHttpRequest();
		xhr.open("GET", uri.toString());
		try {
			xhr.send();
		} catch (IOException e) {
			System.err.println("Could not load " + replaySource + ": " + e);
			System.exit(1);
		}
		String replayStr = xhr.getResponseText();
		ScriptableObject replay = ww.construct("Replay",
				new Object[] { replayStr });
		ScriptableObject meta = (ScriptableObject) replay.get("meta", replay);
		NativeArray playernames = (NativeArray) meta.get("playernames", meta);
		int userIndex = playernames.indexOf(botInput.getPlayer());
		if (userIndex == -1) {
			System.err.println(botInput.getPlayer()
					+ " does not exist in the replay");
			System.exit(1);
		} else if (botInput.getMin() < 1
				|| botInput.getMax() > (Double) replay.get("duration", replay)
				|| botInput.getMin() > botInput.getMax()) {
			System.err.println("You requested turns " + botInput.getMin() + "-"
					+ botInput.getMax() + ", but the range is 1-"
					+ ((Double) replay.get("duration", replay)).intValue());
		}
		String result = (String) ww.invoke(
				replay,
				"generateBotInput",
				new Object[] { userIndex, botInput.getMin() - 1,
						botInput.getMax() - 1 });
		System.out.print(result);
	}

	private URI replayStringToUri(String replay) throws URISyntaxException {
		URI uri = null;
		try {
			uri = new URI(replay);
		} catch (URISyntaxException e) {
		}
		if (uri == null || uri.getScheme() == null) {
			uri = new URI("file", replay, null);
		}
		return uri;
	}

	public Main() throws IOException, InstantiationException,
			IllegalAccessException {
		startStream(System.in);
	}

	@Override
	public String getJavaScriptPath() {
		return "../../js";
	}

	@Override
	public IProgram getProgram() {
		final Ants ants = new Ants();
		return ants;
	}

	@Override
	public void setVisualizerPanel(Panel visualizerPanel, boolean resizable) {
		this.visualizerPanel = visualizerPanel;
		frame = new Frame("Ants Visualizer");
		frame.setBackground(Color.WHITE);
		frame.add(visualizerPanel);
		frame.pack();
		frame.setLocationByPlatform(true);
		frame.addWindowListener(this);
		frame.setResizable(resizable);
		frame.setVisible(true);
	}

	@Override
	public boolean setFullScreen(boolean enable) {
		GraphicsConfiguration winGfxConf = frame.getGraphicsConfiguration();
		GraphicsDevice dev = winGfxConf.getDevice();
		Frame fsWin = (Frame) dev.getFullScreenWindow();
		if (fsWin == null && enable) {
			frame.setVisible(false);
			fsWin = new Frame();
			fsWin.setUndecorated(true);
			fsWin.add(visualizerPanel);
			fsWin.setVisible(true);
			winGfxConf.getDevice().setFullScreenWindow(fsWin);
		} else if (fsWin != null && !enable) {
			fsWin.dispose();
			winGfxConf.getDevice().setFullScreenWindow(null);
			frame.add(visualizerPanel);
			frame.setVisible(true);
		}
		return enable;
	}

	@Override
	public void windowOpened(WindowEvent e) {
	}

	@Override
	public void windowClosing(WindowEvent e) {
		visualizer.exit();
		if (videoCapture != null) {
			videoCapture.quit();
		}
		e.getWindow().dispose();
	}

	@Override
	public void windowClosed(WindowEvent e) {
	}

	@Override
	public void windowIconified(WindowEvent e) {
	}

	@Override
	public void windowDeiconified(WindowEvent e) {
	}

	@Override
	public void windowActivated(WindowEvent e) {
	}

	@Override
	public void windowDeactivated(WindowEvent e) {
	}
}
