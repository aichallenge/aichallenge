package com.aicontest.visualizer;

import java.awt.Frame;
import java.awt.Image;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.stream.FileImageOutputStream;
import javax.swing.SwingUtilities;

public class VideoCapture {

	private final WebWrapper visualizer;
	private final Frame frame;
	private int num = 0;
	private VideoCompressorPool videoCompressorPool;
	private long start;

	public VideoCapture(WebWrapper visualizer, Frame frame, String format)
			throws IOException {
		this.visualizer = visualizer;
		this.frame = frame;
		String fileNameFormat = String.format("%%0%dd." + format, 8);
		int processors = Runtime.getRuntime().availableProcessors();
		videoCompressorPool = new VideoCompressorPool(processors, format,
				fileNameFormat);
	}

	public void captureFrame(double time, int duration) {
		System.out.print("Compressing image " + num);
		long now = new Date().getTime();
		if (time == 0) {
			start = now;
		} else {
			System.out.print(" - ETA: ");
			double percent = time / duration;
			double msPerPercent = (now - start) / percent;
			long remainingTime = (long) Math.ceil((1 - percent) * msPerPercent
					/ 1000);
			if (remainingTime > 3600) {
				System.out.print((remainingTime / 3600) + "h, ");
				remainingTime %= 3600;
			}
			if (remainingTime > 60) {
				System.out.print((remainingTime / 60) + "m, ");
				remainingTime %= 60;
			}
			System.out.print(remainingTime + "s");
		}
		System.out.println();
		BufferedImage image = visualizer.canvas.getPixmap();
		videoCompressorPool.compress(image, num++);
		if (time == duration) {
			quit();
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					frame.dispatchEvent(new WindowEvent(frame,
							WindowEvent.WINDOW_CLOSING));
				}
			});
		}
	}

	public void quit() {
		videoCompressorPool.quit();
	}
}

class VideoCompressorPool {
	private final PoolWorker[] threads;
	private final LinkedList<NumberedImage> queue;
	private final String fileNameFormat;
	private volatile boolean running = true;

	public VideoCompressorPool(int nThreads, String format,
			String fileNameFormat) {
		this.fileNameFormat = fileNameFormat;
		queue = new LinkedList<NumberedImage>();
		threads = new PoolWorker[nThreads];

		for (int i = 0; i < nThreads; i++) {
			threads[i] = new PoolWorker(format);
			threads[i].start();
		}
	}

	public void quit() {
		running = false;
		for (int i = 0; i < threads.length; i++) {
			PoolWorker thread = threads[i];
			thread.interrupt();
			try {
				thread.join();
			} catch (InterruptedException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
	}

	public void compress(BufferedImage image, int num) {
		synchronized (queue) {
			while (!queue.isEmpty()) {
				try {
					queue.wait();
				} catch (InterruptedException ignored) {
				}
			}
			queue.addLast(new NumberedImage(image, num));
			queue.notify();
		}
	}

	private class NumberedImage {
		final BufferedImage image;
		final int num;

		public NumberedImage(BufferedImage image, int num) {
			this.image = image;
			this.num = num;
		}
	}

	private class PoolWorker extends Thread implements ImageObserver {
		private ImageWriter writer;

		public PoolWorker(String format) {
			Iterator<ImageWriter> writers = ImageIO
					.getImageWritersBySuffix(format);
			if (writers.hasNext()) {
				writer = writers.next();
			}
		}

		public void run() {
			NumberedImage image;

			while (true) {
				synchronized (queue) {
					while (queue.isEmpty()) {
						if (!running) return;
						try {
							queue.wait();
						} catch (InterruptedException ignored) {
						}
					}
					image = queue.removeFirst();
					queue.notifyAll();
				}

				try {
					File file = new File(String.format(fileNameFormat,
							image.num));
					FileImageOutputStream os = new FileImageOutputStream(file);
					try {
						BufferedImage normalized = new BufferedImage(
								image.image.getWidth(),
								image.image.getHeight(),
								BufferedImage.TYPE_INT_RGB);
						normalized.getGraphics().drawImage(image.image, 0, 0,
								this);
						writer.setOutput(os);
						writer.write(normalized);
					} finally {
						os.close();
					}
				} catch (Exception e) {
					System.err.println("Couldn't store video frame "
							+ image.num + ": " + e.getMessage());
				}
			}
		}

		@Override
		public boolean imageUpdate(Image img, int infoflags, int x, int y,
				int width, int height) {
			return true;
		}
	}
}
