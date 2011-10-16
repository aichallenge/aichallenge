package com.aicontest.visualizer;

public class VideoOptions {
	private final int fpt;
	private final String format;

	public VideoOptions(String vidArgString) {
		String[] vidArgs = vidArgString.split(",");
		fpt = Integer.parseInt(vidArgs[0]);
		if (fpt < 1) {
			throw new IllegalArgumentException("FPT must be > 0");
		}
		format = vidArgs[1];
	}

	public int getFpt() {
		return fpt;
	}

	public String getFormat() {
		return format;
	}
}
