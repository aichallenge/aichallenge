package com.aicontest.visualizer;

import java.awt.MediaTracker;

import netscape.javascript.JSObject;

public class ImageWaitThread extends Thread {

	private MediaTracker mediaTracker;
	private JSObject manager;
	private int imageCount;

	public ImageWaitThread(MediaTracker mediaTracker, JSObject manager, int imageCount) {
		this.mediaTracker = mediaTracker;
		this.manager = manager;
		this.imageCount = imageCount;
	}

	@Override
	public void run() {
		for (int id = 0; id < imageCount; id++) {
			try {
				mediaTracker.waitForID(id);
			} catch (InterruptedException e) {
				// this will render the image in an incomplete state
			}
			if ((mediaTracker.statusID(id, false) & MediaTracker.COMPLETE) != 0) {
				manager.call("success", new Object[] { id });
			} else {
				manager.call("error", new Object[] { id });
			}
		}
	}

}
