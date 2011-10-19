package com.aicontest.visualizer;

public class BotInputOptions {

	private final String player;
	private final int min;
	private final int max;

	public BotInputOptions(String argString) {
		String[] args = argString.split(",");
		player = args[0];
		String[] turns = args[1].split("-");
		min = Integer.parseInt(turns[0]);
		max = Integer.parseInt(turns[turns.length - 1]);
	}

	public String getPlayer() {
		return player;
	}

	public int getMin() {
		return min;
	}

	public int getMax() {
		return max;
	}
}
