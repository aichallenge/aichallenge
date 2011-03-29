package com.aicontest.visualizer;

import java.io.File;
import java.io.IOException;

public class Compile {

	public static void main(String[] args) throws IOException, InstantiationException, IllegalAccessException, ClassNotFoundException, Throwable {
		new VisualizerWebWrapper(new File(new File(args[0]), args[1]).toString()).savePrecompiledList();
	}

}
