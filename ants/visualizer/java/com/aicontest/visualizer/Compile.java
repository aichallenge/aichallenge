package com.aicontest.visualizer;

import java.io.File;
import java.io.IOException;

public class Compile {

	public static void main(String[] args) throws IOException,
			InstantiationException, IllegalAccessException,
			ClassNotFoundException, Throwable {
		File dir = new File(new File(args[0]), args[1]);
		WebWrapper webWrapper = new WebWrapper(dir.toString());
		webWrapper.loadProgram(new Ants());
		webWrapper.savePrecompiledList();
	}

}
