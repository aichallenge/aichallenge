/*<applet code=Viewer.class width=640 height=480>
	<param name="game_id" value="413"/>
	<param name="DEBUG" value=true />
 </applet> 
 */
// ^^^^ STRICTLY FOR TESTING ^^^^

// Copyright 2010 owners of the AI Challenge project
//
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy
// of the License at http://www.apache.org/licenses/LICENSE-2.0 . Unless
// required by applicable law or agreed to in writing, software distributed
// under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.
//
// Author: Patrick Paskaris
//
// This class is for the viewer applet.

import java.net.URL;
import javax.swing.*;
import java.util.Scanner;

public class Viewer extends JApplet {
	ViewerPanel vp;
	
	public void init() {
		try {
			javax.swing.SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					firstrun();
				}
			});
		} catch (Exception e) {
			System.out.println("Error loading interface!\nPlease contact a contest admin with this error!");
		}
	}
	
	// Executes on first run, what this needs to do:
	// 1. Get the 'final' playback format from the given URL in the params
	// 2. Pass it to ViewerPanel so it can make games out of them.
	private void firstrun() {
		try {
			/*
			 String _url = "http://csclub.uwaterloo.ca/~pgpaskar/planet_wars/test"+getParameter("game_id")+".html";
			URL url = new URL(_url);
			Scanner scanner = new Scanner(url.openConnection().getInputStream());
			scanner.useDelimiter("\\Z");
			
			String data = scanner.next();
			 */
			String data = "0,0,1,30,2:2,4,2,30,2:1,2,0,0,5|0.2.15,1.2.15:::0.2.10,1.2.10:::0.2.8,1.2.8:::0.2.7,1.2.7:::0.2.7,1.2.7:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::1.2.6,0.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:";
			
			vp = new ViewerPanel(data);
			add(vp);
		}
		catch (Exception err) {
			// Add a text handler for VizPanel to display errors or something
			StringBuilder sb = new StringBuilder();
			for (StackTraceElement el : err.getStackTrace()) {
				sb.append(el);
				sb.append("<BR>");
			}
			JLabel error = new JLabel("<HTML><STRONG>Error:  Visualizer was unable to correctly parse the game data!</STRONG><BR><BR>"
									  + err.getMessage() + "<BR>" + ((getParameter("DEBUG") != null) ? sb.toString() : ""));
			add(error);
		}
	}
}