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
import java.util.Hashtable;

public class Viewer extends JApplet {
	private ViewerPanel vp;
	
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
			String _url = "http://www.ai-contest.com/game_info.php?game_id="
			+getParameter("game_id");
			URL url = new URL(_url);
			Scanner scanner = new Scanner(url.openConnection().getInputStream());
			scanner.useDelimiter("\\Z");
			
			String data = scanner.next();
			System.err.println(data);
			//String data = "game_id=4161292\nwinner=\nloser=\ngame_id=1\ndraw=1\ntimestamp=2010-05-30 23:45:02\nplayer_one=j3camero\nplayer_two=jeff_cameron\nplayback_string=0,0,1,30,2:2,4,2,30,2:1,2,0,0,5|0.2.15,1.2.15:::0.2.10,1.2.10:::0.2.8,1.2.8:::0.2.7,1.2.7:::0.2.7,1.2.7:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:::0.2.6,1.2.6:";
			
			String parts[] = data.split("\\n");
			String[] parts2;
			Hashtable dict = new Hashtable();
			for (String part : parts) {
				parts2 = part.split("=");
				if (parts2.length > 1) {
					dict.put(parts2[0], parts2[1]);
				}
			}
			String players[] = {
				(String)dict.get("player_one"),
				(String)dict.get("player_two")
			};

			vp = new ViewerPanel(players,
								 (String)dict.get("playback_string"));
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