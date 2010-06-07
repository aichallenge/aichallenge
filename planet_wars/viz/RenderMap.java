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
// Author: Jeff Cameron (jeff@jpcameron.com)
//
// This class is for testing the map rendering code. It loads one game state
// from a file and renders it.

import java.awt.image.*;
import java.io.*;
import javax.imageio.*;
import java.awt.*;
import java.util.ArrayList;

public class RenderMap {
    public static void main(String[] args) {
        try {
            if (args.length != 2) {
                System.err.println("USAGE: java RenderMap map.txt image.png");
				System.exit(1);
            }
            Game game = new Game(args[0], 100, 0);
            if (game.Init() == 0) {
                System.err.println("Error while loading map " + args[0]);
                System.exit(1);
            }
			
			ArrayList<Color> colors = new ArrayList<Color>();
			colors.add(new Color(106, 74, 60));
			colors.add(new Color(74, 166, 60));
			colors.add(new Color(204, 51, 63));
			colors.add(new Color(235, 104, 65));
			colors.add(new Color(237, 201, 81)	);
			Color bgColor = new Color(188, 189, 172);
			Color textColor = Color.BLACK;
			Font planetFont = new Font("Sans Serif", Font.BOLD, 11);
			Font fleetFont = new Font("Sans serif", Font.PLAIN, 7);
			
			GraphicsConfiguration gc = GraphicsEnvironment
			.getLocalGraphicsEnvironment().getDefaultScreenDevice()
			.getDefaultConfiguration();
			
			BufferedImage image = gc.createCompatibleImage(640, 480);
			
			Graphics2D _g = (Graphics2D)image.createGraphics();
			
			// Turn on AA/Speed
			_g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
								RenderingHints.VALUE_ANTIALIAS_ON);
			_g.setRenderingHint(RenderingHints.KEY_RENDERING,
								RenderingHints.VALUE_RENDER_SPEED);
			_g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
								RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
			
			game.Render(640, 480, 0.0, null, colors, bgColor, textColor,
						planetFont, fleetFont, _g);
			
            File file = new File(args[1]);
            ImageIO.write(image, "png", file);
        } catch (Exception e) {
            System.err.println(e);
        }
    }
}