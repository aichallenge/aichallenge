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
// This class is for the Vizualizer Swing component.

import java.awt.*;
import java.awt.image.*;
import java.util.*;
import javax.swing.*;
import java.io.*;
import javax.imageio.*;
import java.awt.event.*;

public class VizPanel extends JPanel {
	private Game gameCache;
	private double frameCache = 0;
	private JLabel loader;
	private BufferedImage img;
	private Graphics2D _g;
	private ArrayList<Color> colors;
	private Color bgColor;
	private Color textColor;
	private Font planetFont;
	private Font fleetFont;
	private boolean renderInit = false;
	private BufferedImage bgImage;
	
	// HACKY FIX BECAUSE JAVA BLOWS
	private int oldW = 0, oldH = 0;
	// END HACKY FIX
	
	// Creates the color 'theme' used for the game
	private void initRenderer() {
		bgColor = new Color(188, 189, 172);
		
		colors = new ArrayList<Color>();
		colors.add(new Color(106, 74, 60));
		colors.add(new Color(74, 166, 60));
		colors.add(new Color(204, 51, 63));
		colors.add(new Color(235, 104, 65));
		colors.add(new Color(237, 201, 81));
		// anything after this is probably not necessary yet!
		
		textColor = Color.BLACK;
		
		planetFont = new Font("Sans Serif", Font.BOLD, 11);
		fleetFont = new Font("Sans serif", Font.PLAIN, 7);
		
		try {
			// NOTE:	We don't have permission to use this image yet
			//			so it is just here for 'testing' purposes until
			//			we either contact the creator or find one that
			//			we are allowed to use.
			bgImage = ImageIO.read(getClass().getResource("img/space.jpg"));
		} catch (IOException Err) {
			// do nothing
		}
		
		_ginit();
		
		renderInit = true;
	}
	private void _ginit() {
		// Set up the graphics context.
		if (img != null) {
			_g.dispose();
			img.flush();
		}
		
		GraphicsConfiguration gc = GraphicsEnvironment
		.getLocalGraphicsEnvironment().getDefaultScreenDevice()
		.getDefaultConfiguration();
		
		img = gc.createCompatibleImage(getWidth(), getHeight());

		_g = img.createGraphics();
		
		// Turn on AA/Speed
		_g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
							RenderingHints.VALUE_ANTIALIAS_ON);
		_g.setRenderingHint(RenderingHints.KEY_RENDERING,
							RenderingHints.VALUE_RENDER_SPEED);
		_g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
							RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
	}
	
	public VizPanel() {
		try {
			ImageIcon _loader = new ImageIcon("img/loader.gif");
			loader = new JLabel(_loader);
			_loader.setImageObserver(loader);

			setLayout(new BorderLayout());
			add(loader, BorderLayout.CENTER);
		}
		catch (Exception err) {
			loader = null;
		}
	}
	
	// Render a % of a single frame.
	// Returns the frame progress (1-frameMax) or negative if the game object
	// is no longer useful
	public int think(Game game, int frame, int frameMax, boolean forward) {
		if (loader != null) {
			initRenderer();
			remove(loader);
			loader = null;
		}
		
		gameCache = game;
		if (forward) {
			if (frame >= frameMax) {
				frameCache = 1.0;
				return -1;
			} else {
				frameCache = (double)frame/(double)frameMax;
				return frame + 1;
			}
		} else {
			frameCache = (double)frame/(double)frameMax;
			return frame - 1;
		}
	}

	public void paint(Graphics g){
		int width = getWidth(), height = getHeight();
		if (!renderInit || gameCache == null) {
			if (loader != null) {
				loader.paint(g);
			}
		} else {
			if (width != oldW || height != oldH) {
				_ginit();
				oldW = width;
				oldH = height;
			}
			gameCache.Render(width, height, frameCache,
							 bgImage, colors, bgColor, textColor,
							 planetFont, fleetFont, (Graphics2D) _g);
			g.drawImage(img, 0, 0, null);
		}
	}
}