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
// This class is for the viewer panel.

import java.net.*;
import java.io.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class ViewerPanel extends JPanel implements ActionListener, Runnable {
	
	public ViewerPanel(String _players[],  String gameData) {
		players = _players;
		makeInterface();
		loadGame(gameData);
	}
	
	// GUI stuff
	private JLabel		info;
	private VizPanel	viz;
	private JSeparator	sep1;
	private JButton		buttons[];
	private JButton		lastClicked;
	private JButton		pauseButton;
	private JProgressBar	progress;
	
	private Color colors[] = { new Color(74, 166, 60),
		new Color(204, 51, 63), new Color(235, 104, 65), new Color(237, 201, 81)	
	};
	
	// Animation stuff
	private Timer timer;
	private Game games[];
	private int pos = 0;
	private int frame = 0;
	private int _frame = 0;
	private int frameMax = 12;
	private int animSpeed = 25;
	private boolean forward = true;
	private Thread loadThread;
	private String gameData;
	private String players[];
	
	private void loadGame(String _gameData) {
		gameData = _gameData;
		loadThread = new Thread(this);
		loadThread.start();
	}
	
	// Makes the game objects
	public void run() {
		// Make the initial map data, convert to PIT format
		String parts[] = gameData.split("\\|");
		String planets[] = parts[0].split(":");
		String planetData[];
		
		StringBuilder map = new StringBuilder();
		StringBuilder planet;
		for (String p : planets) {
			planetData = p.split("\\,");
			planet = new StringBuilder("P");
			
			for (String _p : planetData) {
				planet.append(" ");
				planet.append(_p);
			}
			map.append(planet.toString());
			map.append("\n");
		}
		
		String mapPIT = map.toString();
		
		String turns[] = parts[1].split(":");
		String fleets[];
		String fleetData[];
		
		// Create the games by issuing the same orders as the game would
		// copy the object each move into a buffer for rendering
		games = new Game[turns.length];
		
		// initial game object
		Game tGame = new Game(mapPIT, turns.length + 1, 1);
		tGame.Init();
		
		int i = 0;
		for (String turn : turns) {			
			// Issue fleet orders
			fleets = turn.split("\\,");
			for (String f : fleets) {
				fleetData = f.split("\\.");
				
				if (fleetData.length > 1) {
					int source = Integer.parseInt(fleetData[0]);
					int target = Integer.parseInt(fleetData[1]);
					int ships = Integer.parseInt(fleetData[2]);
					int player = tGame.GetPlanet(source).Owner();
					tGame.IssueOrder(player, source, target, ships);
				}
			}
			
			// Clone the object, store it in the array
			games[i++] = (Game)(tGame.clone());
			
			// Do a time step
			tGame.DoTimeStep();
		}
		
		for (JButton btn : buttons) {
			if (btn != pauseButton)
				btn.setEnabled(true);
		}
		lastClicked = pauseButton;
		
		progress.setIndeterminate(false);
		progress.setMaximum(i * frameMax);
		
		timer = new Timer(animSpeed, this);
		timer.setActionCommand("animTick");
		
		update();
	}
	
	// Processes button click events
	public void actionPerformed(ActionEvent e) {
		String ac = e.getActionCommand();
		if (ac.equals("gotoStart")) {
			pauseButton.doClick();
			gotoStart();
			frame = 0;
			update();
		} else if (ac.equals("stepBack")) {
			pauseButton.doClick();
			stepBack();
			frame = 0;
			update();
		} else if (ac.equals("playBack")) {
			playBack();
			updateLastClicked((JButton)(e.getSource()));
		} else if (ac.equals("pause")) {
			pause();
			updateLastClicked((JButton)(e.getSource()));
		} else if (ac.equals("playForward")) {
			playForward();
			updateLastClicked((JButton)(e.getSource()));
		} else if (ac.equals("stepForward")) {
			pauseButton.doClick();
			stepForward();
			frame = frameMax;
			update();
		} else if (ac.equals("gotoEnd")) {
			pauseButton.doClick();
			gotoend();
			frame = frameMax;
			update();
		} else if (ac.equals("animTick")) {
			animate();
		} else {
			// TODO: Error? Idk.
		}
	}
	
	private void updateLastClicked(JButton btn) {
		if (lastClicked != null) {
			lastClicked.setEnabled(true);
		}
		btn.setEnabled(false);
		lastClicked = btn;
	}
	
	public void gotoStart() {
		pos = 0;
	}
	public void stepBack() {
		pos = (pos <= 0) ? 0 : pos - 1;
	}
	public void playBack() {
		forward = false;
		timer.start();
	}
	public void pause() {
		timer.stop();
	}
	public void playForward() {
		forward = true;
		timer.start();
	}
	public void stepForward() {
		pos = (pos >= games.length - 1) ? games.length - 1 : pos + 1;
	}
	public void gotoend() {
		pos = games.length - 1;
	}
	
	public void update() {
		progress.setValue(frame + pos*frameMax);
		_frame = viz.think(games[pos], frame, frameMax, forward);
		viz.repaint();
	}
	
	// Performs animation playback
	private void animate() {
		update();
		if (_frame < 0) {
			if (forward) {
				if (pos == games.length - 1) {
					pauseButton.doClick();
					frame = frameMax;
				} else {
					stepForward();
					frame = 0;	
				}
			} else {
				if (pos == 0) {
					pauseButton.doClick();
					frame = 0;
				} else {
					stepBack();
					frame = frameMax;
				}
			}
		} else {
			frame = _frame;
		}
		
	}
	
	// Creates the user interface for the viewer
	private void makeInterface() {
		setSize(800, 600);
		
		// Configure the content pane
		setBackground(Color.WHITE);
		setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();
		
		viz = new VizPanel();
		viz.setSize(640, 480);
		
		JLabel pl;
		JLabel legend[] = new JLabel[players.length];
		viz.addColor(new Color(106, 74, 60)); // neutral color
		for (int i = 0; i < players.length; i++) {
			pl = new JLabel(players[i], new ColorBox(colors[i]), SwingConstants.LEADING);
			legend[i] = pl;
			viz.addColor(colors[i]);
		}
		
		sep1 = new JSeparator();
		
		// length TO BE SET
		progress = new JProgressBar();
		progress.setIndeterminate(true);
		
		buttons = new JButton[7];
		String icons[] = {
			"gotoStart", "stepBack", "playBack", "pause", "playForward",
			"stepForward", "gotoEnd"
		};
		
		JButton btn;
		for (int i = 0; i < icons.length; i++) {
			btn = new JButton(new ImageIcon(getClass().getResource("img/" + icons[i] + ".png")));
			if (icons[i].equals("pause")) {
				pauseButton = btn;
			}
			btn.setName(icons[i]);
			btn.addActionListener(this);
			btn.setActionCommand(icons[i]);
			btn.setEnabled(false); // renable when loaded..
			btn.setFocusPainted(false);
			buttons[i] = btn;
		}
		
		// LAYOUT TIME GHRHAHGHARHAR
		c.fill = GridBagConstraints.BOTH;
		c.anchor = GridBagConstraints.CENTER;
		c.weightx = 0.0;
		c.weighty = 0.0;
		c.gridx = 0;
		c.gridy = 0;
		
		JPanel legendPanel = new JPanel();
		legendPanel.setBackground(Color.WHITE);
		legendPanel.setLayout(new FlowLayout());
		for (int i = 0; i < legend.length; i++) {
			legendPanel.add(legend[i]);
		}
		add(legendPanel, c);
		
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridy++;
		add(viz, c);
		
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 0.0;
		c.weighty = 0.0;
		c.gridy++;
		add(progress, c);
		c.gridy++;
		add(sep1, c);
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.setBackground(Color.WHITE);
		buttonPanel.setLayout(new GridBagLayout());
		GridBagConstraints bc = new GridBagConstraints();
		
		c.fill = GridBagConstraints.NONE;
		c.gridx = 0;
		c.gridy++;
		
		bc.fill = GridBagConstraints.NONE;
		bc.gridx = 0;
		bc.gridy = 0;
		for (int i = 0; i < buttons.length; i++) {
			buttonPanel.add(buttons[i], bc);
			bc.gridx++;
		}
		
		add(buttonPanel, c);
	}
	
}