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
    //loadGame(gameData);
    //try { Thread.sleep(1000); } catch (Exception e) { }
    makeInterface();
    loadGame(gameData);
    lastFrameTime = System.currentTimeMillis();
  }

  // GUI stuff
  private JLabel    info;
  private VizPanel  viz;
  private JSeparator  sep1;
  private JButton    buttons[];
  private JButton    lastClicked;
  private JButton    pauseButton;
  private JProgressBar  progress;

  private Color colors[] = {
      new Color(255, 64, 64),
      new Color(64, 255, 64),
      new Color(64, 64, 255),
      new Color(255, 255, 64)
  };

  // Animation stuff
  private Timer timer;
  private Game games[];
  private double turn = 0.0;
  private long lastFrameTime = 0;
  private double turnsPerSecond = 2.5;
  private double desiredFramerate = 15.0;
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
    // Create the games by issuing the same orders as the game would
    // copy the object each move into a buffer for rendering
    games = new Game[turns.length];
    // initial game object
    Game tGame = new Game(mapPIT, turns.length + 1, 1, "log.txt");
    tGame.Init();
    for (int i = 0; i < turns.length; ++i) {
      Game thisFrame = (Game)tGame.clone();
      String[] items = turns[i].split("\\,");
      for (int j = 0; j < tGame.NumPlanets(); ++j) {
        Planet p = thisFrame.GetPlanet(j);
        String[] fields = items[j].split("\\.");
	p.Owner(Integer.parseInt(fields[0]));
	p.NumShips(Integer.parseInt(fields[1]));
      }
      //System.out.println("NumPlanets: " + tGame.NumPlanets());
      //System.out.println("Items:");
      //for (int j = 0; j < items.length; ++j) {
      //  System.out.println("    " + j + ": " + items[j]);
      //}
      for (int j = tGame.NumPlanets(); j < items.length; ++j) {
        String[] fields = items[j].split("\\.");
        Fleet f = new Fleet(Integer.parseInt(fields[0]),
			    Integer.parseInt(fields[1]),
			    Integer.parseInt(fields[2]),
			    Integer.parseInt(fields[3]),
			    Integer.parseInt(fields[4]),
			    Integer.parseInt(fields[5]));
	thisFrame.AddFleet(f);
      }
      games[i] = thisFrame;
    }
    for (JButton btn : buttons) {
      if (btn != pauseButton)
        btn.setEnabled(true);
    }
    lastClicked = pauseButton;
    progress.setIndeterminate(false);
    progress.setMaximum(100 * games.length);
    progress.setStringPainted(true);
    progress.setString("0 / " + Integer.toString(games.length));
    int millisecondsBetweenFrames = (int)((double)1000 / desiredFramerate);
    timer = new Timer(millisecondsBetweenFrames, this);
    timer.setActionCommand("animTick");
    update();
  }

  // Processes button click events
  public void actionPerformed(ActionEvent e) {
    String ac = e.getActionCommand();
    if (ac.equals("gotoStart")) {
      pauseButton.doClick();
      gotoStart();
      update();
    } else if (ac.equals("stepBack")) {
      pauseButton.doClick();
      stepBack();
      update();
    } else if (ac.equals("playBack")) {
      lastFrameTime = System.currentTimeMillis();
      playBack();
      updateLastClicked((JButton)(e.getSource()));
    } else if (ac.equals("pause")) {
      pause();
      updateLastClicked((JButton)(e.getSource()));
    } else if (ac.equals("playForward")) {
      lastFrameTime = System.currentTimeMillis();
      playForward();
      updateLastClicked((JButton)(e.getSource()));
    } else if (ac.equals("stepForward")) {
      pauseButton.doClick();
      stepForward();
      update();
    } else if (ac.equals("gotoEnd")) {
      pauseButton.doClick();
      gotoend();
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
    turn = 0;
  }
  public void stepBack() {
    turn -= 1.0;
    if (turn <= 0.0) {
      turn = 0.0;
    }
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
    turn += 1.0;
    double max = (double)(games.length - 1);
    if (turn > max) {
	turn = max;
    }
  }
  public void gotoend() {
    turn = (double)(games.length - 1);
  }

  public void update() {
    progress.setValue((int)(turn * 100));
    progress.setString(Integer.toString((int)Math.floor(turn)) +
    	" / " + Integer.toString(games.length));
    viz.prepare(games[(int)turn], turn);
    viz.repaint();
  }

  // Carries out one frame of animation.
  private void animate() {
    long currentTime = System.currentTimeMillis();
    double turnDiff = turnsPerSecond * (currentTime - lastFrameTime) / 1000;
    turn = forward ? turn + turnDiff : turn - turnDiff;
    if (turn >= games.length - 1) {
      pauseButton.doClick();
      turn = games.length - 1;
    }
    if (turn <= 0) {
      pauseButton.doClick();
      turn = 0;
    }
    lastFrameTime = System.currentTimeMillis();
    update();
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
    progress.setStringPainted(false);

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
