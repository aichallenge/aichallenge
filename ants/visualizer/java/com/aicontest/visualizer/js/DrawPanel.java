package com.aicontest.visualizer.js;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;

import javax.swing.JPanel;

import com.aicontest.visualizer.js.dom.HTMLCanvasElement;

public class DrawPanel extends JPanel
{
  private static final long serialVersionUID = -2354974754646841772L;
  private Image offscreen;
  private Graphics offscreenGraphics;

  public DrawPanel()
  {
    super(false);
  }

  public void setBounds(int x, int y, int width, int height)
  {
    super.setBounds(x, y, width, height);
    if ((width > 0) && (height > 0)) {
      offscreen = createImage(width, height);
      offscreenGraphics = offscreen.getGraphics();
    }
  }

  public synchronized void paint(Graphics g)
  {
    g.drawImage(offscreen, 0, 0, this);
    getToolkit().sync();
  }

  public synchronized void repaint()
  {
    if (offscreenGraphics != null) {
      WebWrapper webWrapper = WebWrapper.getInstance();

      if (webWrapper != null) {
        HTMLCanvasElement mainCanvas = webWrapper.getMainCanvas();
        if ((mainCanvas != null) && (mainCanvas.getContext("2d").isDrawn())) {
          BufferedImage pixmap = mainCanvas.getPixmap();
          offscreenGraphics.clearRect(0, 0, getWidth(), getHeight());
          offscreenGraphics.drawImage(pixmap, 0, 0, this);
        }
      }
    }
    super.repaint();
  }
}