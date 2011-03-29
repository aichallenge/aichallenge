package com.aicontest.visualizer.js.dom;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;

class CanvasPattern
{
  private RepeatMode repeatMode;
  private BufferedImage pattern;

  CanvasPattern(Image image, String repetition, ImageObserver obs)
  {
    int width = image.getWidth(obs);
    int height = image.getHeight(obs);
    pattern = new BufferedImage(width, height, 6);
    pattern.getGraphics().drawImage(image, 0, 0, obs);
    if (repetition.equals("repeat-no"))
      repeatMode = RepeatMode.REPEAT_NO;
    else if (repetition.equals("repeat-x"))
      repeatMode = RepeatMode.REPEAT_X;
    else if (repetition.equals("repeat-y"))
      repeatMode = RepeatMode.REPEAT_Y;
    else
      repeatMode = RepeatMode.REPEAT;
  }

  RepeatMode getRepeatMode()
  {
    return repeatMode;
  }

  BufferedImage getPattern() {
    return pattern;
  }

  private static enum RepeatMode
  {
    REPEAT, REPEAT_X, REPEAT_Y, REPEAT_NO;
  }
}