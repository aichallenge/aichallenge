package com.aicontest.visualizer.js.dom;

import java.awt.image.Raster;

public class ImageData
{
  public final int width;
  public final int height;
  public final int[] data;

  public ImageData(Raster raster)
  {
    width = raster.getWidth();
    height = raster.getHeight();
    data = raster.getPixels(0, 0, width, height, (int[])null);
  }
}