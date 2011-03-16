package com.aicontest.visualizer;

import java.awt.image.WritableRaster;

public class ImageOps {

	public void colorize(CanvasRenderingContext2d ctx, int[][] colors) {
		try {
			WritableRaster raster = ctx.getCanvas().getPixmap().getRaster();
			int[] pixel = new int[4];
			for (int x = 0; x < raster.getWidth(); x++) {
				int[] color = colors[colors.length * x / raster.getWidth()];
				for (int y = 0; y < raster.getHeight(); y++) {
					raster.getPixel(x, y, pixel);
					if (pixel[0] == pixel[1] && pixel[1] == pixel[2]) {
						// a gray pixel
						pixel[0] = (pixel[0] + color[0]) >> 1;
						pixel[1] = (pixel[1] + color[1]) >> 1;
						pixel[2] = (pixel[2] + color[2]) >> 1;
					}
					raster.setPixel(x, y, pixel);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
