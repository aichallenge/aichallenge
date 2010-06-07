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
// Represents a single color box for a legend entry

import javax.swing.*;
import java.awt.*;
import java.awt.geom.RoundRectangle2D;

public class ColorBox implements Icon {
	private Color color;
	private final int SIZE = 12;
	private final int ROUND = 4;
	
	public ColorBox(Color _color) {
		color = _color;
	}
	
	public int getIconWidth() {
		return SIZE;
	}
	public int getIconHeight() {
		return SIZE;
	}
	public void paintIcon (Component c, Graphics g, int x, int y) {		
		Graphics2D g2 = (Graphics2D) g;
		
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
							RenderingHints.VALUE_ANTIALIAS_ON);
		g2.setRenderingHint(RenderingHints.KEY_RENDERING,
							RenderingHints.VALUE_RENDER_SPEED);
		
        RoundRectangle2D r = new RoundRectangle2D.Float(x, y,
														SIZE, SIZE,
														ROUND, ROUND);
		g2.setColor(color);
		g2.fill(r);
	}
}