/**
 * @fileOverview Miscellaneous constant values.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * A lookup table that converts byte values from 0-255 to their hexadecimal two letter
 * representation.
 */
INT_TO_HEX = new Array(256);
(function() {
	for ( var i = 0; i < 16; i++)
		INT_TO_HEX[i] = '0' + i.toString(16);
	for (; i < 256; i++)
		INT_TO_HEX[i] = i.toString(16);
}());

/**
 * width of left side panel
 * 
 * @const
 */
var LEFT_PANEL_W = 40;
/**
 * width of right side panel
 * 
 * @const
 */
RIGHT_PANEL_W = 48;
/**
 * height of bottom panel
 * 
 * @const
 */
BOTTOM_PANEL_H = 64;
/**
 * map different colors depending on player count
 *
 * @const
 */
COLOR_MAPS = [  10, // highlighted player
              [ 1 ],
              [ 1, 6 ],
              [ 1, 3, 6 ],
              [ 1, 3, 6, 8 ],
              [ 0, 2, 4, 6, 8 ],
              [ 0, 2, 3, 4, 6, 8 ],
              [ 0, 1, 3, 4, 5, 6, 8 ],
              [ 0, 1, 3, 4, 5, 6, 7, 8 ],
              [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ],
              [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ] ];
/**
 * colors of players
 * 
 * @const
 */
PLAYER_COLORS = [ [ 350, 85, 45 ], [ 20, 80, 55 ],
                  [ 45, 80, 50 ], [ 60, 90, 65 ],
	              [ 110, 60, 75 ], [ 155, 60, 45 ],
	              [ 210, 80, 55 ], [ 265, 80, 45 ],
		          [ 300, 60, 60 ], [ 345, 25, 75 ],

		          [ 0, 0, 90 ] ];

/**
 * color of food items
 * 
 * @const
 */
FOOD_COLOR = hsl_to_rgb([ 50, 20, 50 ]);
/**
 * color of land squares
 * 
 * @const
 */
SAND_COLOR = rgb_to_hex(hsl_to_rgb([ 30, 35, 35 ]));
/**
 * color of text in player stats
 * 
 * @const
 */
STAT_COLOR = rgb_to_hex(hsl_to_rgb([ 0, 0, 10 ]));
/**
 * color of text in stat titles
 * 
 * @const
 */
TEXT_COLOR = rgb_to_hex(hsl_to_rgb([ 0, 0, 10 ]));
/**
 * color of text on graph title
 * 
 * @const
 */
TEXT_GRAPH_COLOR = rgb_to_hex(hsl_to_rgb([ 0, 0, 90 ]));
/**
 * color of background for top stats bars
 * 
 * @const
 */
BACK_COLOR = rgb_to_hex(hsl_to_rgb([ 30, 30, 100 ]));
/**
 * maximum pixel size of map squares
 * 
 * @const
 */
ZOOM_SCALE = 20;

/**
 * The standard font in the visualizer. Be careful here, because my implementation of font string
 * parsing in the Java wrapper is not very resilient to changes. Check back if the font displays ok
 * there.
 * 
 * @const
 */
FONT = 'bold 19px Arial,Sans';
