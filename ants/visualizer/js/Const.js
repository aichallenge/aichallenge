/**
 * @fileOverview Miscellaneous constant values.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

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
var RIGHT_PANEL_W = 48;
/**
 * height of bottom panel
 * 
 * @const
 */
var BOTTOM_PANEL_H = 64;
/**
 * color of food items
 * 
 * @const
 */
var FOOD_COLOR = [ 200, 150, 100 ];
/**
 * colors of players
 */
var PLAYER_COLORS = [ [ 100, 220, 0 ], [ 255, 0, 255 ], [ 0, 210, 210 ], [ 240, 0, 0 ],
		[ 242, 147, 1 ], [ 100, 62, 255 ], [ 200, 200, 200 ], [ 174, 165, 71 ], [ 117, 137, 121 ],
		[ 175, 139, 226 ], [ 30, 85, 213 ], [ 145, 179, 156 ], [ 115, 83, 56 ], [ 165, 45, 151 ],
		[ 110, 124, 65 ], [ 208, 130, 107 ], [ 49, 160, 115 ], [ 46, 133, 36 ], [ 49, 110, 130 ],
		[ 131, 145, 182 ], [ 211, 103, 18 ], [ 118, 116, 53 ], [ 156, 126, 144 ], [ 51, 83, 33 ],
		[ 118, 26, 66 ], [ 94, 76, 169 ] ];
/**
 * color of land squares
 * 
 * @const
 */
var SAND_COLOR = '#532';
/**
 * maximum pixel size of map squares
 * 
 * @const
 */
var ZOOM_SCALE = 20;
/**
 * A lookup table that converts byte values from 0-255 to their hexadecimal two letter
 * representation.
 */
var INT_TO_HEX = new Array(256);
(function() {
	for ( var i = 0; i < 16; i++)
		INT_TO_HEX[i] = '0' + i.toString(16);
	for (; i < 256; i++)
		INT_TO_HEX[i] = i.toString(16);
})();
/**
 * The standard font in the visualizer. Be careful here, because my implementation of font string
 * parsing in the Java wrapper is not very resilient to changes. Check back if the font displays ok
 * there.
 * 
 * @const
 */
var FONT = 'bold 19px Arial,Sans';
