/**
 * width of left side panel
 * @const
 */
var LEFT_PANEL_W = 48;
/**
 * width of right side panel
 * @const
 */
var RIGHT_PANEL_W = 48;
/**
 * height of bottom panel
 * @const
 */
var BOTTOM_PANEL_H = 64;
/**
 * @const
 */
var FOOD_COLOR = [ 200, 150, 100 ];
/**
 * @const
 */
var PLAYER_COLORS = [
	[ 242,  63,  17 ], [  26, 108, 228 ], [   4, 203,   3 ], [ 249, 210,   0 ],
	[  71, 203, 238 ], [ 245,  41, 249 ], [ 211, 103,  18 ], [  23,  24, 137 ],
	[ 112, 110, 119 ], [ 242, 147,   1 ], [ 118,  26,  66 ], [  46, 133,  36 ],
	[ 145, 179, 156 ], [  49, 110, 130 ], [ 115,  83,  56 ], [ 165,  45, 151 ],
	[ 110, 124,  65 ], [  94,  76, 169 ], [ 175, 139, 226 ], [ 208, 130, 107 ],
	[ 131, 145, 182 ], [  51,  83,  33 ], [ 117, 137, 121 ], [  49, 160, 115 ],
	[ 156, 126, 144 ], [ 137,  62, 187 ]
];
/**
 * @const
 */
var COLOR_WATER = '#134';
/**
 * @const
 */
var COLOR_SAND = '#FFD';
/**
 * @const
 */
var ZOOM_SCALE = 20;
var INT_TO_HEX = new Array(256);
(function() {
	for (var i = 0; i < 16; i++) INT_TO_HEX[i] = '0' + i.toString(16);
	for (; i < 256; i++) INT_TO_HEX[i] = i.toString(16);
})();

