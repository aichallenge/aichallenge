/**
 * width of left side panel
 * @const
 */
var LEFT_PANEL_W = 40;
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
	[ 240,   0,   0 ], [ 255,   0, 255 ], [   0, 210, 210 ], [ 118, 116,  53 ],
	[ 100, 220,   0 ], [ 211, 103,  18 ], [  23,  24, 137 ], [  49, 110, 130 ],
	[ 175, 139, 226 ], [ 117, 137, 121 ], [  51,  83,  33 ], [ 118,  26,  66 ],
	[  94,  76, 169 ], [ 112, 110, 119 ], [  46, 133,  36 ], [  30,  85, 213 ],
	[ 242, 147,   1 ], [ 145, 179, 156 ], [ 115,  83,  56 ], [ 165,  45, 151 ],
	[ 110, 124,  65 ], [ 208, 130, 107 ], [ 131, 145, 182 ], [  49, 160, 115 ],
	[ 156, 126, 144 ], [ 137,  62, 187 ]
];
/**
 * @const
 */
var COLOR_SAND = '#532';
/**
 * @const
 */
var ZOOM_SCALE = 20;
var INT_TO_HEX = new Array(256);
(function() {
	for (var i = 0; i < 16; i++) INT_TO_HEX[i] = '0' + i.toString(16);
	for (; i < 256; i++) INT_TO_HEX[i] = i.toString(16);
})();
/**
 * @const
 */
var FONT = 'bold 19px Arial,Sans';
