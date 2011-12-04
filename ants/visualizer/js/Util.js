/**
 * @fileOverview Utility functions used in the visualizer.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * On a infinite repeating map, this maps a coordinate x into the 'original' instance of the map
 * area. That is the area starting at (0;0) ranging to (width;height). This function operates on a
 * single coordinate so the range parameter can be either the width or height.
 * 
 * @param {Number}
 *        x The coordinate that should be remapped.
 * @param {Number}
 *        range The maximum for that coordinate.
 * @return {Number} x' such that, 0 &lt;= x &lt; range.
 */
Math.wrapAround = function(x, range) {
	return x - Math.floor(x / range) * range;
};

/**
 * A simple clamp function that makes sure, a value x doesn't violate a given range.
 * 
 * @param {Number}
 *        x The value that should be clamped to the range.
 * @param {Number}
 *        min The minimum value of a range.
 * @param {Number}
 *        max The maximum value of a range.
 * @return {Number} x, clamped to the range [min..max]
 */
Math.clamp = function(x, min, max) {
	return x < min ? min : x > max ? max : x;
};

/**
 * Calculate the (squared) distance between two points on a wrapped map.
 * 
 * @param {Number}
 *        x1
 * @param {Number}
 *        y1
 * @param {Number}
 *        x2
 * @param {Number}
 *        y2
 * @param {Number}
 *        w The map width.
 * @param {Number}
 *        h The map height.
 * @returns {Number} The squared minimum distance between the points.
 */
Math.dist_2 = function(x1, y1, x2, y2, w, h) {
	var dx = Math.abs(x1 - x2);
	var dy = Math.abs(y1 - y2);
	dx = Math.min(dx, w - dx);
	dy = Math.min(dy, h - dy);
	return dx * dx + dy * dy;
};

var BrowserDetect = {
	searchString: function (data) {
		for (var i=0;i<data.length;i++)	{
			var dataString = data[i].string;
			var dataProp = data[i].prop;
			this.versionSearchString = data[i].versionSearch || data[i].identity;
			if (dataString) {
				if (dataString.indexOf(data[i].subString) != -1)
					return data[i].identity;
			}
			else if (dataProp)
				return data[i].identity;
		}
	},
	searchVersion: function (dataString) {
		var index = dataString.indexOf(this.versionSearchString);
		if (index == -1) return;
		return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
	},
	dataBrowser: [
		{
			string: navigator.userAgent,
			subString: "Chrome",
			identity: "Chrome"
		},
		{ 	string: navigator.userAgent,
			subString: "OmniWeb",
			versionSearch: "OmniWeb/",
			identity: "OmniWeb"
		},
		{
			string: navigator.vendor,
			subString: "Apple",
			identity: "Safari",
			versionSearch: "Version"
		},
		{
			prop: window.opera,
			identity: "Opera",
			versionSearch: "Version"
		},
		{
			string: navigator.vendor,
			subString: "iCab",
			identity: "iCab"
		},
		{
			string: navigator.vendor,
			subString: "KDE",
			identity: "Konqueror"
		},
		{
			string: navigator.userAgent,
			subString: "Firefox",
			identity: "Firefox"
		},
		{
			string: navigator.vendor,
			subString: "Camino",
			identity: "Camino"
		},
		{		// for newer Netscapes (6+)
			string: navigator.userAgent,
			subString: "Netscape",
			identity: "Netscape"
		},
		{
			string: navigator.userAgent,
			subString: "MSIE",
			identity: "Explorer",
			versionSearch: "MSIE"
		},
		{
			string: navigator.userAgent,
			subString: "Gecko",
			identity: "Mozilla",
			versionSearch: "rv"
		}
	],
	dataOS : [
		{
			string: navigator.platform,
			subString: "Win",
			identity: "Windows"
		},
		{
			string: navigator.platform,
			subString: "Mac",
			identity: "Mac"
		},
		{
			   string: navigator.userAgent,
			   subString: "iPhone",
			   identity: "iPhone/iPod"
	    },
		{
			string: navigator.platform,
			subString: "Linux",
			identity: "Linux"
		}
	],
	filterNotAny : function(list) {
		var i;
		var result = false;
		for (i = 0; i < list.length; i++) {
			result = result || this.filter(list[i][0], list[i][1], list[i][2]);
			if (result) break;
		}
		return !result;
	},
	filter : function(browser, version, system) {
		return (!(this.browser != browser || this.version != version || this.system != system));
	},
	init : function() {
		this.browser = this.searchString(this.dataBrowser) || "unknown";
		this.version = this.searchVersion(navigator.userAgent) || this.searchVersion(navigator.appVersion) || "unknown";
		this.system  = this.searchString(this.dataOS) || "unknown";
	}
};
BrowserDetect.init();

/**
 * A set of browser quirks, that can be queried to take alternate code paths.
 * 
 * @namespace
 */
var Quirks = {
	/**
	 * True for browsers that correctly support the HTML canvas shadow effect.
	 */
	fullImageShadowSupport : BrowserDetect.filterNotAny([
		// shadow applies blur in shadow color to image
		['Firefox', 5], 
		// new rendering engine cuts off parts of images
		['Firefox', 7, 'Windows'],
		['Firefox', 8, 'Windows'],
		// no shadow blur support
		['Chrome', 12], ['Chrome', 13]
	])
};

/**
 * Names for common key-codes.
 * 
 * @namespace
 */
var Key = {
	LEFT : 37,
	RIGHT : 39,
	SPACE : 32,
	PGUP : 33,
	PGDOWN : 34,
	HOME : 36,
	END : 35,
	PLUS : 107,
	PLUS_OPERA : 61,
	PLUS_JAVA : 521,
	MINUS : 109,
	MINUS_JAVA : 45
};

/**
 * Functions that wrap content into HTML tags.
 * 
 * @namespace
 */
var Html = {
	/**
	 * Creates a table HTML element.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the table
	 * @returns {String} the HTML table
	 */
	table : function(content) {
		return Html.element('table', 'display:table;width:100%', content);
	},

	/**
	 * Creates a table row HTML element.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the table
	 * @returns {String} the HTML table row
	 */
	tr : function(content) {
		return Html.element('tr', 'display:table-row', content);
	},

	/**
	 * Creates a table data HTML element.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the table
	 * @returns {String} the HTML table cell
	 */
	td : function(content) {
		return Html.element('td', 'display:table-cell;border:dotted 1px;padding:0px 2px', content);
	},

	/**
	 * Creates a HTML font element with underlined style.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the font element
	 * @returns {String} the HTML font element
	 */
	underline : function(content) {
		return Html.element('p', 'text-decoration:underline', content);
	},

	/**
	 * Creates a HTML font element with bold style.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the font element
	 * @returns {String} the HTML font element
	 */
	bold : function(content) {
		return Html.element('p', 'font-weight:bold', content);
	},

	/**
	 * Creates a HTML font element with italic style.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the font element
	 * @returns {String} the HTML font element
	 */
	italic : function(content) {
		return Html.element('p', 'display:inline;font-style:italic', content);
	},

	/**
	 * Helper function to create a HTML element from generic info.
	 * 
	 * @private
	 * @param {String}
	 *        element the element name
	 * @param {String}
	 *        style some CSS to be added
	 * @param content
	 *        a function or any other data type that forms the elements inner HTML
	 * @returns {String} the resulting element string
	 */
	element : function(element, style, content) {
		return '<' + element + ' style="' + style + '">'
				+ (typeof content == 'function' ? content() : content) + '</' + element + '>';
	}

};

/**
 * General purpose drawing functions.
 * 
 * @namespace
 */
var Shape = {
	/**
	 * Draws the rounded rectangles used by buttons. Drawing attributes must be set before calling
	 * this function.
	 * 
	 * @param {RenderingContext2D}
	 *        ctx The rendering context to use.
	 * @param {Number}
	 *        x
	 * @param {Number}
	 *        y
	 * @param {Number}
	 *        w
	 * @param {Number}
	 *        h
	 * @param {Number}
	 *        margin How much to indent the center of the line.
	 * @param {Number}
	 *        r Radius of the rounded corners.
	 */
	roundedRect : function(ctx, x, y, w, h, margin, r) {
		var d = 0.5 * Math.PI;
		ctx.beginPath();
		ctx.moveTo(x + r + margin, y + margin);
		ctx.lineTo(x + w - r - margin, y + margin);
		ctx.arc(x + w - r - margin, y + r + margin, r, -d, 0, false);
		ctx.lineTo(x + w - margin, y + h - r - margin);
		ctx.arc(x + w - r - margin, y + h - r - margin, r, 0, d, false);
		ctx.lineTo(x + r + margin, y + h - margin);
		ctx.arc(x + r + margin, y + h - r - margin, r, d, 2 * d, false);
		ctx.lineTo(x + margin, y + r + margin);
		ctx.arc(x + r + margin, y + r + margin, r, 2 * d, 3 * d, false);
	}
};

/**
 * This function is called on functors to make their classes sub-classes of another class.
 * 
 * @param {Function}
 *        clazz The functor to inherit from.
 */
Function.prototype.extend = function(clazz) {
	var property = undefined;
	for (property in clazz.prototype) {
		this.prototype[property] = clazz.prototype[property];
	}
	// Simulates a super() call.
	var upper = function() {
		if (clazz.prototype.upper) this.upper = clazz.prototype.upper;
		clazz.apply(this, arguments);
		delete this.upper;
	};
	this.prototype.upper = upper;
};

/**
 * @class A simple class storing 2-dimensional size extents.
 * @constructor
 * @param {Number}
 *        w width
 * @param {Number}
 *        h height
 * @property {Number} w width
 * @property {Number} h height
 */
function Size(w, h) {
	this.w = w;
	this.h = h;
}

/**
 * @class A simple location class that stores information about a rectangle.
 * @constructor
 * @param {Number}
 *        x left coordinate
 * @param {Number}
 *        y top coordinate
 * @param {Number}
 *        w width
 * @param {Number}
 *        h height
 * @property {Number} x left coordinate
 * @property {Number} y top coordinate
 * @property {Number} w width
 * @property {Number} h height
 */
function Location(x, y, w, h) {
	this.x = x;
	this.y = y;
	this.w = w;
	this.h = h;
}

/**
 * Checks if a given coordinate pair is within the area described by this object.
 * 
 * @param {Number}
 *        x left coordinate
 * @param {Number}
 *        y top coordinate
 * @returns {Boolean} true, if the point is inside the rectangle
 */
Location.prototype.contains = function(x, y) {
	return (x >= this.x && x < this.x + this.w && y >= this.y && y < this.y + this.h);
};

/**
 * @class A delegate that calls a method on an object, with optional static parameters.
 * @constructor
 * @param {Object}
 *        obj The 'this' object.
 * @param {Function}
 *        func The member function.
 * @param {Array}
 *        args The arguments array. This parameter is optional here and it can be overridden in
 *        #invoke.
 */
function Delegate(obj, func, args) {
	this.obj = obj;
	this.func = func;
	this.args = args;
}

/**
 * Invokes the method stored in this Delegate.
 * 
 * @param {Array}
 *        args Optional argument list that overrides any arguments passed in the constructor.
 */
Delegate.prototype.invoke = function(args) {
	if (args) {
		this.func.apply(this.obj, args);
	} else {
		this.func.apply(this.obj, this.args);
	}
};

/**
 * Converts a hsl color value to rgb.
 *
 * @param {Color} a list of 3 values representing hue, saturation and luminosity
 * @returns {Color} a list of 3 values representing red, green and blue
 */
function hsl_to_rgb (C) {
    var h = C[0];
    var s = C[1]/100;
    var l = C[2]/100;
    var c = (1 - Math.abs(2 * l - 1)) * s;
    var h2 = h / 60;
    var x = c * (1 - Math.abs(h2 % 2 - 1));
    var r, g, b;
    switch (Math.floor(h2)) {
    case 0: r = c; g = x; b = 0; break;
    case 1: r = x; g = c; b = 0; break;
    case 2: r = 0; g = c; b = x; break;
    case 3: r = 0; g = x; b = c; break;
    case 4: r = x; g = 0; b = c; break;
    case 5: r = c; g = 0; b = x; break;
    default: r = 0; g = 0; b = 0; break;
    }
    var m = l - 0.5 * c;
    return [ Math.floor(0.999 + 255 * (r + m)),
             Math.floor(0.999 + 255 * (g + m)),
             Math.floor(0.999 + 255 * (b + m)) ]    
};

/**
 * Converts a rgb color value to hexidecimal.
 *
 * @param {Color} a list of 3 values representing red, green and blue
 * @returns {Color} the hexidecimal representation of a color
 */
function rgb_to_hex (C) {
    return '#' + INT_TO_HEX[C[0]] + INT_TO_HEX[C[1]] + INT_TO_HEX[C[2]];
};
