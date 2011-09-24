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
 * A set of browser quirks, that can be queried to take alternate code paths.
 * 
 * @namespace
 */
Quirks = {
	/**
	 * True for browsers that correctly support the HTML canvas shadow effect.
	 */
	fullImageShadowSupport : !(window.navigator && window.navigator.userAgent
			.match(/\b(Firefox\/5\.|Chrome\/1[23]\.).*/))
};

/**
 * Names for common key-codes.
 * 
 * @namespace
 */
Key = {
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
Html = {
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
Shape = {
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
