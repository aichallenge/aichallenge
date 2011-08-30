/**
 * Random provides functions to generate random numbers in integer ranges.
 */
Random = {
	/**
	 * returns an integer in the range [0..range[
	 * 
	 * @param {number}
	 *        range the exclusive limit of the range
	 */
	range : function(range) {
		return Math.random() * range | 0;
	},
	/**
	 * returns an integer in the range [from..to]
	 * 
	 * @param {number}
	 *        from the low value of the range (inclusive)
	 * @param {number}
	 *        to the high value of the range (inclusive)
	 */
	fromTo : function(from, to) {
		return from + (Math.random() * (1 + to - from) | 0);
	}
};

Math.wrapAround = function(x, range) {
	return x - Math.floor(x / range) * range;
};

Quirks = {
	fullImageShadowSupport : !(window.navigator && window.navigator.userAgent
			.match(/\b(Firefox\/5\.|Chrome\/1[23]\.).*/))
};

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
	MINUS : 109
};

Html = {
	/**
	 * Creates a table HTML element.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the table
	 * @returns {String} the HTML table
	 */
	table : function(content) {
		return this.element('table', 'display:table;width:100%', content);
	},

	/**
	 * Creates a table row HTML element.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the table
	 * @returns {String} the HTML table row
	 */
	tr : function(content) {
		return this.element('tr', 'display:table-row', content);
	},

	/**
	 * Creates a table data HTML element.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the table
	 * @returns {String} the HTML table cell
	 */
	td : function(content) {
		return this
				.element('td',
						'display:table-cell;border:dotted 1px;padding:0px 2px',
						content);
	},

	/**
	 * Creates a HTML font element with underlined style.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the font element
	 * @returns {String} the HTML font element
	 */
	underline : function(content) {
		return this.element('p', 'text-decoration:underline', content);
	},

	/**
	 * Creates a HTML font element with bold style.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the font element
	 * @returns {String} the HTML font element
	 */
	bold : function(content) {
		return this.element('p', 'font-weight:bold', content);
	},

	/**
	 * Creates a HTML font element with italic style.
	 * 
	 * @param {Function}
	 *        content function that returns the content of the font element
	 * @returns {String} the HTML font element
	 */
	italic : function(content) {
		return this.element('p', 'display:inline;font-style:italic', content);
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
	 *        a function or any other data type that forms the elements inner
	 *        HTML
	 * @returns {String} the resulting element string
	 */
	element : function(element, style, content) {
		return '<' + element + ' style="' + style + '">'
				+ (typeof content == 'function' ? content() : content) + '</'
				+ element + '>';
	}

};

function shapeRoundedRect(ctx, x, y, w, h, margin, r) {
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

Function.prototype.extend = function(clazz) {
	var property = undefined;
	for (property in clazz.prototype) {
		this.prototype[property] = clazz.prototype[property];
	}
};