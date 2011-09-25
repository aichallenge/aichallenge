/**
 * @fileOverview Classes for loading the used images in the background.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * @class Stores information about an image source and the result of loading it.
 * @constructor
 * @param {String}
 *        src The image source.
 */
function ImageInfo(src) {
	this.src = src;
	this.success = undefined;
}

/**
 * @class This class keeps a list of images and loads them in the background. It also offers a
 *        pattern slot for every image that is setup by certain methods to contain special modified
 *        versions of that image.
 * @constructor
 * @param {String}
 *        dataDir The base directory string that will be prepended to all image load requests.
 * @param {Delegate}
 *        callback A delegate that will be invoked when the loading of images has completed.
 */
function ImageManager(dataDir, callback) {
	this.dataDir = dataDir;
	this.callback = callback;
	this.info = [];
	this.images = [];
	this.patterns = [];
	this.error = '';
	this.pending = 0;
	this.askSecurity = true;
	this.restrictSecurity = false;
}

/**
 * Announces an image that must be loaded. Calling this method after startRequests() results in
 * unexpected behavior.
 * 
 * @param {String}
 *        source The image name relative to the data directory.
 * @see #startRequests
 */
ImageManager.prototype.add = function(source) {
	this.info.push(new ImageInfo(this.dataDir + source));
	this.images.push(null);
	this.patterns.push(null);
};

/**
 * We clean up the state of all images that failed to download in hope that they will succeed next
 * time. This does not apply to the applet version which handles these cases internally.
 * 
 * @see Visualizer#cleanUp
 */
ImageManager.prototype.cleanUp = function() {
	for ( var i = 0; i < this.images.length; i++) {
		if (this.info[i].success === false) {
			this.info[i].success = undefined;
			this.images[i] = null;
			this.pending++;
		}
	}
	this.startRequests();
};

/**
 * Invoked once after all images have been added to start the download process.
 */
ImageManager.prototype.startRequests = function() {
	var img;
	this.error = '';
	for ( var i = 0; i < this.images.length; i++) {
		if (this.info[i].success === undefined && !this.images[i]) {
			img = new Image();
			this.images[i] = img;
			var that = this;
			/** @ignore */
			img.onload = function() {
				that.imgHandler(this, true);
			};
			/** @ignore */
			img.onerror = function() {
				that.imgHandler(this, false);
			};
			img.onabort = img.onerror;
			img.src = this.info[i].src;
			this.pending++;
		}
	}
};

/**
 * Records the state of an image when the browser has finished loading it. If no more images are
 * pending, the visualizer is signaled.
 * 
 * @private
 * @param {HTMLImageElement}
 *        img The image that finished loading.
 * @param {Boolean}
 *        success If false, an error message for this image will be added.
 */
ImageManager.prototype.imgHandler = function(img, success) {
	var i;
	for (i = 0; i < this.images.length; i++) {
		if (this.images[i].src === img.src) break;
	}
	if (!success) {
		if (this.error) this.error += '\n';
		this.error += this.info[i].src + ' did not load.';
	}
	this.info[i].success = success;
	if (--this.pending == 0) {
		this.callback.invoke([ this.error ]);
	}
};

/**
 * Generates a CanvasPattern for an image, which can be used as fillStyle in drawing operations to
 * create a repeated tile texture. The new pattern overrides the current pattern slot for the image
 * and activates the pattern for drawing.
 * 
 * @param {Number}
 *        idx The index of the image.
 * @param {CanvasRenderingContext2D}
 *        ctx The rendering context to create the pattern in.
 * @param {String}
 *        repeat the pattern repeat mode according to the HTML canvas createPattern() method.
 */
ImageManager.prototype.pattern = function(idx, ctx, repeat) {
	if (!this.patterns[idx]) {
		this.patterns[idx] = ctx.createPattern(this.images[idx], repeat);
	}
	ctx.fillStyle = this.patterns[idx];
};

/**
 * Sets the pattern of an image to a set of colorized copies of itself. Only gray pixels will be
 * touched. The new pattern overrides the current pattern slot for the image.
 * 
 * @param {Number}
 *        idx The index of the image.
 * @param {Array}
 *        colors An array of colors to use. Every array slot can be either an array of rgb values
 *        ([31, 124, 59]) or HTML color string ("#f90433").
 */
ImageManager.prototype.colorize = function(idx, colors) {
	var d, ox, dx, c, i, y, p, data, g;
	var canvas = document.createElement('canvas');
	var ctx = canvas.getContext('2d');
	this.patterns[idx] = canvas;
	canvas.width = this.images[idx].width * colors.length;
	canvas.height = this.images[idx].height;
	ctx.fillStyle = ctx.createPattern(this.images[idx], 'repeat');
	ctx.fillRect(0, 0, canvas.width, canvas.height);
	if (!this.restrictSecurity) {
		try {
			data = ctx.getImageData(0, 0, canvas.width, canvas.height);
		} catch (error1) {
			try {
				var privilegeManager = netscape.security.PrivilegeManager;
				if (this.askSecurity) {
					// alert('Accept the next dialog to have colorized button
					// graphics.');
					this.askSecurity = false;
				}
				privilegeManager.enablePrivilege("UniversalBrowserRead");
				data = ctx.getImageData(0, 0, canvas.width, canvas.height);
			} catch (error2) {
				this.restrictSecurity = true;
				return;
			}
		}
		d = data.data;
		ox = 0;
		dx = 4 * this.images[idx].width;
		for (i = 0; i < colors.length; i++) {
			c = colors[i];
			if (c) {
				if (typeof c == 'string') {
					c = [ 15 * parseInt(c.charAt(1), 16), 15 * parseInt(c.charAt(2), 16),
							15 * parseInt(c.charAt(3), 16) ];
				}
				for (y = 0; y < 4 * data.width * data.height; y += 4 * data.width) {
					for (p = y + ox; p < y + ox + dx; p += 4) {
						// only gray pixels
						g = d[p];
						if (g === d[p + 1] && g === d[p + 2]) {
							if (g < 128) {
								d[p + 0] = (d[p + 0] * c[0]) >> 7;
								d[p + 1] = (d[p + 1] * c[1]) >> 7;
								d[p + 2] = (d[p + 2] * c[2]) >> 7;
							} else {
								g = (g / 127.5) - 1;
								d[p + 0] = (1 - g) * c[0] + g * 255;
								d[p + 1] = (1 - g) * c[1] + g * 255;
								d[p + 2] = (1 - g) * c[2] + g * 255;
							}
						}
					}
				}
			}
			ox += dx;
		}
		ctx.putImageData(data, 0, 0);
	}
};
