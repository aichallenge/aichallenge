/**
 * @constructor
 */
function ImageInfo(src) {
	this.src = src;
	this.success = undefined;
}

/**
 * @constructor
 */
function ImageManager(dataDir, vis, callback) {
	this.dataDir = dataDir;
	this.vis = vis;
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
 * Announces an image that must be loaded. Calling this method after
 * startRequests() results in unexpected behaviour.
 * 
 * @see #startRequests
 */
ImageManager.prototype.add = function(source) {
	this.info.push(new ImageInfo(this.dataDir + source));
	this.images.push(null);
	this.patterns.push(null);
};
/**
 * We clean up the state of all images that failed to download in hope that they
 * will succeed next time. This does not apply to the applet version which
 * handles these cases internally.
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
ImageManager.prototype.startRequests = function() {
	var img;
	this.error = '';
	for ( var i = 0; i < this.images.length; i++) {
		if (this.info[i].success === undefined && !this.images[i]) {
			img = new Image();
			this.images[i] = img;
			var that = this;
			img.onload = function() {
				that.imgHandler(this, true);
			};
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
 * Records the state of an image when the browser has finished loading it. If no
 * more images are pending, the visualizer is signaled.
 * 
 * @private
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
		this.callback.apply(this.vis, [ this.error ]);
	}
};
/**
 * Sets the pattern of an image to a CanvasPattern, wich can be used as
 * fillStyle in drawing operations to create a repeated tile texture.
 */
ImageManager.prototype.pattern = function(idx, ctx, repeat) {
	if (!this.patterns[idx]) {
		this.patterns[idx] = ctx.createPattern(this.images[idx], repeat);
	}
	ctx.fillStyle = this.patterns[idx];
};
/**
 * Sets the pattern of an image to a set of colorized copies of itself.
 */
ImageManager.prototype.colorize = function(idx, colors) {
	var canvas = document.createElement('canvas');
	var ctx = canvas.getContext('2d');
	this.patterns[idx] = canvas;
	canvas.width = this.images[idx].width * colors.length;
	canvas.height = this.images[idx].height;
	ctx.fillStyle = ctx.createPattern(this.images[idx], 'repeat');
	ctx.fillRect(0, 0, canvas.width, canvas.height);
	if (!this.restrictSecurity) {
		try {
			var data = ctx.getImageData(0, 0, canvas.width, canvas.height);
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
		var d = data.data;
		var ox = 0;
		var dx = 4 * this.images[idx].width;
		for ( var i = 0; i < colors.length; i++) {
			var c = colors[i];
			if (c) {
				if (typeof c == 'string') {
					c = [ 15 * parseInt(c.charAt(1), 16),
							15 * parseInt(c.charAt(2), 16),
							15 * parseInt(c.charAt(3), 16) ];
				}
				for ( var y = 0; y < 4 * data.width * data.height; y += 4 * data.width) {
					for ( var p = y + ox; p < y + ox + dx; p += 4) {
						if (d[p] === d[p + 1] && d[p] === d[p + 2]) {
							// only gray pixels
							d[p + 0] = (d[p + 0] * c[0]) >> 8;
							d[p + 1] = (d[p + 1] * c[1]) >> 8;
							d[p + 2] = (d[p + 2] * c[2]) >> 8;
						}
					}
				}
			}
			ox += dx;
		}
		ctx.putImageData(data, 0, 0);
	}
};

// make some exported functions known to Closure Compiler
ImageManager.prototype['imgHandler'] = ImageManager.prototype.imgHandler;
