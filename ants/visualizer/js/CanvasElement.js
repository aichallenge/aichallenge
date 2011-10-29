/**
 * @fileOverview This file contains the stack of off-screen images that are rendered on top and into
 *               each other to create the final display.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * @class A canvas that serves as an off-screen buffer for some graphics to be displayed possibly in
 *        tandem with other canvas elements or graphics.
 * @constructor
 */
function CanvasElement() {
	this.canvas = document.createElement('canvas');
	this.ctx = this.canvas.getContext('2d');
	this.invalid = true;
	this.resized = false;
	this.x = 0;
	this.y = 0;
	this.w = this.canvas.width;
	this.h = this.canvas.height;
	this.dependencies = [];
	this.invalidates = [];
}

/**
 * Sets the size of this canvas and invalidates it, if an actual change is detected.
 * 
 * @param {Number}
 *        width the new width
 * @param {Number}
 *        height the new height
 */
CanvasElement.prototype.setSize = function(width, height) {
	if (this.w !== width || this.h !== height) {
		this.w = width;
		this.h = height;
		if (width > 0 && height > 0) {
			this.canvas.width = width;
			this.canvas.height = height;
		}
		this.invalid = true;
		this.resized = true;
	}
};

/**
 * Checks if a coordinate pair is within the canvas area. The canvas' x and y properties are used as
 * it's offset.
 * 
 * @param {Number}
 *        x the x coordinate in question
 * @param {Number}
 *        y the y coordinate in question
 * @returns {Boolean} true, if the coordinates are contained within the canvas area
 */
CanvasElement.prototype.contains = function(x, y) {
	return (x >= this.x && x < this.x + this.w && y >= this.y && y < this.y + this.h);
};

/**
 * Ensures that the contents of the canvas are up to date. A redraw is triggered if necessary.
 * 
 * @returns {Boolean} true, if the canvas had to be redrawn
 */
CanvasElement.prototype.validate = function() {
	var i;
	for (i = 0; i < this.dependencies.length; i++) {
		if (this.dependencies[i].validate()) this.invalid = true;
	}
	this.checkState();
	if (this.invalid) {
		this.draw(this.resized);
		this.invalid = false;
		this.resized = false;
		return true;
	}
	return false;
};

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElement.prototype.checkState = function() {
// default implementation doesn't invalidate
};

/**
 * Makes another canvas a dependency of this one. This will cause this canvas to be invalidated if
 * the dependency becomes invalid and will cause this canvas to validate the dependency before
 * attempting to validate itself. Do not create cyclic dependencies!
 * 
 * @param {CanvasElement}
 *        element the dependency
 */
CanvasElement.prototype.dependsOn = function(element) {
	this.dependencies.push(element);
	element.invalidates.push(this);
};

/**
 * For cases where a drawn object would cross the border of the canvas and it is desirable to have
 * it wrap around and come in again on the other side, this method can be called with a given
 * function that contains the drawing commands. The wrapping will be simulated by repeatedly calling
 * the function and using matrix translations on the drawing context in between.
 * 
 * @param {Number}
 *        x the left coordinate
 * @param {Number}
 *        y the top coordinate
 * @param {Number}
 *        w the drawing width
 * @param {Number}
 *        h the drawing height
 * @param {Number}
 *        wField the width of the whole field on which wrapping should occur
 * @param {Number}
 *        hField the height of the field
 * @param {Function}
 *        func the drawing routine
 * @param {Array}
 *        args parameters for the drawing routine
 */
CanvasElement.prototype.drawWrapped = function(x, y, w, h, wField, hField, func, args) {
	var delta_x, delta_y, tx, ty, sum;
	if (x < 0 || y < 0 || x + w > wField || y + h > hField) {
		this.ctx.save();
		delta_x = -Math.floor((x + w) / wField) * wField;
		delta_y = -Math.floor((y + h) / hField) * hField;
		this.ctx.translate(delta_x, delta_y);
		for (ty = y + delta_y; ty < hField; ty += hField) {
			sum = 0;
			for (tx = x + delta_x; tx < wField; tx += wField) {
				func.apply(this, args);
				this.ctx.translate(wField, 0);
				sum -= wField;
			}
			this.ctx.translate(sum, hField);
		}
		this.ctx.restore();
	} else {
		func.apply(this, args);
	}
};

/**
 * @class Base class for maps
 * @extends CanvasElement
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 */
function CanvasElementAbstractMap(state) {
	this.upper();
	this.state = state;
	this.water = null;
}
CanvasElementAbstractMap.extend(CanvasElement);

/**
 * Draws a red marker on the map. Used when coordinates are given in the replay URL.
 * 
 * @param {Number}
 *        xs the x pixel position
 * @param {Number}
 *        ys the y pixel position
 */
CanvasElementAbstractMap.prototype.redFocusRectFun = function(xs, ys) {
	var x, y, w, i;
	for (i = 0; i < 5; i++) {
		this.ctx.strokeStyle = 'rgba(255,0,0,' + (i + 1) / 5 + ')';
		w = this.scale + 9 - 2 * i;
		x = xs + i;
		y = ys + i;
		this.ctx.strokeRect(x, y, w, w);
	}
};

/**
 * Draws the terrain map.
 */
CanvasElementAbstractMap.prototype.draw = function() {
	var row, col, start, isWall, xs, ys;
	var rows = this.state.replay.rows;
	var cols = this.state.replay.cols;
	var rowOpt = this.state.options['row'];
	var colOpt = this.state.options['col'];
	this.ctx.fillStyle = SAND_COLOR;
	this.ctx.fillRect(0, 0, this.w, this.h);
	this.ctx.fillStyle = this.ctx.createPattern(this.water, 'repeat');
	for (row = 0; row < rows; row++) {
		start = undefined;
		for (col = 0; col < cols; col++) {
			isWall = this.state.replay.walls[row][col];
			if (start === undefined && isWall) {
				start = col;
			} else if (start !== undefined && !isWall) {
				this.ctx.fillRect(this.scale * start, this.scale * row, this.scale * (col - start),
						this.scale);
				start = undefined;
			}
		}
		if (start !== undefined) {
			this.ctx.fillRect(this.scale * start, this.scale * row, this.scale * (col - start),
					this.scale);
		}
	}
	// marker
	if (!isNaN(rowOpt) && !isNaN(colOpt)) {
		xs = (colOpt % cols) * this.scale - 4.5;
		ys = (rowOpt % rows) * this.scale - 4.5;
		this.drawWrapped(xs, ys, this.scale + 9, this.scale + 9, this.w, this.h,
				this.redFocusRectFun, [ xs, ys ]);
	}
};

/**
 * @class A canvas element for the mini map.
 * @extends CanvasElementAbstractMap
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 */
function CanvasElementMiniMap(state) {
	this.upper(state);
	this.scale = 1;
	this.turn = undefined;
	this.ants = [];
}
CanvasElementMiniMap.extend(CanvasElementAbstractMap);

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElementMiniMap.prototype.checkState = function() {
	if ((this.state.time | 0) !== this.turn) {
		this.invalid = true;
		this.turn = (this.state.time | 0);
		this.ants = this.state.replay.getTurn(this.turn);
	}
};

/**
 * Invokes {@link CanvasElementAbstractMap#draw} to draw the map and then renders ants as pixels on
 * top of it.
 */
CanvasElementMiniMap.prototype.draw = function() {
	var i, ant, color, hills, hill, x, y;
	CanvasElementAbstractMap.prototype.draw.call(this);
	for (i = this.ants.length - 1; i >= 0; i--) {
		if ((ant = this.ants[i].interpolate(this.turn))) {
			color = '#';
			color += INT_TO_HEX[ant['r']];
			color += INT_TO_HEX[ant['g']];
			color += INT_TO_HEX[ant['b']];
			this.ctx.fillStyle = color;
			this.ctx.fillRect(ant['x'], ant['y'], 1, 1);
		}
	}
	hills = this.state.replay.meta['replaydata']['hills'];
	for (i = 0; i < hills.length; i++) {
		hill = hills[i];
		x = (hill[1] + 0.5);
		y = (hill[0] + 0.5);
		if (this.turn < hill[3]) {
			this.ctx.fillStyle = this.state.replay.htmlPlayerColors[hill[2]];
			this.ctx.beginPath();
			this.ctx.arc(x, y, 1.4, 0, 2 * Math.PI, false);
			this.ctx.fill();
		}
	}
};

/**
 * @class A canvas element for the main map.
 * @extends CanvasElementAbstractMap
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 */
function CanvasElementMap(state) {
	this.upper(state);
}
CanvasElementMap.extend(CanvasElementAbstractMap);

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElementMap.prototype.checkState = function() {
	if (this.scale !== this.state.scale) {
		this.invalid = true;
		this.scale = this.state.scale;
	}
};

/**
 * @class A tiny canvas to contain a cached fog pattern for the selected player.
 * @extends CanvasElement
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 */
function CanvasElementFogPattern(state) {
	this.upper();
	this.state = state;
	this.player = undefined;
	this.setSize(2, 2);
}
CanvasElementFogPattern.extend(CanvasElement);

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElementFogPattern.prototype.checkState = function() {
	if (this.player !== this.state.fogPlayer) {
		this.invalid = true;
		this.player = this.state.fogPlayer;
	}
};

/**
 * Draws the 2x2 pixel pattern.
 */
CanvasElementFogPattern.prototype.draw = function() {
	if (this.player !== undefined) {
		this.ctx.fillStyle = this.state.replay.htmlPlayerColors[this.player];
		this.ctx.fillRect(0, 0, 1, 1);
		this.ctx.fillRect(1, 1, 1, 1);
	}
};

/**
 * @class A canvas element for the fog overlay.
 * @extends CanvasElement
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 * @param {CanvasElementFogPattern}
 *        pattern the fog pattern to use
 */
function CanvasElementFog(state, pattern) {
	this.upper();
	this.state = state;
	this.turn = 0;
	this.shiftX = 0;
	this.shiftY = 0;
	this.scale = 1;
	this.fogMap = null;
	this.pattern = pattern;
	this.dependsOn(pattern);
	this.ptrn = null;
}
CanvasElementFog.extend(CanvasElement);

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElementFog.prototype.checkState = function() {
	if (this.player !== this.state.fogPlayer
			|| (this.player !== undefined && ((this.state.shiftX !== this.shiftX && this.w < this.scale
					* this.state.replay.cols)
					|| (this.state.shiftY !== this.shiftY && this.h < this.scale
							* this.state.replay.rows) || this.turn !== (this.state.time | 0) || this.scale !== this.state.scale))) {
		this.invalid = true;
		this.shiftX = this.state.shiftX;
		this.shiftY = this.state.shiftY;
		this.scale = this.state.scale;
		this.turn = this.state.time | 0;
		if (this.player !== this.state.fogPlayer) {
			this.player = this.state.fogPlayer;
			if (this.player !== undefined) {
				this.ptrn = this.ctx.createPattern(this.pattern.canvas, 'repeat');
				this.ctx.clearRect(0, 0, this.w, this.h);
			}
		}
		if (this.player === undefined) {
			this.fogMap = null;
		} else {
			this.fogMap = this.state.getFogMap();
		}
	}
};

/**
 * Draws the minimal fog image required to cover the currently visible area of the map.
 */
CanvasElementFog.prototype.draw = function() {
	var x, y, rowPixels, colPixels, x_idx, y_idx, rows, cols;
	var x_i, y_i, x_f, y_f, fogRow;
	var start = null;
	if (this.fogMap) {
		this.ctx.fillStyle = this.ptrn;
		this.ctx.fillRect(0, 0, this.w, this.h);
		cols = this.fogMap[0].length;
		colPixels = this.scale * cols;
		x = (this.w < colPixels) ? ((this.w - colPixels) >> 1) + this.shiftX : 0;
		rows = this.fogMap.length;
		rowPixels = this.scale * rows;
		y = (this.h < rowPixels) ? ((this.h - rowPixels) >> 1) + this.shiftY : 0;

		x_idx = Math.floor(-x / this.scale);
		y_idx = Math.floor(-y / this.scale);

		y_i = Math.wrapAround(y_idx, rows);
		for (y_f = y + y_idx * this.scale; y_f < this.h; y_f += this.scale) {
			fogRow = this.fogMap[y_i];
			x_i = Math.wrapAround(x_idx, cols);
			for (x_f = x + x_idx * this.scale; x_f < this.w; x_f += this.scale) {
				if (fogRow[x_i] === false) {
					if (start === null) {
						start = x_f;
					}
				} else if (start !== null) {
					this.ctx.clearRect(start, y_f, x_f - start, this.scale);
					start = null;
				}
				x_i = (x_i + 1) % cols;
			}
			if (start !== null) {
				this.ctx.clearRect(start, y_f, x_f - start, this.scale);
				start = null;
			}
			y_i = (y_i + 1) % rows;
		}
	} else {
		this.ctx.clearRect(0, 0, this.w, this.h);
	}
};

/**
 * @class The main map including ants and indicators
 * @extends CanvasElement
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 * @param {CanvasElementMap}
 *        map the background map
 * @param {CanvasElementFog}
 *        fog the fog overlay
 */
function CanvasElementAntsMap(state, map, fog) {
	this.upper();
	this.state = state;
	this.map = map;
	this.fog = fog;
	this.dependsOn(map);
	this.dependsOn(fog);
	this.time = 0;
	this.ants = [];
	this.drawStates = new Object();
	this.pairing = [];
	this.scale = 1;
	this.circledAnts = [];
	this.mouseOverVis = false;
	this.mouseCol = 0;
	this.mouseRow = 0;
	this.hillImage = null;
}
CanvasElementAntsMap.extend(CanvasElement);

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElementAntsMap.prototype.checkState = function() {
	var i, k, kf, p_i, p_k, dx, dy, rows, cols, ar, owner;
	var hash = undefined;
	var timeChanged = this.time !== this.state.time;
	if (timeChanged || this.scale !== this.state.scale || this.label !== this.state.config['label']) {
		this.invalid = true;
		this.time = this.state.time;
		this.scale = this.state.scale;
		this.label = this.state.config['label'];

		// per turn calculations
		if (this.turn !== (this.time | 0)) {
			cols = this.state.replay.cols;
			rows = this.state.replay.rows;
			this.turn = this.time | 0;
			this.ants = this.state.replay.getTurn(this.turn);
			this.pairing = new Array(this.ants.length);
			for (i = this.ants.length - 1; i >= 0; i--) {
				if ((kf = this.ants[i].interpolate(this.turn))) {
					owner = kf['owner'];
					kf = this.ants[i].interpolate(this.turn + 1);
					this.pairing[this.ants[i].id] = {
						kf : kf,
						owner : owner,
						x : Math.wrapAround(kf['x'], cols),
						y : Math.wrapAround(kf['y'], rows),
						targets : []
					};
				}
			}
			if ((ar = this.state.replay.meta['replaydata']['attackradius2'])) {
				for (i = this.ants.length - 1; i >= 0; i--) {
					if (this.ants[i].death === this.turn + 1) {
						p_i = this.pairing[this.ants[i].id];
						if (p_i !== undefined && p_i.owner !== undefined) {
							for (k = this.ants.length - 1; k >= 0; k--) {
								// this check looks odd, but accounts for
								// surviving ants
								if (this.ants[k].death !== this.turn + 1 || k < i) {
									p_k = this.pairing[this.ants[k].id];
									if (p_k !== undefined && p_k.owner !== undefined
											&& p_i.owner !== p_k.owner) {
										// distance between ants' end-points
										dx = Math.wrapAround(p_k.x - p_i.x, cols);
										if (2 * dx > cols) dx -= cols;
										dy = Math.wrapAround(p_k.y - p_i.y, rows);
										if (2 * dy > rows) dy -= rows;
										if (dx * dx + dy * dy <= ar) {
											// these two ants will be in attack
											// range
											p_i.targets.push(p_k.kf);
											p_k.targets.push(p_i.kf);
										}
									}
								}
							}
						}
					}
				}
			}
		}

		// interpolate ants for this point in time
		this.drawStates = new Object();
		for (i = this.ants.length - 1; i >= 0; i--) {
			if ((kf = this.ants[i].interpolate(this.time))) {
				hash = '#';
				hash += INT_TO_HEX[kf['r']];
				hash += INT_TO_HEX[kf['g']];
				hash += INT_TO_HEX[kf['b']];
				kf.calcMapCoords(this.scale, this.w, this.h);
				if (!this.drawStates[hash]) this.drawStates[hash] = [];
				this.drawStates[hash].push(kf);
			}
		}
	}

	// find ants in range of mouse cursor
	if (this.mouseOverVis !== this.state.mouseOverVis
			|| this.mouseOverVis
			&& (timeChanged || this.mouseCol !== this.state.mouseCol || this.mouseRow !== this.state.mouseRow)) {
		this.mouseOverVis = this.state.mouseOverVis;
		this.mouseCol = this.state.mouseCol;
		this.mouseRow = this.state.mouseRow;
		if (this.collectAntsAroundCursor()) this.invalid = true;
	}
};

/**
 * Builds the internal list of ants and food that need a circle drawn around them because the mouse
 * cursor is within their radius of effect (either attack or spawn).
 * 
 * @returns {Boolean} true, if the internal list has changed since the last call of this method
 */
CanvasElementAntsMap.prototype.collectAntsAroundCursor = function() {
	var col, row, ar, sr, colPixels, rowPixels, drawList, i, k, ant, d, owned;
	var found;
	var circledAnts = [];
	var hash = undefined;
	var same = true;
	if (this.mouseOverVis) {
		col = this.scale * this.mouseCol;
		row = this.scale * this.mouseRow;
		ar = this.state.replay.meta['replaydata']['attackradius2'];
		ar *= this.scale * this.scale;
		sr = this.state.replay.meta['replaydata']['spawnradius2'];
		sr *= this.scale * this.scale;
		colPixels = this.scale * this.state.replay.cols;
		rowPixels = this.scale * this.state.replay.rows;
		for (hash in this.drawStates) {
			drawList = this.drawStates[hash];
			for (i = drawList.length - 1; i >= 0; i--) {
				ant = drawList[i];
				d = Math.dist_2(col, row, ant.mapX, ant.mapY, colPixels, rowPixels);
				owned = ant['owner'] !== undefined;
				if (!owned && (d <= sr) || owned && (d <= ar)) {
					if (same) {
						found = false;
						for (k = 0; k < this.circledAnts.length; k++) {
							if (this.circledAnts[k] === ant) {
								found = true;
								break;
							}
						}
						same &= found;
					}
					circledAnts.push(ant);
				}
			}
		}
	}
	same &= circledAnts.length === this.circledAnts.length;
	if (same) return false;
	this.circledAnts = circledAnts;
	return true;
};

/**
 * Draws ants onto the map image. This includes overlay letters / ids, attack lines, effect circles
 * and finally the fog of war.
 */
CanvasElementAntsMap.prototype.draw = function() {
	var halfScale, drawList, n, kf, w, dx, dy, d, fontSize, label, caption, order, razed;
	var target, rows, cols, x1, y1, x2, y2, rowPixels, colPixels, ar, sr, r, hill, hills, i;
	var hash = undefined;

	// draw map
	this.ctx.drawImage(this.map.canvas, 0, 0);

	// hills
	halfScale = 0.5 * this.scale;
	hills = this.state.replay.meta['replaydata']['hills'];
	for (i = 0; i < hills.length; i++) {
		hill = hills[i];
		x1 = (hill[1] - 1) * this.scale;
		y1 = (hill[0] - 1) * this.scale;
		x2 = (hill[1] + 0.5) * this.scale;
		y2 = (hill[0] + 0.5) * this.scale;
		w = 3 * this.scale;
		dx = 60 * hill[2];
		if (this.turn >= hill[3]) {
			// dead
			this.drawWrapped(x1, y1, 3 * this.scale, 3 * this.scale, this.w, this.h, function() {
				this.ctx.drawImage(this.hillImage, dx, 60, 60, 60, x1, y1, w, w);
			}, []);
		} else {
			// or alive ...
			this.ctx.strokeStyle = this.state.replay.htmlPlayerColors[hill[2]];
			this.ctx.fillStyle = this.state.replay.htmlPlayerColors[hill[2]];
			// draw dashed circle around hill, if low resolution
			if (this.scale < 5) {
				this.drawWrapped(x2 - 3 * this.scale, y2 - 3 * this.scale, 6 * this.scale,
						6 * this.scale, this.w, this.h, function() {
							var m;
							var pieces = Math.max(4 * this.scale, 8);
							this.ctx.lineWidth = 1;
							this.ctx.globalAlpha = 0.5;
							for (m = 0; m < 2 * pieces; m += 2) {
								this.ctx.beginPath();
								this.ctx.arc(x2, y2, 3 * this.scale, (m - 0.3) * Math.PI / pieces,
										(m + 0.3) * Math.PI / pieces, false);
								this.ctx.stroke();
							}
						}, []);
			}
			razed = hill[3] <= this.state.replay.duration;
			if (razed && this.turn >= hill[3] - 30 || this.turn <= 10) {
				// draw proximity indicator just before the hill is captured
				r = this.scale * Math.max(3, hill[3] - this.time - 17);
				sr = this.scale * (3 + this.time);
				ar = 99;
				outer: for (hash in this.drawStates) {
					drawList = this.drawStates[hash];
					for (n = drawList.length - 1; n >= 0; n--) {
						kf = drawList[n];
						if (kf['owner'] !== undefined && kf['owner'] !== hill[2]) {
							d = Math.dist_2(this.scale * hill[1], this.scale * hill[0], kf.mapX,
									kf.mapY, this.w, this.h);
							if (!(ar * ar < d)) {
								ar = Math.sqrt(d);
								if (ar < 3 * this.scale) {
									ar = 3 * this.scale;
									break outer;
								}
							}
						}
					}
				}
				r = Math.min(Math.max(r, ar), sr) - this.scale;
				this.drawWrapped(x2 - r - halfScale, y2 - r - halfScale, 2 * r + this.scale, 2 * r
						+ this.scale, this.w, this.h, function() {
					var alpha = r / halfScale;
					if (alpha < 25 && this.state.replay.hasDuration) {
						this.ctx.lineWidth = 2 * halfScale;
						this.ctx.globalAlpha = Math.max(0, 1.0 - 0.04 * alpha);
						this.ctx.beginPath();
						this.ctx.arc(x2, y2, r, 0, 2 * Math.PI, false);
						this.ctx.stroke();
						this.ctx.globalAlpha = Math.max(0, 0.5 - 0.02 * alpha);
						this.ctx.beginPath();
						this.ctx.arc(x2, y2, r - this.scale, 0, 2 * Math.PI, false);
						this.ctx.stroke();
					}
				}, []);
			}
			this.ctx.globalAlpha = 1;
			this.drawWrapped(x1, y1, 3 * this.scale, 3 * this.scale, this.w, this.h, function() {
				this.ctx.drawImage(this.hillImage, dx, 0, 60, 60, x1, y1, w, w);
			}, []);
		}
	}

	// draw ants sorted by color
	for (hash in this.drawStates) {
		this.ctx.fillStyle = hash;
		drawList = this.drawStates[hash];
		for (n = drawList.length - 1; n >= 0; n--) {
			kf = drawList[n];
			if (kf['owner'] !== undefined) {
				this.drawWrapped(kf.mapX, kf.mapY, this.scale, this.scale, this.w, this.h,
						function(x, y, width) {
							this.ctx.beginPath();
							this.ctx.arc(x, y, width, 0, 2 * Math.PI, false);
							this.ctx.fill();
						}, [ kf.mapX + halfScale, kf.mapY + halfScale, halfScale * kf['size'] ]);
			} else {
				w = this.scale;
				dx = kf.mapX;
				dy = kf.mapY;
				if (kf['size'] !== 1) {
					d = 0.5 * (1.0 - kf['size']) * this.scale;
					dx += d;
					dy += d;
					w *= kf['size'];
				}
				this.ctx.fillRect(dx, dy, w, w);
			}
		}
	}

	// draw battle indicators
	rows = this.state.replay.rows;
	rowPixels = rows * this.scale;
	cols = this.state.replay.cols;
	colPixels = cols * this.scale;
	this.ctx.lineWidth = Math.pow(this.scale, 0.3);
	for (hash in this.drawStates) {
		drawList = this.drawStates[hash];
		this.ctx.strokeStyle = hash;
		this.ctx.beginPath();
		for (n = drawList.length - 1; n >= 0; n--) {
			kf = drawList[n];
			if (this.pairing[kf.antId] !== undefined) {
				for (d = this.pairing[kf.antId].targets.length - 1; d >= 0; d--) {
					target = this.pairing[kf.antId].targets[d];
					x1 = kf.mapX + halfScale;
					y1 = kf.mapY + halfScale;
					dx = Math.wrapAround(target.mapX - kf.mapX, colPixels);
					if (2 * dx > colPixels) dx -= colPixels;
					x2 = x1 + 0.5 * dx;
					dy = Math.wrapAround(target.mapY - kf.mapY, rowPixels);
					if (2 * dy > rowPixels) dy -= rowPixels;
					y2 = y1 + 0.5 * dy;
					this.drawWrapped(Math.min(x1, x2) - 1, Math.min(y1, y2) - 1,
							Math.abs(x2 - x1) + 2, Math.abs(y2 - y1) + 2, colPixels, rowPixels,
							function(fx1, fy1, fx2, fy2) {
								this.ctx.moveTo(fx1, fy1);
								this.ctx.lineTo(fx2, fy2);
							}, [ x1, y1, x2, y2 ]);
				}
			}
		}
		this.ctx.stroke();
	}

	// draw attack and spawn radiuses
	if (this.mouseOverVis) {
		ar = this.state.replay.meta['replaydata']['attackradius2'];
		ar = this.scale * Math.sqrt(ar);
		sr = this.state.replay.meta['replaydata']['spawnradius2'];
		sr = this.scale * Math.sqrt(sr);
		for (n = this.circledAnts.length - 1; n >= 0; --n) {
			kf = this.circledAnts[n];
			hash = '#';
			hash += INT_TO_HEX[kf['r']];
			hash += INT_TO_HEX[kf['g']];
			hash += INT_TO_HEX[kf['b']];
			this.ctx.strokeStyle = hash;
			this.ctx.beginPath();
			dx = kf.mapX + halfScale;
			dy = kf.mapY + halfScale;
			r = (kf['owner'] === undefined) ? sr : ar;
			x1 = dx - r;
			y1 = dy - r;
			this.drawWrapped(x1, y1, 2 * r, 2 * r, colPixels, rowPixels, function() {
				this.ctx.moveTo(dx + r, dy);
				this.ctx.arc(dx, dy, r, 0, 2 * Math.PI, false);
			});
			this.ctx.stroke();
		}
	}

	// draw A, B, C, D ... on ants or alternatively the global kf id
	label = this.state.config['label'];
	if (label) {
		fontSize = Math.ceil(Math.max(this.scale, 10) / label);
		this.ctx.save();
		this.ctx.translate(halfScale, halfScale);
		this.ctx.textBaseline = 'middle';
		this.ctx.textAlign = 'center';
		this.ctx.font = 'bold ' + fontSize + 'px Arial';
		this.ctx.fillStyle = '#000';
		this.ctx.strokeStyle = '#fff';
		this.ctx.lineWidth = 0.2 * fontSize;
		order = new Array(this.state.order.length);
		for (n = 0; n < order.length; n++) {
			order[this.state.order[n]] = n;
		}
		for (hash in this.drawStates) {
			drawList = this.drawStates[hash];
			for (n = drawList.length - 1; n >= 0; n--) {
				kf = drawList[n];
				if (label === 1) {
					if (kf['owner'] === undefined) continue;
					caption = String.fromCharCode(0x3b1 + order[kf['owner']]);
				} else {
					caption = kf.antId;
				}
				this.ctx.strokeText(caption, kf.mapX, kf.mapY);
				this.ctx.fillText(caption, kf.mapX, kf.mapY);
				if (kf.mapX < 0) {
					this.ctx.strokeText(caption, kf.mapX + this.map.w, kf.mapY);
					this.ctx.fillText(caption, kf.mapX + this.map.w, kf.mapY);
					if (kf.mapY < 0) {
						this.ctx.strokeText(caption, kf.mapX + this.map.w, kf.mapY + this.map.h);
						this.ctx.fillText(caption, kf.mapX + this.map.w, kf.mapY + this.map.h);
					}
				}
				if (kf.mapY < 0) {
					this.ctx.strokeText(caption, kf.mapX, kf.mapY + this.map.h);
					this.ctx.fillText(caption, kf.mapX, kf.mapY + this.map.h);
				}
			}
		}
		this.ctx.restore();
	}

	// fog
	if (this.state.fogPlayer !== undefined) {
		dx = (this.fog.w < colPixels) ? ((colPixels - this.fog.w + 1) >> 1) - this.fog.shiftX : 0;
		dy = (this.fog.h < rowPixels) ? ((rowPixels - this.fog.h + 1) >> 1) - this.fog.shiftY : 0;
		this.drawWrapped(dx, dy, this.fog.w, this.fog.h, this.w, this.h, function(ctx, img, x, y) {
			ctx.drawImage(img, x, y);
		}, [ this.ctx, this.fog.canvas, dx, dy ]);
	}
};

/**
 * Sets the ant hill image to use when drawing the map.
 * 
 * @param {HTMLCanvasElement}
 *        hillImage a colorized hill graphic.
 */
CanvasElementAntsMap.prototype.setHillImage = function(hillImage) {
	this.hillImage = hillImage;
};

/**
 * @class The main map with ants, dragged with the mouse and extended by borders if required
 * @extends CanvasElement
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 * @param {CanvasElementAntsMap}
 *        antsMap the prepared map with ants
 */
function CanvasElementShiftedMap(state, antsMap) {
	this.upper();
	this.state = state;
	this.antsMap = antsMap;
	this.dependsOn(antsMap);
	this.shiftX = 0;
	this.shiftY = 0;
	this.fade = undefined;
}
CanvasElementShiftedMap.extend(CanvasElement);

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElementShiftedMap.prototype.checkState = function() {
	if (this.state.shiftX !== this.shiftX || this.state.shiftY !== this.shiftY
			|| this.state.fade !== this.fade || this.state.time !== this.time) {
		this.invalid = true;
		this.shiftX = this.state.shiftX;
		this.shiftY = this.state.shiftY;
		this.fade = this.state.fade;
		this.time = this.state.time;
	}
};

/**
 * Draws the visible portion of the map with ants. If the map is smaller than the view area it is
 * repeated in a darker shade on both sides.
 */
CanvasElementShiftedMap.prototype.draw = function() {
	var x, y, dx, dy, cutoff;
	var mx = (this.w - this.antsMap.w) >> 1;
	var my = (this.h - this.antsMap.h) >> 1;
	// map backdrop
	dx = mx + this.shiftX;
	dy = my + this.shiftY;
	dx -= Math.ceil(dx / this.antsMap.w) * this.antsMap.w;
	dy -= Math.ceil(dy / this.antsMap.h) * this.antsMap.h;
	for (x = dx; x < this.w; x += this.antsMap.w) {
		for (y = dy; y < this.h; y += this.antsMap.h) {
			this.ctx.drawImage(this.antsMap.canvas, x, y);
		}
	}
	// map border if moved
	if (this.shiftX !== 0 || this.shiftY !== 0) {
		this.ctx.strokeStyle = '#000';
		this.ctx.lineWidth = 0.5;
		this.ctx.beginPath();
		for (x = dx; x <= this.w; x += this.antsMap.w) {
			this.ctx.moveTo(x, 0);
			this.ctx.lineTo(x, this.h);
		}
		for (y = dy; y <= this.h; y += this.antsMap.h) {
			this.ctx.moveTo(0, y);
			this.ctx.lineTo(this.w, y);
		}
		this.ctx.stroke();
	}
	// shaded static borders
	if (this.w > this.antsMap.w) {
		dx = mx + this.antsMap.w;
		this.ctx.fillStyle = 'rgba(0,0,0,0.3)';
		this.ctx.fillRect(0, 0, mx, this.h);
		this.ctx.fillRect(dx, 0, this.w - dx, this.h);
	}
	if (this.h > this.antsMap.h) {
		dy = my + this.antsMap.h;
		this.ctx.fillStyle = 'rgba(0,0,0,0.3)';
		this.ctx.fillRect(0, 0, this.w, my);
		this.ctx.fillRect(0, dy, this.w, this.h - dy);
	}
	// fade out
	if (this.fade) {
		this.ctx.fillStyle = this.fade;
		this.ctx.fillRect(0, 0, this.w, this.h);
	}
	// game cut-off reason
	cutoff = this.state.replay.meta['replaydata']['cutoff'];
	if (this.time > this.state.replay.duration - 1 && cutoff) {
		cutoff = '"' + cutoff + '"';
		this.ctx.font = FONT;
		dx = 0.5 * (this.w - this.ctx.measureText(cutoff).width);
		dy = this.h - 5;
		this.ctx.lineWidth = 4;
		this.ctx.strokeStyle = '#000';
		this.ctx.strokeText(cutoff, dx, dy);
		this.ctx.fillStyle = '#fff';
		this.ctx.fillText(cutoff, dx, dy);
	}
};

/**
 * @class A canvas element for statistical time graphs.
 * @extends CanvasElement
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 * @param {String}
 *        stats name of the stats to query from the visualizer
 */
function CanvasElementGraph(state, stats) {
	this.upper();
	this.state = state;
	this.stats = stats;
	this.duration = 0;
}
CanvasElementGraph.extend(CanvasElement);

/**
 * Tries to replace the given player's status at the end of the match with a Unicode glyph. This is
 * basically to reduce the noise caused by the longer textual descriptions.
 * 
 * @private
 * @param {Number}
 *        i the zero based player index
 * @returns Returns a well supported Unicode glyph for some known status, or the original status
 *          text otherwise.
 */
CanvasElementGraph.prototype.statusToGlyph = function(i) {
	var status_i = this.state.replay.meta['status'][i];
	if (status_i === 'survived') {
		return '\u2713';
	} else if (status_i === 'eliminated') {
		return '\u2717';
	}
	return status_i;
};

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElementGraph.prototype.checkState = function() {
	if (this.duration !== this.state.replay.duration && this.h > 0) {
		this.invalid = true;
		this.duration = this.state.replay.duration;
	}
};

/**
 * Renders a timeline of the statistical values. The graphs are annotated by the player's state in
 * it's last turn.
 */
CanvasElementGraph.prototype.draw = function() {
	var min, max, i, k, t, scaleX, scaleY, txt, x, y, tw, tx, hills, razed;
	var w = this.w - 1;
	var h = this.h - 1;
	var replay = this.state.replay;
	var values = this.getStats(this.stats).values;
	// Fixes the bug where the values would be scaled iteratively on every screen update in the live
	// visualizer
	var scaleFn = Math.sqrt;
	this.ctx.fillStyle = SAND_COLOR;
	this.ctx.fillRect(0, 0, this.w, this.h);
	this.ctx.font = '10px Arial,Sans';

	// find lowest and highest value
	min = 0;
	max = -Infinity;
	for (i = 0; i <= this.duration; i++) {
		for (k = 0; k < values[i].length; k++) {
			if (max < scaleFn(values[i][k])) {
				max = scaleFn(values[i][k]);
			}
		}
	}

	// draw ticks
	scaleX = (this.duration === 0) ? 0 : w / this.duration;
	this.ctx.strokeStyle = 'rgba(0,0,0,0.5)';
	this.ctx.beginPath();
	for (k = 1; k * scaleX < 2;) {
		k *= 10;
	}
	for (i = k - 1; i <= this.duration + 1; i += k) {
		t = ((i + 1) % (100 * k) ? (i + 1) % (10 * k) ? 3 : 7 : 11);
		this.ctx.moveTo(0.5 + scaleX * i, h - t);
		this.ctx.lineTo(0.5 + scaleX * i, h + 1);
	}
	this.ctx.moveTo(0.5 + 0, h + 0.5);
	this.ctx.lineTo(0.5 + scaleX * (this.duration + 1), h + 0.5);
	this.ctx.stroke();
	scaleY = h / (max - min);

	// hill razes
	this.ctx.textAlign = 'center';
	hills = this.state.replay.meta['replaydata']['hills'];
	for (k = 0; k < hills.length; k++) {
		razed = hills[k][3] < this.state.replay.duration;
		if (razed && values[hills[k][3]]) {
			x = 0.5 + scaleX * hills[k][3];
			y = 0.5 + scaleY * (max - scaleFn(values[hills[k][3]][hills[k][2]]));
			this.ctx.fillStyle = replay.htmlPlayerColors[hills[k][2]];
			this.ctx.beginPath();
			this.ctx.moveTo(x, y);
			this.ctx.lineTo(x - 4, y - 8);
			this.ctx.lineTo(x + 4, y - 8);
			this.ctx.lineTo(x, y);
			this.ctx.fill();
		}
	}

	// time line
	this.ctx.textAlign = 'left';
	for (i = values[0].length - 1; i >= 0; i--) {
		this.ctx.strokeStyle = replay.htmlPlayerColors[i];
		this.ctx.beginPath();
		this.ctx.moveTo(0.5, 0.5 + scaleY * (max - scaleFn(values[0][i])));
		for (k = 1; k <= this.duration; k++) {
			this.ctx.lineTo(0.5 + scaleX * k, 0.5 + scaleY * (max - scaleFn(values[k][i])));
		}
		this.ctx.stroke();
	}
	if (!this.state.isStreaming && replay.meta['status']) {
		for (i = values[0].length - 1; i >= 0; i--) {
			k = replay.meta['playerturns'][i];
			this.ctx.beginPath();
			x = 0.5 + k * scaleX;
			y = 0.5 + scaleY * (max - scaleFn(values[k][i]));
			this.ctx.moveTo(x, y);
			txt = this.statusToGlyph(i);
			tw = this.ctx.measureText(txt).width;
			tx = Math.min(x, w - tw);
			this.ctx.fillStyle = replay.htmlPlayerColors[i];
			this.ctx.strokeStyle = replay.htmlPlayerColors[i];
			if (y < 30) {
				y = ((y + 12) | 0) + 0.5;
				this.ctx.lineTo(x, y - 8);
				this.ctx.moveTo(tx, y - 8);
				this.ctx.lineTo(tx + tw, y - 8);
				this.ctx.fillText(txt, tx, y);
			} else {
				y = ((y - 7) | 0) + 0.5;
				this.ctx.lineTo(x, y + 2);
				this.ctx.moveTo(tx, y + 2);
				this.ctx.lineTo(tx + tw, y + 2);
				this.ctx.fillText(txt, tx, y);
			}
			this.ctx.stroke();
		}
	}
};

/**
 * Helper function that returns a replay property with the given name, that should refer to a
 * statistics array. If the name is 'scores' the replay is also checked for the end game bonus.
 * 
 * @param {String}
 *        name The property name to be queried.
 * @returns {Stats} the statistics set for the given item name.
 */
CanvasElementGraph.prototype.getStats = function(name) {
	var values = this.state.replay[name];
	var bonus;
	if (name === 'counts') {
		bonus = this.state.replay['scores'];
	} else {
		bonus = new Array(values.length);
		if (name === 'scores' && this.turn === this.state.replay.duration) {
			bonus[values.length - 1] = this.state.replay.meta['replaydata']['bonus'];
		}
	}
	return new Stats(values, bonus);
};

/**
 * @class A canvas element for statistics. It makes use of {@link CanvasElementGraph}.
 * @extends CanvasElement
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 * @param {String}
 *        caption the caption that is show to the left of the bar graph
 * @param {String}
 *        stats name of the stats to query from the visualizer
 * @param {String}
 *        bonusText Title over bonus section in the graph.
 */
function CanvasElementStats(state, caption, stats, bonusText) {
	this.upper();
	this.state = state;
	this.caption = caption;
	this.turn = 0;
	this.time = 0;
	this.label = false;
	this.bonusText = bonusText;
	this.graph = new CanvasElementGraph(state, stats);
	this.dependsOn(this.graph);
}
CanvasElementStats.extend(CanvasElement);

/**
 * Size without timeline.
 */
CanvasElementStats.MIN_HEIGHT = 30;
/**
 * Size with timeline.
 */
CanvasElementStats.MAX_HEIGHT = CanvasElementStats.MIN_HEIGHT + 70;

/**
 * Sets the size of this CanvasElementStats and the contained {@link CanvasElementGraph} and
 * invalidates both, if an actual change is detected.
 * 
 * @param {Number}
 *        width the new width
 * @param {Number}
 *        height the new height
 * @see CanvasElement#setSize
 */
CanvasElementStats.prototype.setSize = function(width, height) {
	CanvasElement.prototype.setSize.call(this, width, height);
	this.graph.x = this.x;
	this.graph.y = this.y + 32;
	this.graph.setSize(width - 4, Math.max(0, height - 32));
	this.showGraph = this.graph.h > 0;
};

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElementStats.prototype.checkState = function() {
	if ((this.showGraph && this.time !== this.state.time) || this.time !== (this.state.time | 0)
			|| this.label !== (this.state.config['label'] === 1)) {
		this.invalid = true;
		this.time = this.state.time;
		this.turn = this.time | 0;
		this.label = this.state.config['label'] === 1;
	}
};

/**
 * Daws a bar graph for the current turn and - if enabled - the contained time line.
 * 
 * @param resized
 *        {Boolean} Indicates weather the canvas has been resized and even static elements of the
 *        display have to be redrawn.
 */
CanvasElementStats.prototype.draw = function(resized) {
	var stats, text, x;
	if (resized) {
		this.ctx.fillStyle = BACK_COLOR;
		// this.ctx.fillRect(0, 0, this.w, this.h);

		// outlines
		this.ctx.strokeStyle = STAT_COLOR;
		this.ctx.lineWidth = 2;
		Shape.roundedRect(this.ctx, 0, 0, this.w, this.h, 1, 5);
		if (this.showGraph) {
			this.ctx.moveTo(0, 29);
			this.ctx.lineTo(this.w, 29);
		}
		this.ctx.fill();
		this.ctx.stroke();

		// text
		this.ctx.font = FONT;
		this.ctx.textAlign = 'left';
		this.ctx.textBaseline = 'middle';
		this.ctx.fillStyle = TEXT_COLOR;
		this.ctx.fillText(this.caption, 4, 14);
	}

	// draw scores
	stats = this.getStats(this.graph.stats, this.turn);
	this.drawColorBar(95, 2, this.w - 97, 26, stats, this.bonusText);

	// graph
	if (this.showGraph) {
		this.ctx.fillStyle = TEXT_GRAPH_COLOR;
		this.ctx.drawImage(this.graph.canvas, 2, 30);
		// time indicator
		x = 4.5 + (this.graph.w - 1) * this.time / this.graph.duration;
		this.ctx.lineWidth = 1;
		this.ctx.beginPath();
		this.ctx.moveTo(x, 32.5);
		this.ctx.lineTo(x, 32.5 + this.graph.h - 1);
		this.ctx.stroke();
		text = this.caption + ' | ';
		if (this.turn === this.graph.duration && !this.state.isStreaming) {
			text += 'end / ' + this.graph.duration;
		} else {
			text += 'turn ' + (this.turn + 1) + '/' + this.graph.duration;
		}
		this.ctx.fillText(text, 4, 44);
	}
};

/**
 * Helper function that returns a replay property with the given name, that should refer to a
 * statistics array. If the name is 'scores' the replay is also checked for the end game bonus.
 * 
 * @param {String}
 *        name The property name to be queried.
 * @param {Number}
 *        turn The turn for which to fetch the stats
 * @returns {Stats} the statistics set for the given item name.
 */
CanvasElementStats.prototype.getStats = function(name, turn) {
	var values = this.state.replay[name][turn];
	var bonus = undefined;
	if (name === 'scores' && this.turn === this.state.replay.duration) {
		bonus = this.state.replay.meta['replaydata']['bonus'];
	} else if (name === 'counts') {
		bonus = this.state.replay['stores'][turn];
	}
	return new Stats(values, bonus);
};

/**
 * Renders a horizontal 'stacked' bar graph.
 * 
 * @private
 * @param {Number}
 *        x the left coordinate
 * @param {Number}
 *        y the top coordinate
 * @param {Number}
 *        w the width
 * @param {Number}
 *        h the height
 * @param {Stats}
 *        stats The values and boni to display. The bonus field can be undefined or contain
 *        undefined values.
 * @param {String}
 *        bonusText Title over bonus section.
 */
CanvasElementStats.prototype.drawColorBar = function(x, y, w, h, stats, bonusText) {
	var i, idx, wUsable, xNegSep, text;
	var showBoni = false;
	var boni = new Array(stats.values.length);
	var boniList = new Array(stats.values.length);
	var negatives = new Array();
	var positives = new Array();
	var sumBoni = 0;
	var sumNegative = 0;
	var sumPositive = 0;
	var sumValues, sum;
	var xOffset = x;
	var drawPart = function(ctx, pixels, div, list, values, state, arrow, label) {
		var k, kIdx, wBarRaw, wBar, textWidth;
		ctx.save();
		for (k = 0; k < list.length; k++) {
			kIdx = state.order[list[k]];
			ctx.fillStyle = state.replay.htmlPlayerColors[kIdx];
			ctx.strokeStyle = STAT_COLOR;
			ctx.lineWidth = 0.5;
			if (div) {
				wBarRaw = Math.abs(values[kIdx]) * pixels / div;
			} else {
				wBarRaw = pixels / values.length;
			}
			if (wBarRaw !== 0) {
				// always draw a full width pixel to avoid aliasing
				wBar = Math.ceil(xOffset + wBarRaw) - xOffset;
				wBar = Math.min(x + w - xOffset, wBar);
				if (arrow) {
					ctx.beginPath();
					if (values[kIdx] >= 0) {
						ctx.moveTo(xOffset, y);
						ctx.lineTo(xOffset + wBar, y + h / 2);
						ctx.lineTo(xOffset, y + h);
						ctx.closePath();
					} else {
						ctx.moveTo(xOffset + wBar, y);
						ctx.lineTo(xOffset, y + h / 2);
						ctx.lineTo(xOffset + wBar, y + h);
						ctx.closePath();
					}
					ctx.fill();
					ctx.stroke();
				} else {
					ctx.fillRect(xOffset, y, wBar, h);
				}
				ctx.textBaseline = 'middle';
				ctx.font = 'bold 16px Monospace';
				ctx.fillStyle = TEXT_COLOR; // '#fff';
				ctx.lineWidth = 0.5;
				text = values[kIdx];
				if (label) {
					text = String.fromCharCode(0x3b1 + k) + ' ' + text;
					if (ctx.measureText(text).width + 4 > wBar) {
						text = String.fromCharCode(0x3b1 + k);
					}
				}
				textWidth = ctx.measureText(text).width + 4;
				if (textWidth <= wBar) {
					if (values[kIdx] >= 0) {
						ctx.textAlign = 'left';
						ctx.fillText(text, xOffset + 2, y + h / 2);
					} else {
						ctx.textAlign = 'right';
						ctx.fillText(text, xOffset + wBarRaw - 2, y + h / 2);
					}
				}
				xOffset += wBarRaw;
			}
		}
		ctx.restore();
	};
	this.ctx.save();
	this.ctx.fillStyle = BACK_COLOR;
	this.ctx.beginPath();
	this.ctx.rect(x, y, w, h);
	this.ctx.fill();
	// will we show a separate bonus section?
	for (i = 0; i < stats.values.length; i++) {
		if (stats.bonus !== undefined && stats.bonus[i]) {
			boni[i] = stats.bonus[i];
			sumBoni += boni[i];
			showBoni = true;
		} else {
			boni[i] = 0;
		}
		boniList[i] = i;
	}
	wUsable = showBoni ? w : w;
	// sum up absolutes of all values to determine width
	for (i = 0; i < stats.values.length; i++) {
		idx = this.state.order[i];
		if (stats.values[idx] >= 0) {
			positives.push(i);
			sumPositive += stats.values[idx];
		} else {
			negatives.push(i);
			sumNegative -= stats.values[idx];
		}
	}
	sumValues = sumNegative + sumPositive;
	sum = sumValues + sumBoni;
	// show negative scores
	if (negatives.length) {
		drawPart(this.ctx, wUsable, sum, negatives, stats.values, this.state, true, this.label);
	}
	xNegSep = (x + sumNegative * wUsable / sum) | 0;
	// show positive scores
	drawPart(this.ctx, wUsable, sum, positives, stats.values, this.state, false, this.label);
	this.ctx.lineWidth = 2;
	this.ctx.strokeStyle = STAT_COLOR;
	this.ctx.beginPath();
	if (showBoni) {
		xOffset = Math.ceil(xOffset) + 1;
		this.ctx.moveTo(xOffset, y);
		this.ctx.lineTo(xOffset, y + h);
	}
	if (negatives.length) {
		this.ctx.moveTo(xNegSep, y + 2);
		this.ctx.lineTo(xNegSep, y + h - 2);
	}
	this.ctx.stroke();
	this.ctx.fillStyle = TEXT_COLOR;
	this.ctx.strokeStyle = '#fff';
	this.ctx.font = 'bold 12px Monospace';
	this.ctx.textBaseline = 'top';
	// draw boni
	if (showBoni) {
		xOffset += 1;
		drawPart(this.ctx, wUsable, sum, boniList, boni, this.state, true, this.label);
		this.ctx.textAlign = 'right';
		this.ctx.strokeText(bonusText, x + w - 2, y);
		this.ctx.fillText(bonusText, x + w - 2, y);
	}
	this.ctx.restore();
};

/**
 * @class A helper class to transfer statistical values inside {@link CanvasElement} descendants.
 * @constructor
 * @param values
 *        {Array} Statistical values for every player and turn.
 * @param bonus
 *        {Array} The bonus that will be added to each player's values at the end of the replay. Can
 *        be undefined and is used for the 'scores' statistical item.
 * @property values {Array}
 * @property bonus {Array}
 */
function Stats(values, bonus) {
	this.values = values;
	this.bonus = bonus;
}
