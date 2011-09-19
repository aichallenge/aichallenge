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
	var i, ant, color;
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
	CanvasElement.call(this);
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
	CanvasElement.call(this);
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
	var start = -1;
	this.ctx.fillStyle = this.ptrn;
	this.ctx.fillRect(0, 0, this.w, this.h);
	if (this.fogMap) {
		cols = this.fogMap[0].length;
		colPixels = this.scale * cols;
		x = (this.w < colPixels) ? (this.w - colPixels >> 1) + this.shiftX : 0;
		rows = this.fogMap.length;
		rowPixels = this.scale * rows;
		y = (this.h < rowPixels) ? (this.h - rowPixels >> 1) + this.shiftY : 0;

		x_idx = Math.floor(-x / this.scale);
		y_idx = Math.floor(-y / this.scale);

		y_i = Math.wrapAround(y_idx, rows);
		for (y_f = y + y_idx * this.scale; y_f < this.h; y_f += this.scale) {
			fogRow = this.fogMap[y_i];
			x_i = Math.wrapAround(x_idx, cols);
			for (x_f = x + x_idx * this.scale; x_f < this.w; x_f += this.scale) {
				if (fogRow[x_i] === false) {
					if (start === -1) {
						start = x_f;
					}
				} else if (start !== -1) {
					this.ctx.clearRect(start, y_f, x_f - start, this.scale);
					start = -1;
				}
				x_i = (x_i + 1) % cols;
			}
			if (start !== -1) {
				this.ctx.clearRect(start, y_f, x_f - start, this.scale);
				start = -1;
			}
			y_i = (y_i + 1) % rows;
		}
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
	CanvasElement.call(this);
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
	var col, row, ar, sr, colPixels, rowPixels, drawList, i, k, ant, dr, dc;
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
				dr = Math.abs(row - ant.mapY);
				dc = Math.abs(col - ant.mapX);
				dr = Math.min(dr, rowPixels - dr);
				dc = Math.min(dc, colPixels - dc);
				if (ant['owner'] === undefined && (dr * dr + dc * dc <= sr)
						|| ant['owner'] !== undefined && (dr * dr + dc * dc <= ar)) {
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
	var halfScale, drawList, n, kf, w, dx, dy, d, fontSize, label, caption, order;
	var target, rows, cols, x1, y1, x2, y2, rowPixels, colPixels, ar, sr, r;
	var hash = undefined;

	// draw map
	this.ctx.drawImage(this.map.canvas, 0, 0);

	// draw ants sorted by color
	halfScale = 0.5 * this.scale;
	for (hash in this.drawStates) {
		this.ctx.fillStyle = hash;
		drawList = this.drawStates[hash];
		for (n = drawList.length - 1; n >= 0; n--) {
			kf = drawList[n];
			if (kf['owner'] === undefined) {
				w = halfScale * kf['size'];
				this.ctx.beginPath();
				this.ctx.arc(kf.mapX + halfScale, kf.mapY + halfScale, w, 0, 2 * Math.PI, false);
				this.ctx.fill();
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
				if (dx < 0) {
					this.ctx.fillRect(dx + this.w, dy, w, w);
					if (dy < 0) {
						this.ctx.fillRect(dx + this.w, dy + this.h, w, w);
					}
				}
				if (dy < 0) {
					this.ctx.fillRect(dx, dy + this.h, w, w);
				}
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
		dx = (this.fog.w < colPixels) ? 0.5 * (colPixels - this.fog.w) - this.fog.shiftX : 0;
		dy = (this.fog.h < rowPixels) ? 0.5 * (rowPixels - this.fog.h) - this.fog.shiftY : 0;
		this.drawWrapped(dx, dy, this.fog.w, this.fog.h, this.w, this.h, function(ctx, img, x, y) {
			ctx.drawImage(img, x, y);
		}, [ this.ctx, this.fog.canvas, dx, dy ]);
	}
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
	CanvasElement.call(this);
	this.state = state;
	this.antsMap = antsMap;
	this.dependsOn(antsMap);
	this.shiftX = 0;
	this.shiftY = 0;
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
	if (this.state.shiftX !== this.shiftX || this.state.shiftY !== this.shiftY) {
		this.invalid = true;
		this.shiftX = this.state.shiftX;
		this.shiftY = this.state.shiftY;
	}
};

/**
 * Draws the visible portion of the map with ants. If the map is smaller than the view area it is
 * repeated in a darker shade on both sides.
 */
CanvasElementShiftedMap.prototype.draw = function() {
	var x, y, dx, dy;
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
		this.ctx.lineWidth = 2;
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
	CanvasElement.call(this);
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
 * @param i
 *        {Number} the zero based player index
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
 * Helper function that returns a replay property with the given name, that should refer to a
 * statistics array. If the name is 'scores' the replay is also checked for the end game bonus.
 * 
 * @param name
 *        {String} the property name to be queried
 * @returns {Stats} the statistics set for the given item name.
 */
CanvasElementGraph.prototype.getStats = function(name) {
	var values = this.state.replay[name];
	var bonus = undefined;
	if (name === 'scores') bonus = this.state.replay.meta['replaydata']['bonus'];
	return new Stats(values, bonus);
};

/**
 * Causes a comparison of the relevant values that make up the visible content of this canvas
 * between the visualizer and cached values. If the cached values are out of date the canvas is
 * marked as invalid.
 * 
 * @returns {Boolean} true, if the internal state has changed
 */
CanvasElementGraph.prototype.checkState = function() {
	if (this.duration !== this.state.replay.duration) {
		this.invalid = true;
		this.duration = this.state.replay.duration;
	}
};

/**
 * Renders a timeline of the statistical values. The graphs are annotated by the player's state in
 * it's last turn.
 */
CanvasElementGraph.prototype.draw = function() {
	var min, max, i, k, t, scaleX, scaleY, txt, x, y, tw, tx;
	var w = this.w - 1;
	var h = this.h - 1;
	var replay = this.state.replay;
	var values = this.getStats(this.stats).values;
	this.ctx.fillStyle = '#fff';
	this.ctx.fillRect(0, 0, this.w, this.h);
	// find lowest and highest value
	min = 0;
	max = -Infinity;
	for (i = 0; i <= this.duration; i++) {
		for (k = 0; k < values[i].length; k++) {
			if (max < values[i][k]) {
				max = values[i][k];
			}
		}
	}
	// draw lines
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
	for (i = values[0].length - 1; i >= 0; i--) {
		this.ctx.strokeStyle = replay.htmlPlayerColors[i];
		this.ctx.beginPath();
		this.ctx.moveTo(0.5, 0.5 + scaleY * (max - values[0][i]));
		for (k = 1; k <= this.duration; k++) {
			this.ctx.lineTo(0.5 + scaleX * k, 0.5 + scaleY * (max - values[k][i]));
		}
		this.ctx.stroke();
	}
	if (!this.state.isStreaming && replay.meta['status']) {
		this.ctx.font = '10px Arial,Sans';
		for (i = values[0].length - 1; i >= 0; i--) {
			this.ctx.fillStyle = replay.htmlPlayerColors[i];
			this.ctx.strokeStyle = replay.htmlPlayerColors[i];
			k = replay.meta['replaydata']['scores'][i].length - 1;
			this.ctx.beginPath();
			x = 0.5 + k * scaleX;
			y = 0.5 + scaleY * (max - values[k][i]);
			this.ctx.moveTo(x, y);
			txt = this.statusToGlyph(i);
			tw = this.ctx.measureText(txt).width;
			tx = Math.min(x, w - tw);
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
 * @class A canvas element for statistics. It makes use of {@link CanvasElementGraph}.
 * @extends CanvasElement
 * @constructor
 * @param {State}
 *        state the visualizer state for reference
 * @param {String}
 *        caption the caption that is show to the left of the bar graph
 * @param {String}
 *        stats name of the stats to query from the visualizer
 */
function CanvasElementStats(state, caption, stats) {
	CanvasElement.call(this);
	this.state = state;
	this.caption = caption;
	this.graph = new CanvasElementGraph(state, stats);
	this.turn = 0;
	this.time = 0;
	this.label = false;
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
	this.graph.x = this.x + 4;
	this.graph.y = this.y + 32;
	this.graph.setSize(width - 8, Math.max(0, height - 36));
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
		this.ctx.fillStyle = '#fff';
		this.ctx.fillRect(0, 0, this.w, this.h);

		// outlines
		this.ctx.strokeStyle = '#444';
		this.ctx.lineWidth = 2;
		Shape.roundedRect(this.ctx, 0, 0, this.w, this.h, 1, 5);
		if (this.showGraph) {
			this.ctx.moveTo(0, 29);
			this.ctx.lineTo(this.w, 29);
		}
		this.ctx.stroke();

		// text
		this.ctx.font = FONT;
		this.ctx.textAlign = 'left';
		this.ctx.textBaseline = 'middle';
		this.ctx.fillStyle = '#888';
		this.ctx.fillText(this.caption, 4, 14);
	}

	// draw scores
	stats = this.graph.getStats(this.graph.stats);
	this.drawColorBar(95, 4, this.w - 99, 22, stats.values[this.turn],
			(this.turn === this.state.replay.duration) ? stats.bonus : undefined);

	// graph
	if (this.showGraph) {
		this.graph.validate();
		this.ctx.drawImage(this.graph.canvas, 4, 32);
		// time indicator
		x = 4.5 + (this.graph.w - 1) * this.time / this.graph.duration;
		this.ctx.lineWidth = 1;
		this.ctx.beginPath();
		this.ctx.moveTo(x, 32.5);
		this.ctx.lineTo(x, 32.5 + this.graph.h - 1);
		this.ctx.stroke();
		text = this.caption + ' | ';
		if (this.turn === this.graph.duration && !this.state.isStreaming) {
			text += 'end';
		} else {
			text += 'turn ' + (this.turn + 1) + '/' + this.graph.duration;
		}
		this.ctx.fillText(text, 4, 44);
	}
};

/**
 * Renders a horizontal bar graph of fixed size. Each block is colored using the respective player
 * color.
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
 * @param {Array}
 *        values the values to display
 * @param {Array}
 *        bonus the bonus to apply to 'values', if any; the text will display value and bonus
 *        separated by a '+'
 */
CanvasElementStats.prototype.drawColorBar = function(x, y, w, h, values, bonus) {
	var useValues, i, scale, offsetX, offsetY, amount, text, idx;
	var sum = 0;
	this.ctx.save();
	this.ctx.beginPath();
	this.ctx.rect(x, y, w, h);
	this.ctx.clip();
	for (i = 0; i < values.length; i++) {
		sum += bonus ? values[i] + bonus[i] : values[i];
	}
	useValues = new Array(values.length);
	if (sum == 0) {
		for (i = 0; i < values.length; i++) {
			useValues[i] = 1;
		}
		sum = values.length;
	} else {
		for (i = 0; i < values.length; i++) {
			useValues[i] = bonus ? values[i] + bonus[i] : values[i];
		}
	}
	scale = w / sum;
	offsetX = x;
	for (i = 0; i < useValues.length; i++) {
		idx = this.state.order[i];
		amount = scale * useValues[idx];
		this.ctx.fillStyle = this.state.replay.htmlPlayerColors[idx];
		this.ctx.fillRect(offsetX, y, w - offsetX + x, h);
		offsetX += amount;
	}
	this.ctx.textAlign = 'left';
	this.ctx.textBaseline = 'top';
	this.ctx.font = 'bold 16px Monospace';
	this.ctx.fillStyle = 'rgba(0,0,0,0.5)';
	offsetY = y + 3;
	offsetX = x + 2;
	for (i = 0; i < useValues.length; i++) {
		idx = this.state.order[i];
		text = Math.round(values[idx]);
		if (this.label) {
			text = String.fromCharCode(0x3b1 + i) + ' ' + text;
		}
		var bonusText = (bonus && bonus[idx]) ? '+' + Math.round(bonus[idx]) : '';
		var textWidth = this.ctx.measureText(text).width;
		if (bonusText) {
			this.ctx.font = 'bold italic 12px Monospace';
			var bonusTextWidth = this.ctx.measureText(bonusText).width;
			this.ctx.font = 'bold 16px Monospace';
		} else {
			bonusTextWidth = 0;
		}
		if (scale * useValues[idx] >= textWidth + bonusTextWidth) {
			this.ctx.fillText(text, offsetX, offsetY);
			if (bonusText) {
				this.ctx.font = 'bold italic 12px Monospace';
				this.ctx.fillStyle = 'rgba(0,0,0,0.8)';
				this.ctx.fillText(bonusText, offsetX + textWidth, offsetY);
				this.ctx.font = 'bold 16px Monospace';
				this.ctx.fillStyle = 'rgba(0,0,0,0.5)';
			}
		}
		offsetX += scale * useValues[idx];
	}
	this.ctx.restore();
};

/**
 * @class A helper class to transfer statistical values inside {@link CanvasElement} descendants.
 * @constructor
 * @param values
 *        {Number[][]} Statistical values for every player and turn.
 * @param bonus
 *        {Number[]} The bonus that will be added to each player's values at the end of the replay.
 *        Can be undefined and is used for the 'scores' statistical item.
 * @property values {Number[][]}
 * @property bonus {Number[]}
 */
function Stats(values, bonus) {
	this.values = values;
	this.bonus = bonus;
}
