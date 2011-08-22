/**
 * @fileOverview This file contains the stack of off-screen images that are
 *               rendered on top and into each other to create the final
 *               display.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * @class A canvas that serves as an off-screen buffer for some graphics to be
 *        displayed possibly in tandem with other canvas elements or graphics.
 * @constructor
 * @returns {CanvasElement}
 */
function CanvasElement() {
	this.canvas = document.createElement('canvas');
	this.ctx = this.canvas.getContext('2d');
	this.ctx.globalCompositeOperation = 'copy';
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
 * Sets the size of this canvas and invalidates it, if an actual change is
 * detected.
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
 * Checks if a coordinate pair is within the canvas area. The canvas' x and y
 * properties are used as it's offset.
 * 
 * @param {Number}
 *        x the x coordinate in question
 * @param {Number}
 *        y the y coordinate in question
 * @returns {Boolean} true, if the coordinates are contained within the canvas
 *          area
 */
CanvasElement.prototype.contains = function(x, y) {
	return (x >= this.x && x < this.x + this.w && y >= this.y && y < this.y
			+ this.h);
};

/**
 * Ensures that the contents of the canvas are up to date. A redraw is triggered
 * if necessary.
 * 
 * @returns {Boolean} true, if the canvas had to be redrawn
 */
CanvasElement.prototype.validate = function() {
	var i;
	for (i = 0; i < this.dependencies.length; i++) {
		this.invalid |= this.dependencies[i].validate();
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
 * Causes a comparison of the relevant values that make up the visible content
 * of this canvas between the visualizer and cached values. If the cached values
 * are out of date the canvas is marked as invalid.
 */
CanvasElement.prototype.checkState = function() {
	// default implementation doesn't invalidate
};

/**
 * Makes another canvas a dependency of this one. This will cause this canvas to
 * be invalidated if the dependency becomes invalid and will cause this canvas
 * to validate the dependency before attempting to validate itself. Do not
 * create cyclic dependencies.
 * 
 * @param {CanvasElement}
 *        element the dependency
 */
CanvasElement.prototype.dependsOn = function(element) {
	this.dependencies.push(element);
	element.invalidates.push(this);
};

/**
 * For cases where a drawn object would cross the border of the canvas and it is
 * desirable to have it wrap around and come in again on the other side, this
 * method can be called with a given function that contains the drawing
 * commands. The wrapping will be simulated by repeatedly calling the function
 * and using matrix translations on the drawing context in between.
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
CanvasElement.prototype.drawWrapped = function(x, y, w, h, wField, hField,
		func, args) {
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
 * @returns {CanvasElementAbstractMap}
 */
function CanvasElementAbstractMap() {
	CanvasElement.call(this);
}
CanvasElementAbstractMap.extend(CanvasElement);

/**
 * Draws a red marker on the map. Used when coordinates are given in the replay
 * URL.
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

CanvasElementAbstractMap.prototype.draw = function() {
	var row, col, start, isWall, xs, ys;
	var rows = this.vis.getRows();
	var cols = this.vis.getCols();
	var rowOpt = this.vis.getOption('row');
	var colOpt = this.vis.getOption('col');
	this.ctx.fillStyle = SAND_COLOR;
	this.ctx.fillRect(0, 0, this.w, this.h);
	this.ctx.fillStyle = this.ctx.createPattern(this.vis.getImage(0), 'repeat');
	for (row = 0; row < rows; row++) {
		start = undefined;
		for (col = 0; col < cols; col++) {
			isWall = this.vis.isWall(row, col);
			if (start === undefined && isWall) {
				start = col;
			} else if (start !== undefined && !isWall) {
				this.ctx.fillRect(this.scale * start, this.scale * row,
						this.scale * (col - start), this.scale);
				start = undefined;
			}
		}
		if (start !== undefined) {
			this.ctx.fillRect(this.scale * start, this.scale * row, this.scale
					* (col - start), this.scale);
		}
	}
	if (rowOpt !== undefined && colOpt !== undefined) {
		xs = (colOpt % cols) * this.scale - 4.5;
		ys = (rowOpt % rows) * this.scale - 4.5;
		this.drawWrapped(xs, ys, this.scale + 9, this.scale + 9, this.w,
				this.h, this.redFocusRectFun, [ this.ctx, this.scale, xs, ys ]);
	}
};

/**
 * @class A canvas element for the minimap.
 * @constructor
 * @param {Visualizer}
 *        vis the visualizer for reference
 * @returns {CanvasElementMiniMap}
 */
function CanvasElementMiniMap(vis) {
	CanvasElementAbstractMap.call(this);
	this.vis = vis;
	this.scale = 1;
	this.turn = undefined;
	this.ants = [];
}
CanvasElementMiniMap.extend(CanvasElementAbstractMap);
CanvasElementMiniMap.prototype.checkState = function() {
	if ((this.vis.getTime() | 0) !== this.turn) {
		this.invalid = true;
		this.turn = (this.vis.getTime() | 0);
		this.ants = this.vis.getTurn(this.turn);
	}
};
CanvasElementMiniMap.prototype.draw = function() {
	var i, ant, color;
	CanvasElementAbstractMap.prototype.draw.call(this);
	for (i = this.ants.length - 1; i >= 0; i--) {
		if ((ant = this.ants[i].interpolate(this.turn, Quality.LOW))) {
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
 * @constructor
 * @param {Visualizer}
 *        vis the visualizer for reference
 * @returns {CanvasElementMap}
 */
function CanvasElementMap(vis) {
	CanvasElementAbstractMap.call(this);
	this.vis = vis;
}

CanvasElementMap.extend(CanvasElementAbstractMap);

CanvasElementMap.prototype.draw = function() {
	this.scale = this.vis.getScale();
	CanvasElementAbstractMap.prototype.draw.call(this);
};

/**
 * @class A canvas element for the fog overlay.
 * @constructor
 * @param {Visualizer}
 *        vis the visualizer for reference
 * @param {CanvasElementFogPattern}
 *        pattern the fog pattern to use
 * @returns {CanvasElementFog}
 */
function CanvasElementFog(vis, pattern) {
	CanvasElement.call(this);
	this.vis = vis;
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

CanvasElementFog.prototype.checkState = function() {
	if (this.player !== this.vis.getFogPlayer()
			|| (this.player !== undefined && ((this.vis.getShiftX() !== this.shiftX && this.w < this.scale
					* this.vis.getCols())
					|| (this.vis.getShiftY() !== this.shiftY && this.h < this.scale
							* this.vis.getRows())
					|| this.turn !== (this.vis.getTime() | 0) || this.scale !== this.vis
					.getScale()))) {
		this.invalid = true;
		this.shiftX = this.vis.getShiftX();
		this.shiftY = this.vis.getShiftY();
		this.scale = this.vis.getScale();
		this.turn = this.vis.getTime() | 0;
		if (this.player !== this.vis.getFogPlayer()) {
			this.player = this.vis.getFogPlayer();
			if (this.player !== undefined) {
				this.ptrn = this.ctx.createPattern(this.pattern.canvas,
						'repeat');
			}
		}
		if (this.player === undefined) {
			this.fogMap = null;
		} else {
			this.fogMap = this.vis.getFogMap();
		}
	}
};

CanvasElementFog.prototype.draw = function() {
	var x, y, rowPixels, colPixels, x_idx, y_idx, rows, cols, x_i, y_i, x_f, y_f;
	var fogRow;
	this.ctx.clearRect(0, 0, this.w, this.h);
	if (this.fogMap) {
		cols = this.fogMap[0].length;
		colPixels = this.scale * cols;
		x = (this.w < colPixels) ? 0.5 * (this.w - colPixels) + this.shiftX : 0;
		rows = this.fogMap.length;
		rowPixels = this.scale * rows;
		y = (this.h < rowPixels) ? 0.5 * (this.h - rowPixels) + this.shiftY : 0;

		x_idx = Math.floor(-x / this.scale);
		y_idx = Math.floor(-y / this.scale);

		this.ctx.fillStyle = this.ptrn;
		y_i = Math.wrapAround(y_idx, rows);
		y_f = y + y_idx * this.scale;
		while (y_f < this.h) {
			fogRow = this.fogMap[y_i];
			x_i = Math.wrapAround(x_idx, rows);
			x_f = x + x_idx * this.scale;
			while (x_f < this.w) {
				if (fogRow[x_i]) {
					this.ctx.fillRect(x_f, y_f, this.scale, this.scale);
				}
				x_i = (x_i + 1) % cols;
				x_f += this.scale;
			}
			y_i = (y_i + 1) % rows;
			y_f += this.scale;
		}
	}
};

/**
 * @class The main map including ants and indicators
 * @constructor
 * @param {Visualizer}
 *        vis the visualizer for reference
 * @param {CanvasElementMap}
 *        map the background map
 * @param {CanvasElementFog}
 *        fog the fog overlay
 * @returns {CanvasElementMapWithAnts}
 */
function CanvasElementMapWithAnts(vis, map, fog) {
	CanvasElement.call(this);
	this.vis = vis;
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

CanvasElementMapWithAnts.extend(CanvasElement);

CanvasElementMapWithAnts.prototype.checkState = function() {
	var i, k, ant, p_i, p_k, dx, dy, rows, cols, ar, owner;
	var hash = undefined;
	var timeChanged = this.time !== this.vis.getTime();
	if (timeChanged || this.scale !== this.vis.getScale()
			|| this.label !== this.vis.getConfig('label')) {
		this.invalid = true;
		this.time = this.vis.getTime();
		this.scale = this.vis.getScale();
		this.label = this.vis.getConfig('label');

		// per turn calculations
		if (this.turn !== (this.time | 0)) {
			this.turn = this.time | 0;
			this.ants = this.vis.getTurn(this.turn);
			this.pairing = new Array(this.ants.length);
			for (i = this.ants.length - 1; i >= 0; i--) {
				if ((ant = this.ants[i].interpolate(this.turn, Quality.LOW))) {
					owner = ant['owner'];
					ant = this.ants[i].interpolate(this.turn + 1, Quality.LOW);
					this.pairing[this.ants[i].id] = {
						ant : ant,
						owner : owner,
						x : ant['x'],
						y : ant['y'],
						targets : []
					};
				}
			}
			cols = this.vis.getCols();
			rows = this.vis.getRows();
			if ((ar = this.vis.getAttackRadius2())) {
				for (i = this.ants.length - 1; i >= 0; i--) {
					if (this.ants[i].death === this.turn + 1) {
						p_i = this.pairing[this.ants[i].id];
						if (p_i !== undefined && p_i.owner !== undefined) {
							for (k = this.ants.length - 1; k >= 0; k--) {
								if (this.ants[k].death !== this.turn + 1
										|| k < i) {
									p_k = this.pairing[this.ants[k].id];
									if (p_k !== undefined
											&& p_k.owner !== undefined
											&& p_i.owner !== p_k.owner) {
										// distance between ants' end-points
										dx = Math.wrapAround(p_k.x - p_i.x,
												cols);
										if (2 * dx > cols) dx -= cols;
										dy = Math.wrapAround(p_k.y - p_i.y,
												rows);
										if (2 * dy > rows) dy -= rows;
										if (dx * dx + dy * dy <= ar) {
											// these two ants will be in attack
											// range
											p_i.targets.push(p_k.ant);
											p_k.targets.push(p_i.ant);
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
			if ((ant = this.ants[i].interpolate(this.time, Quality.LOW))) {
				hash = '#';
				hash += INT_TO_HEX[ant['r']];
				hash += INT_TO_HEX[ant['g']];
				hash += INT_TO_HEX[ant['b']];
				ant['x'] = Math.round(this.scale * ant['x']) + this.scale - 1;
				ant['y'] = Math.round(this.scale * ant['y']) + this.scale - 1;
				// correct coordinates
				ant['x'] = Math.wrapAround(ant['x'], this.w) - this.scale + 1;
				ant['y'] = Math.wrapAround(ant['y'], this.h) - this.scale + 1;
				if (!this.drawStates[hash]) this.drawStates[hash] = [];
				this.drawStates[hash].push(ant);
			}
		}
	}

	// find ants in range of mouse cursor
	if (this.mouseOverVis !== this.vis.getMouseOverVis()
			|| this.mouseOverVis
			&& (timeChanged || this.mouseCol !== this.vis.getMouseCol() || this.mouseRow !== this.vis
					.getMouseRow())) {
		this.mouseOverVis = this.vis.getMouseOverVis();
		this.mouseCol = this.vis.getMouseCol();
		this.mouseRow = this.vis.getMouseRow();
		this.invalid |= this.collectAntsAroundCursor();
	}
};

CanvasElementMapWithAnts.prototype.collectAntsAroundCursor = function() {
	var col, row, ar, sr, colPixels, rowPixels, drawList, i, k, ant, dr, dc;
	var found;
	var circledAnts = [];
	var hash = undefined
	var same = true;
	col = this.scale * this.mouseCol;
	row = this.scale * this.mouseRow;
	ar = this.scale * this.scale * this.vis.getAttackRadius2();
	sr = this.scale * this.scale * this.vis.getSpawnRadius2();
	colPixels = this.scale * this.vis.getCols();
	rowPixels = this.scale * this.vis.getRows();
	for (hash in this.drawStates) {
		drawList = this.drawStates[hash];
		for (i = drawList.length - 1; i >= 0; i--) {
			ant = drawList[i];
			dr = Math.abs(row - ant['y']);
			dc = Math.abs(col - ant['x']);
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
	same &= circledAnts.length === this.circledAnts.length;
	if (same) return false;
	this.circledAnts = circledAnts;
	return true;
}

CanvasElementMapWithAnts.prototype.draw = function() {
	var halfScale, drawList, n, ant, w, dx, dy, d, fontSize, label, caption;
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
			ant = drawList[n];
			if (ant['owner'] === undefined) {
				w = halfScale * ant['size'];
				this.ctx.beginPath();
				this.ctx.arc(ant['x'] + halfScale, ant['y'] + halfScale, w, 0,
						2 * Math.PI, false);
				this.ctx.fill();
			} else {
				w = this.scale;
				dx = ant['x'];
				dy = ant['y'];
				if (ant['size'] !== 1) {
					d = 0.5 * (1.0 - ant['size']) * this.scale;
					dx += d;
					dy += d;
					w *= ant['size'];
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
	rows = this.vis.getRows();
	rowPixels = rows * this.scale;
	cols = this.vis.getCols();
	colPixels = cols * this.scale;
	this.ctx.lineWidth = Math.pow(this.scale, 0.3);
	for (hash in this.drawStates) {
		drawList = this.drawStates[hash];
		this.ctx.strokeStyle = hash;
		this.ctx.beginPath();
		for (n = drawList.length - 1; n >= 0; n--) {
			ant = drawList[n];
			if (this.pairing[ant.id] !== undefined) {
				for (d = this.pairing[ant.id].targets.length - 1; d >= 0; d--) {
					target = this.pairing[ant.id].targets[d];
					x1 = ant['x'] + halfScale;
					y1 = ant['y'] + halfScale;
					dx = Math.wrapAround(target['x'] - ant['x'], colPixels);
					if (2 * dx > colPixels) dx -= colPixels;
					x2 = x1 + 0.5 * dx;
					dy = Math.wrapAround(target['y'] - ant['y'], rowPixels);
					if (2 * dy > rowPixels) dy -= rowPixels;
					y2 = y1 + 0.5 * dy;
					this.drawWrapped(Math.min(x1, x2) - 1,
							Math.min(y1, y2) - 1, Math.abs(x2 - x1) + 2, Math
									.abs(y2 - y1) + 2, colPixels, rowPixels,
							function(fx1, fy1, fx2, fy2) {
								this.ctx.moveTo(fx1, fy1);
								this.ctx.lineTo(fx2, fy2);
							}, [ x1, y1, x2, y2 ]);
				}
			}
		}
		this.ctx.stroke();
	}

	if (this.mouseOverVis) {
		// draw attack and spawn radiuses
		ar = this.scale * Math.sqrt(this.vis.getAttackRadius2());
		sr = this.scale * Math.sqrt(this.vis.getSpawnRadius2());
		for (n = this.circledAnts.length - 1; n >= 0; --n) {
			ant = this.circledAnts[n];
			hash = '#';
			hash += INT_TO_HEX[ant['r']];
			hash += INT_TO_HEX[ant['g']];
			hash += INT_TO_HEX[ant['b']];
			this.ctx.strokeStyle = hash;
			this.ctx.beginPath();
			dx = ant['x'] + halfScale;
			dy = ant['y'] + halfScale;
			r = (ant['owner'] === undefined) ? sr : ar;
			x1 = dx - r;
			y1 = dy - r;
			this.drawWrapped(x1, y1, 2 * r, 2 * r, colPixels, rowPixels,
					function() {
						this.ctx.moveTo(dx + r, dy);
						this.ctx.arc(dx, dy, r, 0, 2 * Math.PI, false);
					});
			this.ctx.stroke();
		}
	}

	// draw A, B, C, D ... on ants or alternatively the global ant id
	label = this.vis.getConfig('label');
	if (label) {
		fontSize = Math.ceil(Math.max(this.scale, 8) / label);
		this.ctx.save();
		this.ctx.translate(halfScale, halfScale);
		this.ctx.textBaseline = 'middle';
		this.ctx.textAlign = 'center';
		this.ctx.font = 'bold ' + fontSize + 'px Arial';
		this.ctx.fillStyle = '#000';
		this.ctx.strokeStyle = '#fff';
		this.ctx.lineWidth = 0.2 * fontSize;
		for (hash in this.drawStates) {
			drawList = this.drawStates[hash];
			for (n = drawList.length - 1; n >= 0; n--) {
				ant = drawList[n];
				if (label === 1) {
					if (ant['owner'] === undefined) continue;
					caption = String.fromCharCode(65 + ant['owner']);
				} else {
					caption = ant.id;
				}
				this.ctx.strokeText(caption, ant['x'], ant['y']);
				this.ctx.fillText(caption, ant['x'], ant['y']);
				if (ant['x'] < 0) {
					this.ctx.strokeText(caption, ant['x'] + this.map.w,
							ant['y']);
					this.ctx.fillText(caption, ant['x'] + this.map.w, ant['y']);
					if (ant['y'] < 0) {
						this.ctx.strokeText(caption, ant['x'] + this.map.w,
								ant['y'] + this.map.h);
						this.ctx.fillText(caption, ant['x'] + this.map.w,
								ant['y'] + this.map.h);
					}
				}
				if (ant['y'] < 0) {
					this.ctx.strokeText(caption, ant['x'], ant['y']
							+ this.map.h);
					this.ctx.fillText(caption, ant['x'], ant['y'] + this.map.h);
				}
			}
		}
		this.ctx.restore();
	}

	// fog
	if (this.vis.getFogPlayer() !== undefined) {
		dx = (this.fog.w < colPixels) ? 0.5 * (colPixels - this.fog.w)
				- this.fog.shiftX : 0;
		dy = (this.fog.h < rowPixels) ? 0.5 * (rowPixels - this.fog.h)
				- this.fog.shiftY : 0;
		this.drawWrapped(dx, dy, this.fog.w, this.fog.h, this.w, this.h,
				function(ctx, img, x, y) {
					ctx.drawImage(img, x, y);
				}, [ this.ctx, this.fog.canvas, dx, dy ]);
	}
};

/**
 * @class The main map with ants dragged with the mouse and extended by borders
 *        if required
 * @constructor
 * @param {Visualizer}
 *        vis the visualizer for reference
 * @param {CanvasElementMapWithAnts}
 *        mapWithAnts the prepared map with ants
 */
function CanvasElementShiftedMap(vis, mapWithAnts) {
	CanvasElement.call(this);
	this.vis = vis;
	this.mapWithAnts = mapWithAnts;
	this.dependsOn(mapWithAnts);
	this.shiftX = 0;
	this.shiftY = 0;
}

CanvasElementShiftedMap.extend(CanvasElement);

CanvasElementShiftedMap.prototype.checkState = function() {
	if (this.vis.getShiftX() !== this.shiftX
			|| this.vis.getShiftY() !== this.shiftY) {
		this.invalid = true;
		this.shiftX = this.vis.getShiftX();
		this.shiftY = this.vis.getShiftY();
	}
};

CanvasElementShiftedMap.prototype.draw = function() {
	var x, y, dx, dy;
	var mx = (this.w - this.mapWithAnts.w) >> 1;
	var my = (this.h - this.mapWithAnts.h) >> 1;
	// map backdrop
	dx = mx + this.shiftX;
	dy = my + this.shiftY;
	dx -= Math.ceil(dx / this.mapWithAnts.w) * this.mapWithAnts.w;
	dy -= Math.ceil(dy / this.mapWithAnts.h) * this.mapWithAnts.h;
	for (x = dx; x < this.w; x += this.mapWithAnts.w) {
		for (y = dy; y < this.h; y += this.mapWithAnts.h) {
			this.ctx.drawImage(this.mapWithAnts.canvas, x, y);
		}
	}
	// map border if moved
	if (this.shiftX !== 0 || this.shiftY !== 0) {
		this.ctx.strokeStyle = '#000';
		this.ctx.lineWidth = 2;
		this.ctx.beginPath();
		for (x = dx; x <= this.w; x += this.mapWithAnts.w) {
			this.ctx.moveTo(x, 0);
			this.ctx.lineTo(x, this.h);
		}
		for (y = dy; y <= this.h; y += this.mapWithAnts.h) {
			this.ctx.moveTo(0, y);
			this.ctx.lineTo(this.w, y);
		}
		this.ctx.stroke();
	}
	// shaded static borders
	if (this.w > this.mapWithAnts.w) {
		dx = mx + this.mapWithAnts.w;
		this.ctx.fillStyle = 'rgba(0,0,0,0.3)';
		this.ctx.fillRect(0, 0, mx, this.h);
		this.ctx.fillRect(dx, 0, this.w - dx, this.h);
	}
	if (this.h > this.mapWithAnts.h) {
		dy = my + this.mapWithAnts.h;
		this.ctx.fillStyle = 'rgba(0,0,0,0.3)';
		this.ctx.fillRect(0, 0, this.w, my);
		this.ctx.fillRect(0, dy, this.w, this.h - dy);
	}
};

/**
 * @class A tiny canvas to contain a cached fog pattern for the selected player.
 * @constructor
 * @param {Visualizer}
 *        vis the visualizer for reference
 */
function CanvasElementFogPattern(vis) {
	CanvasElement.call(this);
	this.vis = vis;
	this.player = undefined;
	this.setSize(2, 2);
}
CanvasElementFogPattern.extend(CanvasElement);

CanvasElementFogPattern.prototype.checkState = function() {
	if (this.player !== this.vis.getFogPlayer()) {
		this.invalid = true;
		this.player = this.vis.getFogPlayer();
	}
};

CanvasElementFogPattern.prototype.draw = function() {
	if (this.player !== undefined) {
		this.ctx.fillStyle = this.vis.getHtmlPlayerColor(this.player);
		this.ctx.fillRect(0, 0, 1, 1);
		this.ctx.fillRect(1, 1, 1, 1);
	}
};

/**
 * @class A canvas element for statistical time graphs.
 * @constructor
 * @param {Visualizer}
 *        vis the visualizer for reference
 * @param {String}
 *        stats name of the stats to query from the visualizer
 */
function CanvasElementGraph(vis, stats) {
	CanvasElement.call(this);
	this.vis = vis;
	this.stats = stats;
	this.duration = 0;
}

CanvasElementGraph.extend(CanvasElement);

CanvasElementGraph.prototype.checkState = function() {
	if (this.duration !== this.vis.getReplayDuration()) {
		this.invalid = true;
		this.duration = this.vis.getReplayDuration();
	}
};

CanvasElementGraph.prototype.draw = function() {
	var min, max, i, k, t, scaleX, scaleY, status, x, y, tw, tx;
	var w = this.w - 1;
	var h = this.h - 1;
	var replay = this.vis.getReplay();
	var values = this.vis.getStats(this.stats).values;
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
	for (i = 0; i <= this.duration + 1; i++) {
		t = i + 1;
		this.ctx.moveTo(0.5 + scaleX * i, h - (t % 100 ? t % 10 ? 3 : 7 : 17));
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
			this.ctx.lineTo(0.5 + scaleX * k, 0.5 + scaleY
					* (max - values[k][i]));
		}
		this.ctx.stroke();
	}
	if (!this.vis.isStreaming && replay.meta['status']) {
		this.ctx.font = '10px Arial,Sans';
		for (i = values[0].length - 1; i >= 0; i--) {
			this.ctx.fillStyle = replay.htmlPlayerColors[i];
			this.ctx.strokeStyle = replay.htmlPlayerColors[i];
			k = replay.meta['replaydata']['scores'][i].length - 1;
			this.ctx.beginPath();
			x = 0.5 + k * scaleX;
			y = 0.5 + scaleY * (max - values[k][i]);
			this.ctx.moveTo(x, y);
			status = this.vis.statusToGlyph(i);
			tw = this.ctx.measureText(status).width;
			tx = Math.min(x, w - tw);
			if (y < 30) {
				y = ((y + 12) | 0) + 0.5;
				this.ctx.lineTo(x, y - 8);
				this.ctx.moveTo(tx, y - 8);
				this.ctx.lineTo(tx + tw, y - 8);
				this.ctx.fillText(status, tx, y);
			} else {
				y = ((y - 7) | 0) + 0.5;
				this.ctx.lineTo(x, y + 2);
				this.ctx.moveTo(tx, y + 2);
				this.ctx.lineTo(tx + tw, y + 2);
				this.ctx.fillText(status, tx, y);
			}
			this.ctx.stroke();
		}
	}
};

/**
 * @class A canvas element for statistics. It makes use of
 *        {@link CanvasElementGraph}.
 * @constructor
 * @param {Visualizer}
 *        vis the visualizer for reference
 * @param {String}
 *        caption the caption that is show to the left of the bar graph
 * @param {String}
 *        stats name of the stats to query from the visualizer
 */
function CanvasElementStats(vis, caption, stats) {
	CanvasElement.call(this);
	this.vis = vis;
	this.caption = caption;
	this.graph = new CanvasElementGraph(vis, stats);
	this.turn = 0;
	this.time = 0;
}

CanvasElementStats.extend(CanvasElement);

CanvasElementStats.MIN_HEIGHT = 30;
CanvasElementStats.MAX_HEIGHT = CanvasElementStats.MIN_HEIGHT + 70;

CanvasElementStats.prototype.setSize = function(width, height) {
	CanvasElement.prototype.setSize.call(this, width, height);
	this.graph.x = this.x + 4;
	this.graph.y = this.y + 32;
	this.graph.setSize(width - 8, Math.max(0, height - 36));
	this.showGraph = this.graph.h > 0;
};

CanvasElementStats.prototype.checkState = function() {
	if ((this.showGraph && this.time !== this.vis.getTime())
			|| this.time !== (this.vis.getTime() | 0)) {
		this.invalid = true;
		this.time = this.vis.getTime();
		this.turn = this.time | 0;
	}
};

CanvasElementStats.prototype.draw = function(resized) {
	var stats, text, x;
	if (resized) {
		this.ctx.fillStyle = '#fff';
		this.ctx.fillRect(0, 0, this.w, this.h);

		// outlines
		this.ctx.strokeStyle = '#444';
		this.ctx.lineWidth = 2;
		shapeRoundedRect(this.ctx, 0, 0, this.w, this.h, 1, 5);
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
	stats = this.vis.getStats(this.graph.stats);
	this.drawColorBar(95, 4, this.w - 99, 22, stats.values[this.turn],
			(this.turn === this.vis.getReplayDuration()) ? stats.bonus
					: undefined);

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
		if (this.turn === this.graph.duration && !this.vis.isStreaming) {
			text += 'end';
		} else {
			text += 'turn ' + (this.turn + 1) + '/' + this.graph.duration;
		}
		this.ctx.fillText(text, 4, 44);
	}
};

/**
 * Renders a horizontal bar graph of fixed size. Each block is colored using the
 * respective player color.
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
 *        bonus the bonus to apply to 'values', if any; the text will display
 *        value and bonus separated by a '+'
 */
CanvasElementStats.prototype.drawColorBar = function(x, y, w, h, values, bonus) {
	var useValues, i, scale, offsetX, offsetY, amount, text;
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
		amount = scale * useValues[i];
		this.ctx.fillStyle = this.vis.getReplay().htmlPlayerColors[i];
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
		text = Math.round(values[i]);
		if (this.vis.getConfig('label') === 1) {
			text = String.fromCharCode(65 + i) + ':' + text;
		}
		var bonusText = (bonus && bonus[i]) ? '+' + Math.round(bonus[i]) : '';
		var textWidth = this.ctx.measureText(text).width;
		if (bonusText) {
			this.ctx.font = 'bold italic 12px Monospace';
			var bonusTextWidth = this.ctx.measureText(bonusText).width;
			this.ctx.font = 'bold 16px Monospace';
		} else {
			bonusTextWidth = 0;
		}
		if (scale * useValues[i] >= textWidth + bonusTextWidth) {
			this.ctx.fillText(text, offsetX, offsetY);
			if (bonusText) {
				this.ctx.font = 'bold italic 12px Monospace';
				this.ctx.fillStyle = 'rgba(0,0,0,0.8)';
				this.ctx.fillText(bonusText, offsetX + textWidth, offsetY);
				this.ctx.font = 'bold 16px Monospace';
				this.ctx.fillStyle = 'rgba(0,0,0,0.5)';
			}
		}
		offsetX += scale * useValues[i];
	}
	this.ctx.restore();
};