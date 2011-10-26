/**
 * @fileoverview This file contains the visualizer UI: buttons.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * @class The base class for buttons.
 * @constructor
 * @param {ButtonGroup}
 *        group the button group that this button belongs to
 * @param {Delegate}
 *        onclick a callback for the button-click
 * @property {Boolean} locked true, when the button is locked down (radio buttons)
 */
function Button(group, onclick) {
	this.group = group;
	this.onclick = onclick;
	this.hover = false;
	this.down = 0;
	this.locked = false;
	this.enabled = onclick ? true : false;
}

/**
 * This renders the button. drawInternal() is called from here to let derived classes add graphics
 * or text to the basic button style.
 */
Button.prototype.draw = function() {
	var g = this.group;
	var ctx = g.manager.ctx;
	var loc = this.getLocation();
	var cw = (loc.w > g.x + g.w - loc.x) ? g.x + g.w - loc.x : loc.w;
	var ch = (loc.h > g.y + g.h - loc.y) ? g.y + g.h - loc.y : loc.h;
	if (cw <= 0 || ch <= 0) return;
	ctx.save();
	ctx.beginPath();
	ctx.rect(loc.x, loc.y, cw, ch);
	ctx.clip();
	ctx.fillStyle = '#fff';
	ctx.fill();
	if (!this.enabled) ctx.globalAlpha = 0.5;
	var r = 0.2 * Math.min(loc.w, loc.h);
	if (this.onclick && this.enabled) {
		if (this.hover || this.down !== 0) {
			Shape.roundedRect(ctx, loc.x, loc.y, loc.w, loc.h, 1, r);
			ctx.fillStyle = 'rgb(255, 230, 200)';
			ctx.fill();
		}
		ctx.shadowBlur = 4 - 0.75 * this.down;
		ctx.shadowOffsetX = -0.5 * this.down;
		ctx.shadowOffsetY = +0.5 * this.down;
		if (Quirks.fullImageShadowSupport) {
			ctx.shadowColor = 'rgba(0, 0, 0, 0.7)';
		} else {
			// FireFox 5 with shadow bug or other bad support
			ctx.shadowColor = 'rgba(0, 0, 0, 0)';
		}
	}
	ctx.save();
	ctx.translate(loc.x, loc.y - 1 + this.down);
	this.drawInternal(ctx);
	ctx.restore();
	if (this.onclick && this.enabled && (this.hover || this.down)) {
		ctx.shadowColor = 'rgba(0, 0, 0, 0)';
		ctx.lineWidth = 2;
		ctx.strokeStyle = '#444';
		ctx.stroke();
	}
	ctx.restore();
};

/**
 * Updates the button state when it is pressed and invokes the button onclick delegate.
 */
Button.prototype.mouseDown = function() {
	var i, btns;
	switch (this.group.mode) {
	case ButtonGroup.MODE_RADIO:
		this.locked = !this.locked;
		if (this.locked) {
			if (this.onclick) {
				this.onclick.invoke([ this.idx ]);
			}
			btns = this.group.buttons;
			for (i = 0; i < btns.length; i++) {
				if (btns[i].down !== 0) {
					btns[i].down = 0;
					btns[i].locked = false;
					btns[i].draw();
				}
			}
		}
	case ButtonGroup.MODE_NORMAL:
		this.down = 2;
		break;
	}
	this.draw();
};

/**
 * Updates the button state when it is released and invokes the button onclick delegate.
 */
Button.prototype.mouseUp = function() {
	switch (this.group.mode) {
	case ButtonGroup.MODE_RADIO:
		if (this.locked) {
			this.down = 1;
		} else {
			if (this.onclick) {
				this.onclick.invoke([]);
			}
			this.down = 0;
		}
		break;
	case ButtonGroup.MODE_NORMAL:
		if (this.hover && this.onclick) this.onclick.invoke();
		this.down = 0;
		break;
	}
	this.draw();
};

/**
 * @class a group of buttons
 * @constructor
 * @param {ButtonManager}
 *        manager the button manager that this button group will belong to
 * @param {Number}
 *        border adds padding around the buttons on each side
 */
function ButtonGroup(manager, border) {
	this.buttons = [];
	this.manager = manager;
	this.border = border ? border : 0;
}

/**
 * makes a button group invisible
 */
ButtonGroup.MODE_HIDDEN = 0;

/**
 * a normal button group where the buttons can be clicked independently
 */
ButtonGroup.MODE_NORMAL = 1;

/**
 * a radio button group where only one button is down at a time
 */
ButtonGroup.MODE_RADIO = 2;

/**
 * Redraws all buttons in this group.
 */
ButtonGroup.prototype.draw = function() {
	var i;
	for (i = 0; i < this.buttons.length; i++) {
		if (this.buttons[i].draw) this.buttons[i].draw();
	}
};

/**
 * Finds an active button under the mouse cursor.
 * 
 * @param {Number}
 *        mx the mouse x position
 * @param {Number}
 *        my the mouse y position
 * @returns {Button} the active button under the mouse cursor or null
 */
ButtonGroup.prototype.mouseMove = function(mx, my) {
	var i;
	for (i = 0; i < this.buttons.length; i++) {
		if (this.buttons[i].getLocation) {
			if (this.buttons[i].getLocation().contains(mx, my)) {
				return (this.buttons[i].enabled) ? this.buttons[i] : null;
			}
		}
	}
	return null;
};

/**
 * @class A specialized button that displays an image.
 * @extends Button
 * @constructor
 * @param {ImageButtonGroup}
 *        group the button group that this button belongs to
 * @param {Number}
 *        idx selects the partial image to be shown from the group's image
 * @param {Number}
 *        delta the actual position inside the button group in pixels
 * @param {Delegate}
 *        onclick a callback for the button-click
 * @param {String}
 *        hint a hint that is displayed when the mouse hovers over this button
 * @property {Number} the partial image to be shown from the group's image
 */
function ImageButton(group, idx, delta, onclick, hint) {
	this.upper(group, onclick);
	this.idx = idx;
	this.offset = (group.size - 2 * group.border) * idx;
	this.delta = delta;
	this.hint = hint;
}
ImageButton.extend(Button);

/**
 * Draws the partial image into the button.
 * 
 * @private
 * @param {CanvasRenderingContext2D}
 *        ctx the rendering context to use
 * @see Button#draw
 */
ImageButton.prototype.drawInternal = function(ctx) {
	var b = this.group.border;
	var bs = this.group.size - 2 * b;
	ctx.drawImage(this.group.img, this.offset, 0, bs, bs, b, b, bs, bs);
};

/**
 * Calculates and returns the position and size of this button.
 * 
 * @returns {Location} the location of this button
 */
ImageButton.prototype.getLocation = function() {
	return new Location(this.group.x + (this.group.vertical ? 0 : this.delta), this.group.y
			+ (this.group.vertical ? this.delta : 0), this.group.size, this.group.size);
};

/**
 * @class A button group that displays graphics of the same size.
 * @extends ButtonGroup
 * @constructor
 * @param {ButtonManager}
 *        manager the button manager that this button group will belong to
 * @param {HTMLImageElement}
 *        img the image that contains a row of graphics for this button group
 * @param {Boolean}
 *        layout one of {@link ImageButtonGroup#HORIZONTAL} or {@link ImageButtonGroup#VERTICAL}
 * @param {Number}
 *        mode one of {@link ButtonGroup#MODE_HIDDEN} (hides the button group),
 *        {@link ButtonGroup#MODE_NORMAL} (normal buttons) or {@link ButtonGroup#MODE_RADIO} (radio
 *        buttons)
 * @param {Number}
 *        border adds padding around the buttons on each side
 * @param {Number}
 *        extent the size of the button group (height for vertical layouts, width for horizontal
 *        layouts)
 */
function ImageButtonGroup(manager, img, layout, mode, border, extent) {
	this.upper(manager, border);
	this.img = img;
	this.vertical = layout;
	this.mode = mode;
	this.size = img.height + 2 * this.border;
	this.x = 0;
	this.y = 0;
	this.w = (this.vertical) ? this.size : extent;
	this.h = (this.vertical) ? extent : this.size;
}
ImageButtonGroup.extend(ButtonGroup);

/**
 * horizontal button layout
 */
ImageButtonGroup.HORIZONTAL = false;

/**
 * vertical button layout
 */
ImageButtonGroup.VERTICAL = true;

/**
 * Calculates the optimal position for the next added button.
 * 
 * @private
 * @returns {Number} the optimal position for the next added button
 */
ImageButtonGroup.prototype.nextDelta = function() {
	if (this.buttons.length !== 0) {
		var lastBtn = this.buttons[this.buttons.length - 1];
		return lastBtn.delta + (lastBtn.size || this.size);
	}
	return 0;
};

/**
 * Adds a button to this group according to given parameters.
 * 
 * @param {Number}
 *        idx selects the partial image to be shown from the group's image
 * @param {Function}
 *        onclick a callback for the button-click
 * @param {String}
 *        hint a hint that is displayed when the mouse hovers over this button
 * @returns {ImageButton} the newly added button
 */
ImageButtonGroup.prototype.addButton = function(idx, onclick, hint) {
	var delta = this.nextDelta();
	var btn = new ImageButton(this, idx, delta, onclick, hint);
	this.buttons.push(btn);
	return btn;
};

/**
 * Adds a some space in between buttons.
 * 
 * @param {Number}
 *        size the amount of space in pixels
 */
ImageButtonGroup.prototype.addSpace = function(size) {
	var delta = this.nextDelta();
	this.buttons.push({
		delta : delta,
		size : size
	});
};

/**
 * Looks up the button that displays a given partial image.
 * 
 * @param {Number}
 *        idx index into the image atlas of the partial image
 * @returns {ImageButton} the first found button or null if none matches
 */
ImageButtonGroup.prototype.getButton = function(idx) {
	var i;
	for (i = 0; i < this.buttons.length; i++) {
		if (this.buttons[i].idx === idx) return this.buttons[i];
	}
	return null;
};

/**
 * @class A specialized button that displays a short label.
 * @extends Button
 * @constructor
 * @param {TextButtonGroup}
 *        group the button group that this button belongs to
 * @param {String}
 *        text the label that is displayed on the button
 * @param color
 *        a fillStyle to be applied when the label is drawn
 * @param {Delegate}
 *        onclick a callback for the button-click
 */
function TextButton(group, text, color, onclick) {
	this.upper(group, onclick);
	this.text = text;
	this.color = color;
	this.x = 0;
	this.y = 0;
	this.setText(text);
	this.h = 28;
}
TextButton.extend(Button);

/**
 * Draws the label into the button.
 * 
 * @private
 * @param {CanvasRenderingContext2D}
 *        ctx the rendering context to use
 * @see Button#draw
 */
TextButton.prototype.drawInternal = function(ctx) {
	ctx.textAlign = 'left';
	ctx.textBaseline = 'bottom';
	ctx.font = FONT;

	ctx.strokeStyle = '#000';
	ctx.lineWidth = 1;
	ctx.strokeText(this.text, 4, 25);

	ctx.fillStyle = this.color;
	ctx.shadowColor = this.color;
	ctx.shadowOffsetX = 0;
	ctx.shadowOffsetY = 0;
	ctx.shadowBlur = 0.5;
	ctx.fillText(this.text, 4, 25);
};

/**
 * Calculates and returns the position and size of this button.
 * 
 * @returns {Location} the location of this button
 */
TextButton.prototype.getLocation = function() {
	return new Location(this.group.x + this.x, this.group.y + this.y, this.w, this.h);
};

/**
 * Updates the button text and resizes the button.
 * 
 * @param {String}
 *        text The new caption.
 */
TextButton.prototype.setText = function(text) {
	this.text = text;
	this.group.manager.ctx.font = FONT;
	this.w = this.group.manager.ctx.measureText(text).width + 8;
};

/**
 * @class A group of labeled buttons.
 * @extends ButtonGroup
 * @constructor
 * @param {ButtonManager}
 *        manager the button manager that this button group will belong to
 * @param {Number}
 *        mode one of {@link ButtonGroup#MODE_HIDDEN} (hides the button group),
 *        {@link ButtonGroup#MODE_NORMAL} (normal buttons) or {@link ButtonGroup#MODE_RADIO} (radio
 *        buttons)
 * @param {Number}
 *        border adds padding around the buttons on each side
 */
function TextButtonGroup(manager, mode, border) {
	this.upper(manager, border);
	this.mode = mode;
	this.x = 0;
	this.y = 0;
	this.h = undefined;
	this.w = undefined;
}
TextButtonGroup.extend(ButtonGroup);

/**
 * Adds a button to this group according to given parameters.
 * 
 * @param {String}
 *        text the label that is displayed on the button
 * @param color
 *        a fillStyle to be applied when the label is drawn
 * @param {Function}
 *        onclick a callback for the button-click
 * @returns {TextButton} the newly added button
 */
TextButtonGroup.prototype.addButton = function(text, color, onclick) {
	var btn = new TextButton(this, text, color, onclick);
	this.buttons.push(btn);
	return btn;
};

/**
 * This will layout the buttons according to a given width, much like left aligned text in a word
 * processor.
 * 
 * @param {Number}
 *        width the maximum line width in pixels
 * @returns {Number} the resulting height of the group after the process
 */
TextButtonGroup.prototype.cascade = function(width) {
	var i;
	var btn = null;
	var addX = 0;
	var addY = 0;
	this.w = width;
	for (i = 0; i < this.buttons.length; i++) {
		btn = this.buttons[i];
		if (addX && addX + btn.w > this.w) {
			addX = 0;
			addY += btn.h + 2;
		}
		btn.x = addX;
		btn.y = addY;
		addX += btn.w + 2;
	}
	return this.h = btn ? addY + btn.h : 0;
};

/**
 * @class Manages buttons and their mouse events.
 * @constructor
 * @param {CanvasRenderingContext2D}
 *        ctx The context into which the buttons are rendered.
 */
function ButtonManager(ctx) {
	this.ctx = ctx;
	this.groups = new Object();
	this.hover = null;
	this.pinned = null;
}

/**
 * Adds a new image button group.
 * 
 * @param {String}
 *        name a descriptive group name for easy lookup
 * @param {HTMLImageElement}
 *        img the reference image that partial images for the buttons will be taken from
 * @param {Boolean}
 *        layout one of {@link ImageButtonGroup#HORIZONTAL} or {@link ImageButtonGroup#VERTICAL}
 * @param {Number}
 *        mode one of {@link ButtonGroup#MODE_HIDDEN} (hides the button group),
 *        {@link ButtonGroup#MODE_NORMAL} (normal buttons) or {@link ButtonGroup#MODE_RADIO} (radio
 *        buttons)
 * @param {Number}
 *        border adds padding around the buttons on each side
 * @param {Number}
 *        extent the size of the button group (height for vertical layouts, width for horizontal
 *        layouts)
 * @returns {ImageButtonGroup} the created button group
 */
ButtonManager.prototype.addImageGroup = function(name, img, layout, mode, border, extent) {
	return this.groups[name] = new ImageButtonGroup(this, img, layout, mode, border, extent);
};

/**
 * Adds a new text button group.
 * 
 * @param {String}
 *        name a descriptive group name for easy lookup
 * @param {Number}
 *        mode one of {@link ButtonGroup#MODE_HIDDEN} (hides the button group),
 *        {@link ButtonGroup#MODE_NORMAL} (normal buttons) or {@link ButtonGroup#MODE_RADIO} (radio
 *        buttons)
 * @param {Number}
 *        border adds padding around the buttons on each side
 * @returns {TextButtonGroup} the created button group
 */
ButtonManager.prototype.addTextGroup = function(name, mode, border) {
	return this.groups[name] = new TextButtonGroup(this, mode, border);
};

/**
 * Redraws all visible button groups.
 */
ButtonManager.prototype.draw = function() {
	var groupName = undefined;
	for (groupName in this.groups)
		if (this.groups.hasOwnProperty(groupName)) {
			if (this.groups[groupName].mode !== ButtonGroup.MODE_HIDDEN) {
				this.groups[groupName].draw();
			}
		}
};

/**
 * Retrieves a button group with a given name from the manager.
 * 
 * @param {String}
 *        name the name of the group
 * @returns {ButtonGroup} the button group if it exists or undefined
 */
ButtonManager.prototype.getGroup = function(name) {
	return this.groups[name];
};

/**
 * Finds an active button under the mouse cursor. The call is forwarded to matching button groups.
 * 
 * @see ButtonGroup#mouseMove
 * @param {Number}
 *        mx the mouse x position
 * @param {Number}
 *        my the mouse y position
 * @returns {Button} the active button under the mouse cursor or null
 */
ButtonManager.prototype.mouseMove = function(mx, my) {
	var result = null;
	var groupName = undefined;
	for (groupName in this.groups) {
		// use of method, to allow Eclipse to infer the type
		var bg = this.getGroup(groupName);
		if (bg.mode != ButtonGroup.MODE_HIDDEN && my >= bg.y && my < bg.y + bg.h && mx >= bg.x
				&& mx < bg.x + bg.w) {
			result = bg.mouseMove(mx, my);
			if (result !== null) {
				break;
			}
		}
	}
	if (this.hover !== result) {
		// the hover must change
		if (this.hover) {
			// we had a previous hover
			this.hover.hover = false;
			if (this.hover === this.pinned) {
				// this was a pinned button, release it
				this.hover.mouseUp();
			} else {
				this.hover.draw();
			}
		}
		this.hover = result;
		if (result) {
			// we have a new hover
			this.hover.hover = true;
			if (this.hover === this.pinned) {
				this.hover.mouseDown();
			} else {
				this.hover.draw();
			}
		}
	}
	return result;
};

/**
 * Mouse release event that is forwarded to the button the mouse was on, if any.
 */
ButtonManager.prototype.mouseUp = function() {
	if (this.pinned) {
		this.pinned.mouseUp();
		this.pinned = null;
	}
};

/**
 * Mouse press event that is forwarded to the button the mouse was over, if any.
 */
ButtonManager.prototype.mouseDown = function() {
	if (this.hover) {
		this.pinned = this.hover;
		this.pinned.mouseDown();
	}
};
