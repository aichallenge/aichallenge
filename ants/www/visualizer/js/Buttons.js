function Button(group, offset, delta, action) {
	this.group = group;
	this.offset = offset;
	this.delta = delta;
	this.action = action;
	this.hover = false;
	this.down = false;
}
Button.prototype.mouseDown = function() {
	switch (this.group.mode) {
		case ButtonGroup.MODE_RADIO:
			if (this.down) return;
			var btns = this.group.buttons;
			for (var i = 0; i < btns.length; i++) {
				if (btns[i].down) {
					btns[i].down = false;
					btns[i].draw();
				}
			}
		case ButtonGroup.MODE_NORMAL:
			this.down = true;
			this.draw();
			break;
	}
};
Button.prototype.mouseUp = function() {
	switch (this.group.mode) {
		case ButtonGroup.MODE_NORMAL:
			this.down = false;
			this.draw();
			break;
	}
};
Button.prototype.draw = function() {
	var ctx = this.group.manager.vis.main.ctx;
	with (this.group) {
		var ix = x + (vertical ? 0 : this.delta);
		var iy = y + (vertical ? this.delta : 0);
		var n = 1;
		var r = 0.2 * this.group.size;
		var d = 0.5 * Math.PI;
		ctx.save();
		ctx.translate(ix, iy);
		ctx.clearRect(0, 0, size, size);
		ctx.beginPath();
		ctx.moveTo(0, 0);
		ctx.lineTo(size, 0);
		ctx.lineTo(size, size);
		ctx.lineTo(0, size);
		ctx.closePath();
		ctx.clip();
		if (this.hover || this.down) {
			ctx.beginPath();
			ctx.moveTo(r + n, n);
			ctx.lineTo(size - r - n, n);
			ctx.arc(size - r - n, r + n, r, -d, 0, false);
			ctx.lineTo(size - n, size - r - n);
			ctx.arc(size - r - n, size - r - n, r, 0, d, false);
			ctx.lineTo(r + n, size - n);
			ctx.arc(r + n, size - r - n, r, d, 2 * d, false);
			ctx.lineTo(n, r + n);
			ctx.arc(r + n, r + n, r, 2 * d, 3 * d, false);
			ctx.fillStyle = this.down ? 'rgba(108, 200, 158, 0.5)' : 'rgba(108, 108, 158, 0.3)';
			ctx.fill();
		}
		ctx.save();
		ctx.shadowColor = 'rgba(0, 50, 200, 0.7)';
		var bs = size - 2 * border;
		var dy = (this.down) ? 0 : -2;
		if (dy) {
			ctx.shadowBlur = 5;
			ctx.shadowOffsetX = -2;
			ctx.shadowOffsetY = +2;
		} else {
			ctx.shadowBlur = 1;
		}
		ctx.drawImage(img, this.offset, 0, bs, bs, border, border + dy, bs, bs);
		ctx.restore();
		if (this.hover || this.down) {
			ctx.lineWidth = 2;
			ctx.strokeStyle = 'rgba(0, 0, 0, 1)';
			ctx.stroke();
		}
		ctx.restore();
	}
};


function ButtonGroup(manager, img, layout, mode, border) {
	this.manager = manager;
	this.img = img;
	this.vertical = layout;
	this.mode = mode;
	this.border = border ? border : 0;
	this.x = 0;
	this.y = 0;
	this.size = img.height + 2 * this.border;
	this.w = (this.vertical) ? this.size : 0;
	this.h = (this.vertical) ? 0 : this.size;
	this.buttons = [];
}
ButtonGroup.HORIZONTAL = false;
ButtonGroup.VERTICAL = true;
ButtonGroup.MODE_HIDDEN = 0;
ButtonGroup.MODE_NORMAL = 1;
ButtonGroup.MODE_RADIO = 2;
ButtonGroup.prototype.addButton = function(idx, action) {
	this.buttons.push(new Button(this, (this.size - 2 * this.border) * idx, (this.vertical) ? this.h : this.w, action));
	this.vertical ? this.h += this.size : this.w += this.size;
};
ButtonGroup.prototype.addSpace = function(size) {
	this.buttons.push({
		delta: (this.vertical) ? this.h : this.w,
		size: size
	});
	this.vertical ? this.h += size : this.w += size;
};
ButtonGroup.prototype.draw = function() {
	for (var i = 0; i < this.buttons.length; i++) {
		if (this.buttons[i].draw) this.buttons[i].draw();
	}
};
ButtonGroup.prototype.mouseMove = function(mx, my) {
	var delta = (this.vertical) ? my : mx;
	for (var i = 0; i < this.buttons.length; i++) {
		if (delta < this.buttons[i].delta + (this.buttons[i].size ? this.buttons[i].size : this.size)) {
			return (this.buttons[i].draw) ? this.buttons[i] : null;
		}
	}
	return null;
};


/**
 * Manages buttons and their mouse events.
 * @param {Visualizer} vis the visualizer
 */
function ButtonManager(vis) {
	this.vis = vis;
	this.groups = {};
	this.hover = null;
	this.nailed = null;
}
/**
 * @returns {ButtonGroup} the created button group
 */
ButtonManager.prototype.addGroup = function(name, img, layout, mode, border) {
	return this.groups[name] = new ButtonGroup(this, img, layout, mode, border);
};
ButtonManager.prototype.draw = function() {
	for (var name in this.groups) with (this.groups[name]) {
		if (mode != ButtonGroup.MODE_HIDDEN) {
			draw();
		}
	}
};
ButtonManager.prototype.mouseMove = function(mx, my) {
	var result = null;
	for (var name in this.groups) with (this.groups[name]) {
		if (mode != ButtonGroup.MODE_HIDDEN && my >= y && my < y + h && mx >= x && mx < x + w) {
			mx -= x;
			my -= y;
			result = mouseMove(mx, my);
			if (result !== null) {
				break;
			}
		}
	}
	if (this.hover !== result) {
		if (this.hover) {
			if (this.hover.hover || this.hover.down) {
				this.hover.hover = false;
				this.hover.down &= this.hover.group.mode == ButtonGroup.MODE_RADIO;
				this.hover.draw();
			}
		}
		if (result && (!this.nailed || this.nailed === result)) {
			if (!result.hover) {
				result.hover = true;
				result.down = (result === this.nailed) || (result.down && this.hover.group.mode == ButtonGroup.MODE_RADIO);
				result.draw();
			}
		}
		this.hover = result;
		this.repaintCheck();
	}
	return result;
};
ButtonManager.prototype.mouseUp = function() {
	if (this.nailed) {
		this.nailed.mouseUp();
		if (this.nailed == this.hover) {
			this.nailed.action();
		}
		this.nailed = null;
		this.repaintCheck();
	}
};
ButtonManager.prototype.mouseDown = function() {
	if (this.hover) {
		this.hover.mouseDown();
		this.nailed = this.hover;
		this.repaintCheck();
	}
};
/**
 * @private
 */
ButtonManager.prototype.repaintCheck = function() {
	if (this.vis.options.java) {
		this.vis.main.element.repaint();
	}
};
