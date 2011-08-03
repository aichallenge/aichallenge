/**
 * @class A canvas that serves as an off-screen buffer for some graphics to be
 *		displayed possibly in tandem with other canvas elements or graphics.
 * @constructor
 * @param {Object} thiz the object that method belongs to
 * @param {Function} method the object function that will be called when this
 *		canvas needs to be redrawn
 */
CanvasElement = function(thiz, method) {
	this.canvas = document.createElement('canvas');
	this.ctx = this.canvas.getContext('2d');
	this.invalid = true;
	this.drawObj = thiz;
	this.drawFun = method;
	this.x = 0;
	this.y = 0;
	this.w = this.canvas.width;
	this.h = this.canvas.height;
}
CanvasElement.prototype.setSize = function(width, height) {
	if (this.w !== width || this.h !== height) {
		this.canvas.width  = this.w = width;
		this.canvas.height = this.h = height;
		this.invalid = true;
	}
}
CanvasElement.prototype.contains = function(x, y) {
	return (x >= this.x && x < this.x + this.w
		&& y >= this.y && y < this.y + this.h);
};
CanvasElement.prototype.invalidate = function() {
	this.invalid = true;
};
CanvasElement.prototype.validate = function() {
	if (this.invalid) {
		this.drawFun.call(this.drawObj, this.ctx, this.w, this.h);
		this.invalid = false;
	}
};