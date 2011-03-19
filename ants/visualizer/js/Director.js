/**
 * @class The director is supposed to keep track of playback speed and position.
 * @constructor
 */
function Director(vis) {
	this.speed = 0;
	this.position = 0;
	this.lastTime = undefined;
	this.vis = vis;
	this.duration = 0;
	this.defaultSpeed = 1;
	this.cpu = 0.5;
	this.onstate = undefined;
	this.timeout = undefined;
}
/**
 * When the director is in playback mode it has a lastTime. This is just a
 * convenience method of querying the playback mode.
 * @type boolean
 */
Director.prototype.playing = function() {
	return this.speed !== 0;
};
Director.prototype.playStop = function() {
	this.playing() ? this.stop() : this.play();
};
Director.prototype.play = function() {
	if (!this.playing()) {
		if (this.position == this.duration) {
			this.position = 0;
		}
		this.speed = this.defaultSpeed;
		if (this.onstate) this.onstate();
		this.loop(false);
	}
};
Director.prototype.stop = function() {
	if (this.playing()) {
		this.speed = 0;
		this.lastTime = undefined;
		if (this.onstate) this.onstate();
	}
};
Director.prototype.gotoTick = function(tick) {
	this.stop();
	if (tick < 0) {
		tick = 0;
	} else if (tick > this.duration) {
		tick = this.duration;
	}
	if (this.position != tick) {
		var oldTick = this.position | 0;
		this.position = tick;
		this.vis.draw(this.position, oldTick !== tick);
	}
};
Director.prototype.loop = function() {
	if (this.speed === 0) {
		return;
	}
	var lastTime = this.lastTime;
	this.lastTime = new Date().getTime();
	var goOn = true;
	var oldTurn = this.position | 0;
	if (lastTime === undefined) {
		oldTurn = -1;
	} else {
		this.position += (this.lastTime - lastTime) * this.speed * 0.001;
	}
	if (this.position <= 0 && this.speed < 0) {
		this.position = 0;
		goOn = false;
		this.stop();
	} else if (this.position >= this.duration && this.speed > 0) {
		this.position = this.duration;
		goOn = false;
		this.stop();
	}
	this.vis.draw(this.position, (oldTurn != (this.position | 0)) ? this.position | 0 : undefined);
	if (goOn) {
		var that = this;
		var cpuTime = new Date().getTime() - this.lastTime;
		var delay = (this.cpu <= 0 || this.cpu > 1) ? 0 : Math.ceil(cpuTime / this.cpu - cpuTime);
		this.timeout = window.setTimeout(function() {that.loop(true)}, delay);
	}
};

Director.prototype.cleanUp = function() {
	window.clearTimeout(this.timeout);
	this.stop();
	this.position = 0;
};
/**
 * Causes the visualizer to draw the current game state.
 */
Director.prototype.draw = function() {
	this.vis.draw(this.position, this.position | 0);
};
/**
 * When an applet goes fullscreen it is detached and reinitialized. We need to
 * stop the animation until it is available again.
 */
Director.prototype.freeze = function() {
	window.clearTimeout(this.timeout);
};
