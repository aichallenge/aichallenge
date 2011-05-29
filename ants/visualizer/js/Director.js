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
	this.stopAt = undefined;
	this.cpu = vis.config['cpu'];
	this.onstate = undefined;
	this.timeout = undefined;
	this.frameCounter = undefined;
	this.frameStart = undefined;
	this.frameCpu = undefined;
	this.tickFlag = false;
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
		this.stopAt = this.duration;
		if (this.onstate) this.onstate();
		this.loop(0);
		if (this.vis.options['profile']) console.profile();
	}
};
Director.prototype.stop = function() {
	if (this.playing()) {
		if (this.vis.options['profile']) console.profileEnd();
		this.speed = 0;
		this.lastTime = undefined;
		if (this.onstate) this.onstate();
	}
};
Director.prototype.gotoTick = function(time) {
	this.stop();
	if (time < 0) {
		time = 0;
	} else if (time > this.duration) {
		time = this.duration;
	}
	if (this.position != time) {
		var oldTick = this.position | 0;
		this.position = time;
		time |= 0;
		this.vis.draw(this.position, oldTick !== time);
	}
};
Director.prototype.slowmoTo = function(time) {
	if (time < 0) {
		time = 0;
	} else if (time > this.duration) {
		time = this.duration;
	}
	this.stopAt = time;
	var playing = this.playing();
	if (this.position < time) {
		this.speed = +1;
	} else {
		this.speed = -1;
	}
	if (!playing) {
		if (this.onstate) this.onstate();
		this.loop(0);
	}
};
Director.prototype.loop = function(delay) {
	if (this.speed === 0) {
		return;
	}
	var lastTime = this.lastTime;
	this.lastTime = new Date().getTime();
	var oldTurn = this.position | 0;
	if (lastTime === undefined) {
		var cpuTime = undefined;
		oldTurn = -1;
	} else {
		cpuTime = this.lastTime - lastTime - delay;
		this.position += (this.lastTime - lastTime) * this.speed * 0.001;
	}
	var goOn = true;
	if (this.speed < 0 && this.position <= this.stopAt) {
		this.position = this.stopAt;
		goOn = false;
		this.stop();
	} else if (this.speed > 0 && this.position >= this.stopAt) {
		this.position = this.stopAt;
		goOn = false;
		this.stop();
	}
	this.vis.draw(this.position, this.tickFlag || oldTurn != (this.position | 0));
	this.tickFlag = false;
	if (goOn) {
		if (this.vis.options['debug'] && cpuTime !== undefined) {
			if (this.frameStart === undefined) {
				this.frameStart = lastTime;
				this.frameCounter = 0;
				this.frameCpu = 0;
			}
			this.frameCounter++;
			this.frameCpu += cpuTime;
			if (this.lastTime >= this.frameStart + 1000) {
				var delta = (this.lastTime - this.frameStart);
				var fps = Math.round(1000 * this.frameCounter / delta);
				var cpu = Math.round( 100 * this.frameCpu     / delta);
				document.title = fps + ' fps @ ' + cpu + '% cpu';
				this.frameStart = this.lastTime;
				this.frameCounter = 0;
				this.frameCpu = 0;
			}
		}
		var that = this;
		var useMax = (this.cpu <= 0 || this.cpu > 1) || cpuTime === undefined;
		if (useMax) {
			that.timeout = window.setTimeout(function() {that.loop(delay)}, 0);
		} else {
			delay = useMax ? 0 : Math.ceil(cpuTime / this.cpu - cpuTime);
			// looks odd, but synchronizes JS and rendering threads so we get
			// accurate cpu times
			this.timeout = window.setTimeout(function() {
				that.timeout = window.setTimeout(function() {that.loop(delay)}, delay);
			}, 0);
		}
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
	if (this.playing()) {
		this.tickFlag = true;
	} else {
		this.vis.draw(this.position, true);
	}
};
/**
 * When an applet goes fullscreen it is detached and reinitialized. We need to
 * stop the animation until it is available again.
 */
Director.prototype.freeze = function() {
	window.clearTimeout(this.timeout);
};
Director.prototype.speedUp = function() {
	this.defaultSpeed *= 1.5;
	this.speed *= 1.5;
};
Director.prototype.slowDown = function() {
	this.defaultSpeed /= 1.5;
	this.speed /= 1.5;
};