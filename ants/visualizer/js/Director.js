/**
 * @fileOverview This file is dedicated to the playback logic of the visualizer.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * @class The director is supposed to keep track of playback speed and position and keep an eye on
 *        CPU usage. It does that in an acceptable fashion.
 * @constructor
 * @param {Visualizer}
 *        vis The visualizer that the director will communicate with.
 */
function Director(vis) {
	this.speed = 0;
	this.lastTime = undefined;
	this.time = 0;
	this.vis = vis;
	this.duration = 0;
	this.defaultSpeed = 1;
	this.stopAt = undefined;
	this.cpu = vis.state.config['cpu'];
	this.onstate = undefined;
	this.timeout = undefined;
	this.frameCounter = 0;
	this.frameStart = undefined;
	this.frameCpu = undefined;
	this.fixedFpt = undefined;
}

/**
 * Query if the director is currently playing back.
 * 
 * @returns {Boolean} True, if the current speed is not 0
 */
Director.prototype.playing = function() {
	return this.speed !== 0;
};

/**
 * Toggles between playing and pausing by calling play() and stop().
 */
Director.prototype.playStop = function() {
	this.playing() ? this.stop() : this.play();
};

/**
 * Starts or resumes playback. If the visualizer is at the end of the replay it starts over again.
 * This method will also start profiling if selected by {@link Config#profile}.
 */
Director.prototype.play = function() {
	if (!this.playing()) {
		this.speed = this.defaultSpeed;
		if (this.vis.state.options['loop']) {
			this.stopAt = undefined;
		} else {
			if (this.time === this.duration) {
				this.vis.state.time = this.time = 0;
			}
			this.stopAt = this.duration;
		}
		if (this.onstate) this.onstate();
		this.loop(0);
		if (this.vis.state.options['profile']) console.profile();
	}
};

/**
 * Pauses playback. If play() started a profiling session, it will be stopped here and the result
 * will appear in Chrome's profiling view.
 */
Director.prototype.stop = function() {
	if (this.playing()) {
		if (this.vis.state.options['profile']) console.profileEnd();
		this.speed = 0;
		this.lastTime = undefined;
		if (this.onstate) this.onstate();
	}
};

/**
 * Stops playback and jumps to a specific time. This is usually used to jump to the start of a turn.
 * 
 * @param {Number}
 *        time The time in question. 0 will jump to the start of 'turn 1'. Out of range values will
 *        be clamped.
 */
Director.prototype.gotoTick = function(time) {
	this.stop();
	var effectiveTime = Math.clamp(time, 0, this.duration);
	this.vis.state.fade = undefined;
	if (this.vis.state.time !== effectiveTime) {
		this.vis.state.time = this.time = effectiveTime;
		this.vis.draw();
	}
};

/**
 * Starts a slow motion playback (forward or backward) from the current position to a given time at
 * a rate of one turn per second.
 * 
 * @param {Number}
 *        time The time in question. 0 will jump to the start of 'turn 1'. Out of range values will
 *        be clamped.
 */
Director.prototype.slowmoTo = function(time) {
	var wasPlaying;
	var effectiveTime = Math.clamp(time, 0, this.duration);
	if (this.vis.state.time !== effectiveTime) {
		this.stopAt = effectiveTime;
		wasPlaying = this.playing();
		if (this.vis.state.time < effectiveTime) {
			this.speed = +1;
		} else {
			this.speed = -1;
		}
		if (!wasPlaying) {
			if (this.onstate) this.onstate();
			this.loop(0);
		}
	}
};

/**
 * Performs one playback step. Basically it calls {@link Visualizer#draw}, and will schedule the
 * next call to itself if the playback hasn't met an end condition (like the target time for a
 * slow-mo). The CPU usage limit is obeyed here and if the visualizer is in {@link Config#debug}
 * mode the title bar of the browser is updated with an FPS counter.
 * 
 * @private
 * @param {Number}
 *        delay This is the delay that was artificially introduced to meet the CPU usage limit given
 *        in the {@link Config}. It is used to calculate a CPU usage estimate.
 */
Director.prototype.loop = function(delay) {
	var newDelay, i, a, repeat, useMax;
	var cpuTime = undefined;
	var lastTime = this.lastTime;
	if (this.speed === 0) {
		return;
	}
	if (this.fixedFpt === undefined) {
		this.lastTime = new Date().getTime();
		if (lastTime !== undefined) {
			cpuTime = this.lastTime - lastTime - delay;
			do {
				a = (this.lastTime - lastTime) * this.speed * 0.001;
				i = undefined;
				repeat = false;
				if (this.vis.state.options['loop'] && a !== 0) {
					if (this.speed > 0) {
						if (this.time < 0) {
							i = 0 - this.time;
							if (i <= a) {
								this.time += i;
								this.speed = this.defaultSpeed;
								repeat = true;
							}
						} else if (this.time < this.duration) {
							i = this.duration - this.time;
							if (i <= a) {
								this.time += i;
								this.speed = 1;
								repeat = true;
							}
						} else {
							i = this.duration + 1.5 - this.time;
							if (i <= a) {
								this.time = -1.5;
								repeat = true;
							}
						}
					} else {
						if (this.time > this.duration) {
							i = this.duration - this.time;
							if (i >= a) {
								this.time += i;
								this.speed = this.defaultSpeed;
								repeat = true;
							}
						} else if (this.time > 0) {
							i = 0 - this.time;
							if (i >= a) {
								this.time += i;
								this.speed = -1;
								repeat = true;
							}
						} else {
							i = -1.5 - this.time;
							if (i >= a) {
								this.time = this.duration + 1.5;
								repeat = true;
							}
						}
					}
				}
				if (repeat) {
					lastTime += (this.lastTime - lastTime) * (i / a);
				} else {
					this.time += a;
				}
			} while (repeat);
		}
	} else {
		this.frameCounter++;
		this.time = this.frameCounter / this.fixedFpt;
	}
	// check if we can go on after this frame, stop or fade out and repeat
	var goOn = true;
	if (this.speed < 0) {
		if (this.time <= this.stopAt) {
			this.time = this.stopAt;
			goOn = false;
			this.stop();
		}
	} else if (this.speed > 0) {
		if (this.time >= this.stopAt) {
			this.time = this.stopAt;
			goOn = false;
			this.stop();
		}
	}
	// check for fade out color
	if (this.vis.state.options['loop'] && (this.time < 0 || this.time > this.duration)) {
		if (this.time < -1) {
			i = Math.round(255 * (2 + this.time));
			a = 1;
		} else if (this.time < 0) {
			i = 255;
			a = -this.time;
		} else if (this.time > this.duration + 1) {
			i = Math.round(255 * (this.time - this.duration - 1));
			a = 1;
		} else {
			i = 0;
			a = this.time - this.duration;
		}
		this.vis.state.fade = 'rgba(' + i + ',' + i + ',' + i + ',' + a + ')';
	} else {
		this.vis.state.fade = undefined;
	}
	this.vis.state.time = Math.clamp(this.time, 0, this.duration);
	this.vis.draw();
	if (goOn) {
		if (this.fixedFpt === undefined) {
			if (this.vis.state.options['debug'] && cpuTime !== undefined) {
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
					var cpu = Math.round(100 * this.frameCpu / delta);
					document.title = fps + ' fps @ ' + cpu + '% cpu';
					this.frameStart = this.lastTime;
					this.frameCounter = 0;
					this.frameCpu = 0;
				}
			}
			useMax = (this.cpu <= 0 || this.cpu > 1) || cpuTime === undefined;
		} else {
			useMax = true;
		}
		var that = this;
		if (useMax) {
			that.timeout = window.setTimeout(function() {
				that.loop(delay);
			}, 0);
		} else {
			newDelay = useMax ? 0 : Math.ceil(cpuTime / this.cpu - cpuTime);
			// looks odd, but synchronizes JS and rendering threads so we get
			// accurate CPU times
			this.timeout = window.setTimeout(function() {
				that.timeout = window.setTimeout(function() {
					that.loop(newDelay);
				}, newDelay);
			}, 0);
		}
	}
};

/**
 * The clean up method for the director resets the loop timeout and calls stop().
 * 
 * @see Visualizer#cleanUp
 */
Director.prototype.cleanUp = function() {
	window.clearTimeout(this.timeout);
	this.stop();
};

/**
 * Causes the visualizer to draw the current game state.
 * 
 * @param {Boolean}
 *        urgent When the director is currently playing back, the call would be a no-op and the
 *        redraw delayed to the next invocation of loop(). If this is not desired because - for
 *        example a resize occurred, this flag can be set to true.
 */
Director.prototype.draw = function(urgent) {
	if (this.playing()) {
		if (urgent) {
			window.clearTimeout(this.timeout);
			this.loop(0);
		}
	} else {
		this.vis.draw();
	}
};

/**
 * Adds 50% speed to the playback.
 */
Director.prototype.speedUp = function() {
	this.defaultSpeed *= 1.5;
	this.speed *= 1.5;
};

/**
 * The inverse of {@link Director#speedUp}.
 */
Director.prototype.slowDown = function() {
	this.defaultSpeed /= 1.5;
	this.speed /= 1.5;
};
