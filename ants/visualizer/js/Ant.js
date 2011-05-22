

/**
 * The enum for standard graphics is referred to by the imgA and imgB fields of
 * HiFiData and read in the drawing routine of the visualizer.
 */
Img = {
	EMPTY: 26,
	FOOD: 27
};

Quality = {
	LOW: false,
	HIGH: true
};



/**
 * The constructor for on-screen ant objects initializes it with one key frame.
 * @param {number} id This is the unique object id of the new ant.
 * @param {number} time Sets the time in which the object appears in turn units.
 * @constructor
 */
function Ant(id, time) {
	this.id = id;
	this.lo = [new LoFiData()];
	this.lo[0].time = time;
	this.hi = [new HiFiData()];
	this.hi[0].time = time;
	this.owner = undefined;
	/** @private */
	this.loInter = new LoFiData();
	this.loInter.id = id;
	/** @private */
	this.hiInter = new HiFiData();
	this.hiInter.id = id;
	/** @private */
	this.loLookup = [];
	/** @private */
	this.hiLookup = [];
}
/**
 * Returns a keyframe for the ant at the given time. If it is newly created and
 * inbetween two existing frames it will be the result of a linear interpolation
 * of those. If the time is beyond the last keyframe, the result is a copy of
 * the last keyframe. It is an error to specify a time before the first
 * keyframe.
 */
Ant.prototype.frameAt = function(time, quality, create) {
	var set = quality ? this.hi : this.lo;
	var frame;
	for (var i = set.length - 1; i >= 0; i--) {
		if (set[i].time == time) {
			return set[i];
		} else if (set[i].time < time) {
			if (create) {
				frame = quality ? new HiFiData(set[i]) : new LoFiData(set[i]);
				frame.time = time;
				set.splice(i + 1, 0, frame);
				return frame;
			}
			break;
		}
	}
	return null;
};
/**
 * Interpolates the key frames around the given time and returns the result. If
 * the time exceeds the time stamp of the last key frame, that key frame is
 * returned instead.
 * @param {number} time the time in question
 * @param {Quality} quality selects between the low and high quality set of key
 *     frames
 */
Ant.prototype.interpolate = function(time, quality) {
	var i, min, max, delta, set, result, lookup, lastFrame, timeIdx, goFrom;
	if (quality) {
		set = this.hi;
		result = this.hiInter;
		lookup = this.hiLookup;
	} else {
		set = this.lo;
		result = this.loInter;
		lookup = this.loLookup;
	}
	if (time < set[0].time) return null;
	lastFrame = set[set.length - 1];
	if (time >= lastFrame.time) return result.assign(lastFrame);
	timeIdx = time | 0;
	goFrom = lookup[timeIdx];
	if (goFrom === undefined) {
		if (timeIdx < set[0].time) {
			goFrom = 0;
		} else {
			min = 0;
			max = set.length - 1;
			do {
				i = (min + max >> 1);
				if (timeIdx < set[i].time) {
					max = i;
				} else if (timeIdx > set[i + 1].time) {
					min = i;
				} else {
					goFrom = i;
					lookup[timeIdx] = i;
				}
			} while (goFrom === undefined);
		}
		lookup[timeIdx] = goFrom;
	}
	while (time > set[goFrom + 1].time) goFrom++;
	delta = (time - set[goFrom].time) / (set[goFrom + 1].time - set[goFrom].time);
	return result.interpolate(set[goFrom], set[goFrom + 1], delta);
};
Ant.prototype.fade = function(quality, key, valueb, timea, timeb) {
	var i, valuea, mix, f0, f1;
	var set = quality ? this.hi : this.lo;
	// create and adjust the start and end frames
	f0 = this.frameAt(timea, quality, true);
	f1 = this.frameAt(timeb, quality, true);
	// update frames inbetween
	for (i = set.length - 1; i >= 0; i--) {
		if (set[i].time === timea) {
			break;
		}
	}
	valuea = f0[key];
	for (i++; set[i] !== f1; i++) {
		mix = (set[i].time - timea) / (timeb - timea);
		set[i][key] = (1 - mix) * valuea + mix * valueb;
	}
	for (; i < set.length; i++) set[i][key] = valueb;
};
// animates the ant ([{<time between 0 and 1>, {<attribute to set absolute>, ...}, {<attribute to set relative>, ...}}, ...])
// currently unused
Ant.prototype.animate = function(list) {
	var key, a, i;
	var interpol = new Array(list.length);
	for (i = 0; i < list.length; i++) {
		var time = this.keyFrames[0].time + list[i].time;
		interpol[i] = this.interpolate(time);
	}
	for (i = 0; i < list.length; i++) {
		for (a = 0; a < this.keyFrames.length; a++) {
			if (this.keyFrames[a].time > time) {
				this.keyFrames.splice(a, 0, interpol[i]);
				break;
			}
		}
		for (key in list[i].absolute) {
			interpol[i][key] = list[i].absolute[key];
		}
		for (key in list[i].relative) {
			interpol[i][key] += list[i].relative[key];
		}
	}
};

/**
 * @constructor
 */
function LoFiData(other) {
	if (other) {
		this.assign(other);
	} else {
		this.time = 0.0;
		this['x'] = 0.0;
		this['y'] = 0.0;
		this['r'] = 0;
		this['g'] = 0;
		this['b'] = 0;
		this['size'] = 0.0;
		this['owner'] = undefined;
	}
}
LoFiData.prototype.interpolate = function(a, b, useb) {
	var usea = 1.0 - useb;
	this.time = usea * a.time + useb * b.time;
	this['x'] = usea * a['x'] + useb * b['x'];
	this['y'] = usea * a['y'] + useb * b['y'];
	this['r'] = (usea * a['r'] + useb * b['r']) | 0;
	this['g'] = (usea * a['g'] + useb * b['g']) | 0;
	this['b'] = (usea * a['b'] + useb * b['b']) | 0;
	this['size'] = usea * a['size'] + useb * b['size'];
	this['owner'] = a['owner'];
	return this;
};
LoFiData.prototype.assign = function(other) {
	this.time = other.time;
	this['x'] = other['x'];
	this['y'] = other['y'];
	this['r'] = other['r'];
	this['g'] = other['g'];
	this['b'] = other['b'];
	this['size'] = other['size'];
	this['owner'] = other['owner'];
	return this;
};

/**
 * @constructor
 */
function HiFiData(other) {
	this.time = other ? other.time : 0;
	this['x'] = other ? other['x'] : 0;
	this['y'] = other ? other['y'] : 0;
	this.angle = other ? other.angle : Math.random() * 2 * Math.PI;
	this.imgA = other ? other.imgA : Img.EMPTY;
	this.imgB = other ? other.imgB : Img.FOOD;
	this.transition = other ? other.transition : 0;
}
HiFiData.prototype.interpolate = function(other, b) {
	var a = 1.0 - b;
	var result = new HiFiData();
	result.time = a * this.time + b * other.time;
	return result;
};
