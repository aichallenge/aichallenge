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
 * @param {number} id the unique object id
 * @param {number} time the time in which the object appears in turn units
 */
function Ant(id, time) {
	this.id = id;
	this.lo = [ new LoFiData() ];
	this.lo[0].time = time;
	this.hi = [ new HiFiData() ];
	this.hi[0].time = time;
	this.player = undefined;
	/** @private */
	this.loLookup = [];
	/** @private */
	this.hiLookup = [];
}
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
	var i, delta;
	var set = quality ? this.hi : this.lo;
	var lookup = quality ? this.hiLookup : this.loLookup;
	var timeIdx = time | 0;
	var goFrom = lookup[timeIdx];
	if (goFrom === undefined) {
		for (i = set.length - 1; i >= 0; i--) {
			if (set[i].time <= timeIdx) {
				goFrom = i;
				lookup[timeIdx] = i;
			}
		}
		if (goFrom === undefined) {
			throw 'Can not interpolate before first key frame';
		}
	}
	while (goFrom < set.length - 1 && set[goFrom + 1].time <= time) {
		goFrom++;
	}
	if (goFrom == set.length - 1) {
		return set[goFrom].copy();
	}
	delta = (time - set[goFrom].time) / (set[goFrom + 1].time - set[goFrom].time);
	if (delta == 0) {
		return set[goFrom].copy();
	} else if (delta == 1) {
		return set[goFrom + 1].copy();
	}
	return set[goFrom].interpolate(set[goFrom + 1], delta);
};
Ant.prototype.fade = function(quality, key, pairs) {
	var i, k, av, bv, at, bt, mix;
	var set = quality ? this.hi : this.lo;
	var f = [];
	// create the required frames
	for (i = 0; i < pairs.length; i++) {
		f[i] = this.frameAt(pairs[i].time, quality, true);
		if (pairs[i].value !== undefined) {
			f[i][key] = pairs[i].value;
		}
	}
	// update frames inbetween
	for (i = set.length - 1; i >= 0; i--) {
		if (f[0].time === pairs[0].time) {
			break;
		}
	}
	i++;
	for (k = 0; k < f.length - 1; k++) {
		av = f[k][key];
		bv = f[k + 1][key];
		at = f[k].time;
		bt = f[k + 1].time;
		for (; i < set.length && set[i] != f[k + 1]; i++) {
			mix = (set[i].time - at) / (bt - at);
			set[i][key] = (1 - mix) * av + mix * bv;
		}
	}
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

function LoFiData(other) {
	this.time = other ? other.time : 0;
	this.x = other ? other.x : 0;
	this.y = other ? other.y : 0;
	this.r = other ? other.r : 0;
	this.g = other ? other.g : 0;
	this.b = other ? other.b : 0;
	this.a = other ? other.a : 0.0;
}
LoFiData.prototype.interpolate = function(other, b) {
	var a = 1.0 - b;
	var result = new LoFiData();
	result.time = a * this.time + b * other.time;
	result.x = a * this.x + b * other.x;
	result.y = a * this.y + b * other.y;
	result.r = (a * this.r + b * other.r) | 0;
	result.g = (a * this.g + b * other.g) | 0;
	result.b = (a * this.b + b * other.b) | 0;
	result.a = a * this.a + b * other.a;
	return result;
};
LoFiData.prototype.copy = function() {
	var result = new LoFiData();
	for (var i in this) {
		result[i] = this[i];
	}
	return result;
};

function HiFiData(other) {
	this.time = other ? other.time : 0;
	this.x = other ? other.x : 0;
	this.y = other ? other.y : 0;
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
