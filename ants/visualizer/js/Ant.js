/**
 * @fileoverview This file contains an ant and it's animation key frames as used by the
 *               visualization.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * The constructor initializes it with one key frame.
 * 
 * @class An ant in the visualization. It is used in rendering and created when the replay is parsed
 *        as well as extended with animation key frames as more turns are requested. It has turned
 *        out that pre-calculating all key frames for thousand ants over the course of thousand
 *        turns is not feasible. Key frames are practically always appended. There is no support for
 *        jumping to the end of the game and leaving gaps in-between.
 * @constructor
 * @param {Number}
 *        id This is the unique object id of the new ant.
 * @param {Number}
 *        time Sets the time in which the object appears in turn units.
 * @constructor
 */
function Ant(id, time) {
	this.id = id;
	this.death = undefined;
	this.keyFrames = [ new KeyFrame() ];
	this.keyFrames[0].time = time;
	this.owner = undefined;
	/** @private */
	this.keyFrameCache = new KeyFrameEx(id, this.keyFrames[0]);
	/** @private */
	this.lookup = [];
}

/**
 * Returns a key frame for the ant at the given time. If it has to be newly created and is in
 * between two existing frames it will be the result of a linear interpolation of those. If the time
 * is beyond the last key frame, the result is a copy of the last key frame. It is an error to
 * specify a time before the first key frame.
 * 
 * @param {Number}
 *        time the time in question
 * @returns {KeyFrame} a key frame for the time or null, if the time is before the first key frame
 */
Ant.prototype.frameAt = function(time) {
	var frame;
	var set = this.keyFrames;
	for ( var i = set.length - 1; i >= 0; i--) {
		if (set[i].time == time) {
			return set[i];
		} else if (set[i].time < time) {
			frame = new KeyFrame();
			if (i === set.length - 1) {
				frame.assign(set[i]);
			} else {
				frame.interpolate(set[i], set[i + 1], time);
			}
			frame.time = time;
			set.splice(i + 1, 0, frame);
			return frame;
		}
	}
	return null;
};

/**
 * Interpolates the key frames around the given time and returns the result. If the time exceeds the
 * time stamp of the last key frame, that key frame is returned instead. If the ant doesn't exist
 * yet at that time, null is returned.
 * 
 * @param {Number}
 *        time the time in question
 * @returns {KeyFrameEx} the interpolated key frame
 */
Ant.prototype.interpolate = function(time) {
	var i, min, max, lastFrame, timeIdx, goFrom;
	var set = this.keyFrames;

	// no key frame for times before the first key frame
	if (time < set[0].time) return null;

	// times beyond the last key frame always return the latter
	lastFrame = set[set.length - 1];
	if (time >= lastFrame.time) {
		this.keyFrameCache.assign(lastFrame);
	} else {
		timeIdx = time | 0;
		goFrom = this.lookup[timeIdx];
		if (goFrom === undefined) {
			if (timeIdx < set[0].time) {
				goFrom = 0;
			} else {
				min = 0;
				max = set.length - 1;
				do {
					i = (min + max) >> 1;
					if (timeIdx < set[i].time) {
						max = i;
					} else if (timeIdx > set[i + 1].time) {
						min = i;
					} else {
						goFrom = i;
						this.lookup[timeIdx] = i;
					}
				} while (goFrom === undefined);
			}
			this.lookup[timeIdx] = goFrom;
		}
		while (time > set[goFrom + 1].time)
			goFrom++;
		this.keyFrameCache.interpolate(set[goFrom], set[goFrom + 1], time);
	}
	return this.keyFrameCache;
};

/**
 * Creates an fade over of some attribute given the start time and the end time with a target value.
 * The attribute stays unchanged at the start time. This method is used by the {@link Replay} to
 * create animation effects.
 * 
 * @param {String}
 *        key attribute name
 * @param {Number}
 *        valueb target value
 * @param {Number}
 *        timea start time
 * @param {Number}
 *        timeb end time
 */
Ant.prototype.fade = function(key, valueb, timea, timeb) {
	var i, valuea, mix, f0, f1;
	var set = this.keyFrames;
	// create and adjust the start and end frames
	f0 = this.frameAt(timea);
	f1 = this.frameAt(timeb);
	// update frames in between
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
	for (; i < set.length; i++)
		set[i][key] = valueb;
};

/**
 * The constructor is only called from within methods of {@link Ant} that add key frames.
 * 
 * @class A single animation key frame of an ant.
 * @constructor
 */
function KeyFrame() {
	this.time = 0.0;
	this['x'] = 0.0;
	this['y'] = 0.0;
	this['r'] = 0;
	this['g'] = 0;
	this['b'] = 0;
	this['size'] = 0.0;
	this['owner'] = undefined;
}

/**
 * Assigns the interpolation of two other key frames at a given time to this key frame. This method
 * is used by {@link Ant}.
 * 
 * @param {KeyFrame}
 *        a first key frame
 * @param {KeyFrame}
 *        b second key frame
 * @param {Number}
 *        time the time, which should be between a and b
 * @returns {KeyFrame} this object
 */
KeyFrame.prototype.interpolate = function(a, b, time) {
	var useb = (time - a.time) / (b.time - a.time);
	var usea = 1.0 - useb;
	this.time = usea * a.time + useb * b.time;
	this['x'] = (a['x'] === b['x']) ? a['x'] : usea * a['x'] + useb * b['x'];
	this['y'] = (a['y'] === b['y']) ? a['y'] : usea * a['y'] + useb * b['y'];
	this['r'] = (usea * a['r'] + useb * b['r']) | 0;
	this['g'] = (usea * a['g'] + useb * b['g']) | 0;
	this['b'] = (usea * a['b'] + useb * b['b']) | 0;
	this['size'] = usea * a['size'] + useb * b['size'];
	this['owner'] = a['owner'];
	return this;
};

/**
 * Assigns the values of another key frame object to this one.
 * 
 * @param {KeyFrame}
 *        other the other key frame
 * @returns {KeyFrame} this object
 */
KeyFrame.prototype.assign = function(other) {
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
 * @class An extended key frame which holds the owning ant's id and an additional coordinate pair
 *        that reflects the pixel position on the scaled map.
 * @extends KeyFrame
 * @constructor
 * @param {Number}
 *        antId the owning ant's id
 * @param {KeyFrame}
 *        keyFrame the key frame to copy from
 */
function KeyFrameEx(antId, keyFrame) {
	this.antId = antId;
	this.assign(keyFrame);
	this.mapX = this['x'];
	this.mapY = this['y'];
}
KeyFrameEx.extend(KeyFrame);

/**
 * Updates the map coordinates.
 * 
 * @param scale
 *        pixel size of an ant on the map
 * @param mapWidth
 *        pixel width of the map
 * @param mapHeight
 *        pixel height of the map
 */
KeyFrameEx.prototype.calcMapCoords = function(scale, mapWidth, mapHeight) {
	this.mapX = Math.round(scale * this['x']) + scale - 1;
	this.mapY = Math.round(scale * this['y']) + scale - 1;
	// correct coordinates
	this.mapX = Math.wrapAround(this.mapX, mapWidth) - scale + 1;
	this.mapY = Math.wrapAround(this.mapY, mapHeight) - scale + 1;
};
