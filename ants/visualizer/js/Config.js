/**
 * @fileOverview This file is for the persistent configuration of the visualizer.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * @class This class keeps track of persistent configuration values. The values are stored in
 *        browser "local storage" under "visualizer.&lt;key&gt;" identifiers.
 * @constructor
 * @property {Boolean} fullscreen If set, the visualizer uses the whole browser window to display,
 *           replacing any other content. Default: false
 * @property {Number} label Can take the values 0 = draw no label on ants, 1 = draw letters on ants,
 *           2 = draw the ant id (occurrence in replay file). Default: 0
 * @property {Number} zoom Sets the zoom factor. The actual size of map squares depends on the
 *           visualizer size. Default: 1
 * @property {Number} duration How long a replay should normally play, in seconds. The speed is
 *           adapted to that. Default: 75
 * @property {Number} speedSlowest Overrides the duration in case the replay is so short that the
 *           playback rate would be below this turns per second. Default: 2
 * @property {Number} speedFastest Overrides the duration in case the replay is so long that the
 *           playback rate would be over this turns per second. Default: 8
 * @property {Number} speedFactor An additional speed factor that is applied (logarithmically) to
 *           the result of the calculation and can be modified with the speed buttons in the
 *           visualizer. Default: 0
 * @property {Number} cpu How much CPU juice the visualizer should consume during regular playback.
 *           After each rendered frame, pauses will be inserted to shape an average CPU utilization.
 *           It is far from exact, but serves the purpose to keep the CPU from maxing out and people
 *           from complaining. Default: 0.5
 */
function Config() {
	// user settings
	this.load();
}
Config.prototype['fullscreen'] = false;
Config.prototype['label'] = 0;
Config.prototype['zoom'] = 1;
Config.prototype['duration'] = 75;
Config.prototype['speedSlowest'] = 2;
Config.prototype['speedFastest'] = 8;
Config.prototype['speedFactor'] = 0;
Config.prototype['cpu'] = 0.5;

/**
 * Loads all keys from a generic object into the configuration.
 * 
 * @param {Object}
 *        preset the object containing configuration values
 */
Config.prototype.overrideFrom = function(preset) {
	var key = undefined;
	for (key in preset) {
		if (!Config.prototype.hasOwnProperty(key) || (typeof this[key] === 'function'))
			throw new Error("Cannot override '" + key
					+ "', because it is not a known configuration value.");
		this[key] = preset[key];
	}
};

/**
 * Checks if browser "local storage" is available.
 * 
 * @returns {Boolean} True, if the browser supports local storage.
 */
Config.prototype.hasLocalStorage = function() {
	try {
		return !!window.localStorage;
	} catch (error) {
		return false;
	}
};

/**
 * Stores all keys to local storage (if supported).
 * 
 * @throws {Error}
 *         Throws an error, if something different than a string, number or boolean is stored.
 * @return {Boolean} True, if successful.
 */
Config.prototype.save = function() {
	if (this.hasLocalStorage() === true) {
		for ( var key in this) {
			if (this[key] === undefined || this[key] === Config.prototype[key]) {
				delete window.localStorage['visualizer.' + key];
			} else if (this[key] === null) {
				window.localStorage['visualizer.' + key] = "null";
			} else {
				var prefix = undefined;
				switch (typeof this[key]) {
				case 'number':
					prefix = 'N';
					break;
				case 'boolean':
					prefix = 'B';
					break;
				case 'string':
					prefix = 'S';
					break;
				case 'function':
					break;
				default:
					throw new Error('Only numbers, booleans and strings can be saved in the config');
				}
				if (prefix) {
					window.localStorage['visualizer.' + key] = prefix + this[key];
				}
			}
		}
		return true;
	}
	return false;
};

/**
 * Tries to load configuration values from local storage if available.
 * 
 * @returns {Boolean} True, if successful.
 */
Config.prototype.load = function() {
	if (this.hasLocalStorage() === true) {
		for ( var key in this) {
			if (typeof this[key] != 'function') {
				var val = window.localStorage['visualizer.' + key];
				if (val === 'null') {
					this[key] = null;
				} else if (val) {
					switch (val.charAt(0)) {
					case 'N':
						this[key] = Number(val.substr(1));
						break;
					case 'B':
						this[key] = (val == 'Btrue');
						break;
					case 'S':
						this[key] = val.substr(1);
						break;
					}
				}
			}
		}
		return true;
	}
	return false;
};

/**
 * Deletes all local storage items for the web site.
 */
Config.prototype.clear = function() {
	if (this.hasLocalStorage() === true) {
		window.localStorage.clear();
	}
};
