/**
 * keeps track of persistent configuration values
 * @constructor
 */
function Config(preset) {
	// user settings
	this.load();
	// override
	if (preset) for (var key in preset) {
		this[key] = preset[key];
	}
}
Config.prototype['fullscreen'] = false;
Config.prototype['label'] = 0;
Config.prototype['graphics'] = false;
Config.prototype['zoom'] = 1;
Config.prototype['duration'] = 100;
Config.prototype['speedSlowest'] = 2;
Config.prototype['speedFastest'] = 5;
Config.prototype['speedFactor'] = 0;
Config.prototype['cpu'] = 0.5;
/**
 * checks if local storage is available
 */
Config.prototype.hasLocalStorage = function() {
	try {
		return !!window.localStorage;
	} catch (error) {
		return error;
	}
};
/**
 * stores all keys to local storage if supported
 * @return true, if successful, an error if not
 */
Config.prototype.save = function() {
	if (this.hasLocalStorage() === true) {
		for (var key in this) {
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
						throw 'Only numbers, booleans and strings can be saved in the config';
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
 * Tries to load config values from local storage if available. Registred
 * functions on value change will fire.
 */
Config.prototype.load = function() {
	if (this.hasLocalStorage() === true) {
		for (var key in this) {
			var val = window.localStorage['visualizer.' + key];
			if (typeof this[key] != 'function' && val) {
				if (val === 'null') {
					this[key] = null;
				} else {
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
Config.prototype.clear = function() {
	if (this.hasLocalStorage() === true) {
		window.localStorage.clear();
	}
};
