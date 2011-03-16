/**
 * @constructor
 */
function AppletManager() {
	/**
	 * @private
	 */
	this.callbacks = {};
	/**
	 * @private
	 */
	this.tokenId = 0;
}
AppletManager.prototype.add = function(user) {
	this.callbacks[this.tokenId] = user;
	return this.tokenId++;
};
AppletManager.prototype.appletInitialized = function(token) {
	var vis = this.callbacks[token];
	if (vis) {
		delete this.callbacks[token];
		vis.initializedApplet(token);
	} else {
		alert('no applet with token: ' + token + ' in list');
	}
};
/**
 * Opera has a lose understanding of JavaScript's single-threadedness. A
 * call from an applet halts the currently executing code, executes the 
 * code called by the applet, then resumes with the previous operation.
 * Since this results in race conditions I defer the applet's call until
 * any active operation is completed. The next issue is Safari, which
 * has trouble decoding and Object[] which contains
 * The arguments will be passed using
 * func.apply(thisArg, argArray)
 * @param {Object} thisArg the object the function schould be called on;
 *        will be 'this' in the function
 * @param {String} func the function to be called
 * @param varArgs the applet will append any number of arguments here;
 *     they will be retrieved through the arguments variable
 */
AppletManager.prototype.callAfterExecutionUnit = function(thisArg, func, varArgs) {
	var jsArray = new Array(arguments.length - 2);
	for (var i = 0; i < jsArray.length; i++) {
		jsArray[i] = arguments[i + 2];
	}
	window.setTimeout(function() {
		thisArg[func].apply(thisArg, jsArray);
	}, 0);
};

appletManager = new AppletManager();

// make some exported functions known to Closure Compiler
AppletManager.prototype['callAfterExecutionUnit'] = AppletManager.prototype.callAfterExecutionUnit;
AppletManager.prototype['appletInitialized'] = AppletManager.prototype.appletInitialized;