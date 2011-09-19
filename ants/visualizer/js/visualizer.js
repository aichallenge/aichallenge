/**
 * @fileOverview This file acts as the central import point for the other JavaScript files that make
 *               up the visualizer.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

$import('Util');
$import('Ant');
$import('Application');
$import('Buttons');
$import('Config');
$import('Const');
$import('Director');
$import('ImageManager');
$import('Replay');
$import('CanvasElement');

var $import_base;

/**
 * Imports a file in Java package notation.
 * 
 * @param {String}
 *        file the 'package' name
 */
function $import(file) {
	var ends_with = function(str, pat) {
		return str.slice(-pat.length) == pat;
	};
	var scripts = document.getElementsByTagName("script");
	if ($import_base === undefined) {
		for ( var i = 0, len = scripts.length; i < len; ++i) {
			if (ends_with(scripts[i].src, 'visualizer.js')) {
				var pathLen = scripts[i].src.lastIndexOf('/') + 1;
				$import_base = scripts[i].src.substr(0, pathLen);
				break;
			}
		}
	}
	file = $import_base + file.replace(/[.]/g, '/') + '.js';
	for ( var i = 0; i < scripts.length; i++) {
		if (scripts[i].src === file) {
			return;
		}
	}
	document.write('<script src="' + file + '"></script>');
}
