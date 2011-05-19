$import('Ant');
$import('AppletManager');
$import('Application');
$import('Buttons');
$import('Config');
$import('Const');
$import('Director');
$import('ImageManager');
$import('Util');
$import('Replay');

var $import_base;

/**
 * Imports a file in Java package notation.
 * @param {String} file the 'package' name
 */
function $import(file) {
    var scripts = document.getElementsByTagName("script");
    if ($import_base === undefined) {
        var pathLen = scripts[0].src.lastIndexOf('/') + 1;
        $import_base = scripts[0].src.substr(0, pathLen);
    }
    file = $import_base + file.replace(/[.]/g, '/') + '.js'
    for (var i = 0; i < scripts.length; i++) {
        if (scripts[i].src === file) {
            return;
        }
    }
    document.write('<script src="' + file + '"></script>');
}
