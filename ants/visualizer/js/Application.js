/**
 * @fileoverview This is a visualizer for the ant game.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/*
 * @todo FEAT: keyboard +/- speed setting
 * @todo FEAT: info button showing a message box with game meta data
 * @todo FEAT: menu items: toggle graph/score bars, cpu use
 * @todo FEAT: setting for cpu usage
 * @todo NICE: better player rank display
 * @todo COSMETIC: switch to console.log for debug and load messages
 * @todo COSMETIC: fix duplicate 'parsing replay...' messages
 */

LoadingState = {
	IDLE: 0,
	LOADING: 1,
	CLEANUP: 2
};

Key = {
	LEFT: 37,
	RIGHT: 39,
	SPACE: 32,
	PGUP: 33,
	PGDOWN: 34,
	HOME: 36,
	END: 35,
	PLUS: 187,
	MINUS: 189
};

/**
 * @constructor
 */
function Location(x, y, w, h) {
	this.x = x;
	this.y = y;
	this.w = w;
	this.h = h;
}
Location.prototype.offX = function() {
	return this.x + this.w;
};
Location.prototype.offY = function() {
	return this.y + this.h;
};
Location.prototype.contains = function(x, y) {
	return (x >= this.x && x < this.x + this.w
		&& y >= this.y && y < this.y + this.h);
};

/**
 * @class The main 'application' object that provides all necessary methods for
 *     the use in a web page.
 * @constructor
 * @param {Node} container the html element, that the visualizer will embed into
 * @param {String} dataDir This relative path to the visualizer data files. You
 *     will get an error message if you forget the tailing '/'.
 * @param {Boolean} interactive optional, if true or omitted, then the
 *     visualizer is interactive
 * @param {Number} w an optional maximum width or undefined
 * @param {Number} h an optional maximum height or undefined
 * @param {Object} config an optional configuration; each field overrides the
 *     respective value in the user's configuration or the default
 */
Visualizer = function(container, dataDir, interactive, w, h, config) {
	/**
	 * any generated DOM elements will be placed here
	 * @private
	 */
	this.container = container;
	/**
	 * contains the backdrop for the map
	 * @private
	 */
	this.map = {};
	/**
	 * Caches the graphics of the map border
	 * @private
	 */
	this.border = {};
	/**
	 * Caches overlay graphics like fog
	 * @private
	 */
	this.overlay = {};
	/**
	 * Caches the score graph
	 * @private
	 */
	this.scores = {};
	/**
	 * array of precomputed turn data
	 * @private
	 */
	this.turns = undefined;
	/**
	 * usable width for the visualizer
	 * @private
	 */
	this.w = w;
	/**
	 * usable height for the visualizer
	 * @private
	 */
	this.h = h;
	/**
	 * locations of elements on the screen
	 * @private
	 */
	this.loc = {};
	/**
	 * size of an ant in pixels
	 * @private
	 */
	this.scale = undefined;
	/**
	 * Options from URL GET parameters or the constructor arguments
	 * @private
	 */
	this.options = {};
	this.options['data_dir'] = dataDir;
	this.options['interactive'] = !(interactive === false);
	// read URL parameters and store them in the parameters object
	var equalPos, value, key, i;
	var parameters = window.location.href;
	if ((i = parameters.indexOf('?')) !== -1) {
		parameters = parameters.substr(i + 1).split('#')[0].split('&');
		for (i = 0; i < parameters.length; i++) {
			equalPos = parameters[i].indexOf('=');
			key = parameters[i].substr(0, equalPos);
			value = parameters[i].substr(equalPos + 1);
			if (key === 'debug' || key === 'profile' || key === 'interactive') {
				value = value == 'true' || value == '1';
			} else if (key === 'row' || key === 'col' || key === 'turn') {
				value = parseInt(value);
				if (!(value >= 0)) value = 0;
			} else if (key === 'config') {
				config = JSON.parse(unescape(value));
			}
			this.options[key] = value;
		}
	}
	// set default zoom to max if we are going to zoom in on a square
	if (this.options['row'] !== undefined && this.options['col'] !== undefined) {
		if (!config) config = {};
		config['zoom'] = 1 << Math.ceil(Math.log(ZOOM_SCALE) / Math.LN2);
	}
	/**
	 * presistable configuration values
	 * @private
	 */
	this.config = new Config(config);
	/**
	 * manages playback commands and timing
	 * @private
	 */
	this.director = new Director(this);
	/**
	 * @private
	 */
	this.mouseX = -1;
	/**
	 * @private
	 */
	this.mouseY = -1;
	/**
	 * @private
	 */
	this.mouseDown = 0;
	/**
	 * @private
	 */
	this.mouseOverVis = false;
	/**
	 * @private
	 */
	this.shiftX = 0;
	/**
	 * @private
	 */
	this.shiftY = 0;
	/**
	 * @private
	 */
	this.mapCenterX = 0;
	/**
	 * @private
	 */
	this.mapCenterY = 0;
	/**
	 * buttons
	 * @private
	 */
	this.btnMgr = new ButtonManager(this);
	/**
	 * @private
	 */
	this.log = document.createElement('div');
	var text = 'Loading visualizer...';
	text += '<table>';
	for (key in this.options) {
		value = this.options[key];
		text += '<tr><td>-&nbsp;</td><td>' + key + '&nbsp;&nbsp;</td><td><b>' + value + '&nbsp;&nbsp;</b></td><td><i>';
		if (key == "data_dir") {
			text += '(Image directory)';
		}
		text += '</i></td></tr>';
	}
	text += '</table>';
	while (this.container.hasChildNodes()) {
		this.container.removeChild(this.container.lastChild);
	}
	this.log.innerHTML = text;
	this.container.appendChild(this.log);
	/**
	 * images used by the visualizer
	 * @private
	 */
	this.imgMgr = new ImageManager((dataDir || '') + 'img/', this,
			this.completedImages);
	this.imgMgr.add('water.png');
	this.imgMgr.add('mud.jpg')
	this.imgMgr.add('ant.png')
	this.imgMgr.add('playback.png');
	this.imgMgr.add('fog.png');
	this.imgMgr.add('toolbar.png');
	this.imgMgr.add('food.png');
	this.imgMgr.add('rank.png');
	this.imgMgr.add('graph_options.png');
	/**
	 * the highest player count in a previous replay to avoid button repaints
	 * @private
	 */
	this.colorizedPlayerCount = 0;
	// state information that must be reset on error/reload
	/**
	 * @private
	 */
	this.replay = undefined;
	/**
	 * the main canvas
	 * @private
	 */
	this.main = {};
	/**
	 * the minimap canvas
	 * @private
	 */
	this.minimap = {};
	/**
	 * a hint text overlay
	 * @private
	 */
	this.hint = '';
	/**
	 * @private
	 */
	this.fog = undefined;
	/**
	 * @private
	 */
	this.isStreaming = false;
	this.loading = LoadingState.IDLE;
	this.imgMgr.startRequests();
}
/**
 * @private
 */
Visualizer.prototype.progress = function(log, func) {
	if (this.loading !== LoadingState.LOADING) return;
	var vis = this;
	if (log) this.logOut(log);
	window.setTimeout(function() {
		try {
			func();
		} catch (error) {
			// (for Firefox Java errors:) if error is just a string, wrap it into an object
			if (typeof error == 'string') error = {message: error};
			var msg = '';
			for(var key in error) {
				var escaped = new String(error[key]).replace('&', '&amp;');
				escaped = escaped.replace('<', '&lt;').replace('>', '&gt;');
				msg += '<p><u><b>Error ' + key + ':</b></u>\n' + escaped + '</p>';
			}
			vis.errorOut(msg);
			var selectedPosX = 0;
			var selectedPosY = 0;
			var obj = vis.log;
			if (vis.log.offsetParent) do {
				selectedPosX += obj.offsetLeft;
				selectedPosY += obj.offsetTop;
			} while ((obj = obj.offsetParent));
			window.scrollTo(selectedPosX, selectedPosY);
		}
	}, 50);
};
/**
 * Places a paragraph with a message in the visualizer dom element.
 * @param {string} text the message text
 * @private
 */
Visualizer.prototype.logOut = function(text) {
	text = text.replace(/\n/g, '<br>');
	this.log.innerHTML += text + '<br>';
};
/**
 * Stops loading, cleans up the instance and calls logOut with the text in red.
 * @param {string} text the error message text
 * @private
 */
Visualizer.prototype.errorOut = function(text) {
	this.logOut('<font color="red">' + text + '</font>');
	this.cleanUp();
};
/**
 * @private
 */
Visualizer.prototype.cleanUp = function() {
	this.loading = LoadingState.CLEANUP;
	this.imgMgr.cleanUp();
	this.director.cleanUp();
	if (this.replay && this.replay instanceof XMLHttpRequest) this.replay.abort();
	this.replay = undefined;
	if (this.main.canvas) {
		if (this.container.firstChild === this.main.canvas) {
			this.container.removeChild(this.main.canvas);
		}
	}
	this.fog = undefined;
	this.isStreaming = false;
	document.onkeydown = null;
	document.onkeyup = null;
	document.onkeypress = null;
	window.onresize = null;
	this.log.style.display = 'block';
};
Visualizer.prototype.preload = function() {
	if (this.loading !== LoadingState.IDLE) return true;
	this.log.innerHTML = '';
	this.cleanUp();
	this.loading = LoadingState.LOADING;
	return false;
};
/**
 * Loads a replay file located on the same server using a XMLHttpRequest.
 * @param {string} file the relative file name
 */
Visualizer.prototype.loadReplayDataFromURI = function(file) {
	if (this.preload()) return;
	var vis = this;
	this.progress('Fetching replay from: <i>' + file + '</i>...', function() {
		vis.replay = new XMLHttpRequest();
		vis.replay.onreadystatechange = function() {
			if (vis.replay.readyState === 4) {
				if (vis.loading === LoadingState.LOADING) {
					if (vis.replay.status === 200) {
						vis.replay = '' + vis.replay.responseText;
						vis.loadParseReplay();
					} else {
						vis.errorOut('Status ' + vis.replay.status + ': ' + vis.replay.statusText);
					}
				}
			}
		};
		vis.replay.open("GET", file);
		if (vis.options['debug']) {
			vis.replay.setRequestHeader('Cache-Control', 'no-cache');
		}
		vis.replay.send();
		vis.loadCanvas(true);
	});
};
/**
 * Loads a replay string directly.
 * @param {string} data the replay string
 */
Visualizer.prototype.loadReplayData = function(data) {
	if (this.preload()) return;
	this.replay = data;
	this.loadCanvas(true);
};
Visualizer.prototype.streamingInit = function() {
	this.preload();
	this.isStreaming = true;
	return this.replay = new Replay();
};
Visualizer.prototype.streamingStart = function() {
	this.isStreaming = stream.visualizerReady();
	if (this.loading === LoadingState.LOADING) {
		if (this.replay.duration > 0) {
			// set cpu to 100%, we need it
			this.director.cpu = 1;
			this.loadCanvas(true);
		}
	} else {
		// call resize to update the gui
		this.resize(true);
		// resume playback if we are at the end
		resume = !this.director.playing() && (this.director.position === this.director.duration);
		if (this.director.stopAt === this.director.duration) {
			this.director.stopAt = this.replay.duration;
		}
		this.director.duration = this.replay.duration;
		if (resume) {
			this.director.play();
		}
	}
};
/**
 * @private
 */
Visualizer.prototype.loadParseReplay = function() {
	var vis = this;
	this.progress('Parsing the replay...', function() {
		if (!vis.replay) {
			if (vis.loading !== LoadingState.CLEANUP) {
				throw new Error('Replay is undefined.');
			}
		} else if (vis.replay instanceof Replay) { // has just been parsed
			return;
		} else if (typeof vis.replay == 'string') { // string only
			vis.replay = new Replay(vis.replay, vis.options['debug'], vis.options['user']);
		} else if (vis.replay instanceof XMLHttpRequest) { // wait for the reply
			return;
		} else {
			throw new Error('Something unknown is in the replay variable: ' + vis.replay);
		}
		vis.tryStart();
	});
};
/**
 * Creates a canvas element
 * @private
 */
Visualizer.prototype.loadCanvas = function(prompt) {
	var vis = this;
	this.progress(prompt ? 'Creating canvas...' : undefined, function() {
		if (!vis.main.canvas) {
			vis.main.canvas = document.createElement('canvas');
		}
		var c = vis.main.canvas;
		vis.main.ctx = c.getContext('2d');
		if (vis.container.firstChild !== c) {
			vis.container.insertBefore(c, vis.log);
		}
		vis.map = new CanvasElement(vis, vis.renderMap);
		vis.createCanvas(vis.border);
		vis.createCanvas(vis.overlay);
		vis.createCanvas(vis.scores);
		vis.createCanvas(vis.minimap);
		vis.tryStart();
	});
};
/**
 * Called by the ImageManager when no more images are loading
 */
Visualizer.prototype.completedImages = function(error) {
	if (error) {
		this.errorOut(error);
	} else {
		this.tryStart();
	}
};
/**
 * Checks if we have a drawing context (canvas/applet), the images and the
 * replay. If all components are loaded it starts playback.
 */
Visualizer.prototype.tryStart = function() {
	var bg, stop;
	var vis = this;
	if (this.replay === undefined) return;
	// we need to parse the replay, unless it has been parsed by the
	// XmlHttpRequest callback
	if (this.replay instanceof Replay) {
		if (this.main.ctx && !this.imgMgr.error && !this.imgMgr.pending) {
			vis = this;
			if (this.options['interactive']) {
				// add static buttons
				if (!vis.btnMgr.groups['playback']) {
					if (this.replay.hasDuration) {
						bg = vis.btnMgr.addImageGroup('playback',
								vis.imgMgr.images[3], ImageButtonGroup.HORIZONTAL,
								ButtonGroup.MODE_NORMAL, 2, 0);
						bg.addButton(3, function() {
							vis.director.gotoTick(0)
						}, 'jump to start of first turn');
						bg.addSpace(32);
						bg.addButton(5, function() {
							stop = (Math.ceil(vis.director.position * 2) - 1) / 2;
							vis.director.slowmoTo(stop);
						}, 'play one move/attack phase backwards');
						//bg.addButton(0, function() {vis.director.playStop()});
						bg.addSpace(64);
						bg.addButton(4, function() {
							vis.director.playStop()
						}, 'play/stop the game');
						//drawImage(this.imgMgr.images[1], 1 * 64, 0, 64, 64, x + 4.5 * 64, y, 64, 64);
						bg.addSpace(64);
						bg.addButton(6, function() {
							var stop = (Math.floor(vis.director.position * 2) + 1) / 2;
							vis.director.slowmoTo(stop);
						}, 'play one move/attack phase');
						bg.addSpace(32);
						bg.addButton(2, function() {
							vis.director.gotoTick(vis.director.duration);
						}, 'jump to end of last turn');
					}
					bg = vis.btnMgr.addImageGroup('toolbar', vis.imgMgr.images[5],
							ImageButtonGroup.VERTICAL, ButtonGroup.MODE_NORMAL, 2, 0);
					if (this.config.hasLocalStorage()) {
						bg.addButton(0, function() {
							vis.config.save()
						}, 'save and reuse the current settings');
					}
					if (!window.isFullscreenSupported || window.isFullscreenSupported()) {
						bg.addButton(1, function() {
							vis.setFullscreen(!vis.config['fullscreen']);
						}, 'toggle fullscreen mode');
					}
					bg.addButton(2, function() {
						vis.setZoom(2 * vis.config['zoom']);
						vis.director.draw();
					}, 'zoom in');
					bg.addButton(3, function() {
						var oldScale = vis.scale;
						do {
							vis.setZoom(0.5 * vis.config['zoom']);
						} while (vis.scale == oldScale && vis.config['zoom'] > 1);
						vis.director.draw();
					}, 'zoom out');
					bg.addButton(4, function() {
						vis.shiftX = vis.mapCenterX;
						vis.shiftY = vis.mapCenterY;
						var btn = vis.btnMgr.groups['toolbar'].getButton(4);
						btn.enabled = false;
						btn.draw();
						vis.director.draw();
					}, 'center the map').enabled = false;
					bg.addButton(5, function() {
						vis.setAntLabels((vis.config['label'] + 1) % 3);
						vis.director.draw();
					}, 'toggles: 1. player letters on ants, 2. global ids on ants');
					if (this.replay.hasDuration) {
						bg.addButton(6, function() {
							vis.config['speedFactor'] += 1;
							vis.setReplaySpeed();
						});
						bg.addButton(7, function() {
							vis.config['speedFactor'] -= 1;
							vis.setReplaySpeed();
						});
					}
				}
				// generate fog images
				var colors = [SAND_COLOR];
				for (i = 0; i < this.replay.players; i++) {
					colors.push(this.replay.meta['playercolors'][i]);
				}
				if (this.colorizedPlayerCount < this.replay.players) {
					this.colorizedPlayerCount = this.replay.players;
					this.imgMgr.colorize(4, colors);
					this.imgMgr.colorize(2, colors);
				}
				if (this.replay.hasDuration) {
					bg = this.btnMgr.addImageGroup('fog', this.imgMgr.patterns[4],
						ImageButtonGroup.VERTICAL, ButtonGroup.MODE_RADIO, 2);
					var buttonAdder = function(fog) {
						return bg.addButton(i, function() {
							vis.showFog(fog);
						}, (i == 0) ? 'clear fog of war' : 'show fog of war for ' + vis.replay.meta['playernames'][i - 1]);
					}
					for (var i = 0; i < colors.length; i++) {
						if (i == 0) {
							buttonAdder(undefined).down = true;
						} else {
							buttonAdder(i - 1);
						}
					}
				}
			}
			// add player buttons
			if (this.replay.hasDuration) {
				this.addPlayerButtons();
			}
			// calculate speed from duration and config settings
			this.director.duration = this.replay.duration;
			this.setReplaySpeed();
			if (this.options['interactive']) {
				this.director.onstate = function() {
					var btn = vis.btnMgr.groups['playback'].buttons[4];
					btn.offset = (vis.director.playing() ? 7 : 4) * vis.imgMgr.images[3].height;
					if (btn === vis.btnMgr.nailed) {
						vis.btnMgr.nailed = null;
					}
					btn.mouseUp();
				};
				// this will fire once in FireFox when a key is held down
				document.onkeydown = function(event) {
					if (!event) {
						// IE doesn't pass this as an argument
						event = window.event;
					}
					vis.keyPressed(event.keyCode);
				};
			}
			// setup mouse handlers
			this.main.canvas.onmousemove = function(event) {
				var mx = 0;
				var my = 0;
				var obj = this;
				if (this.offsetParent) do {
					mx += obj.offsetLeft;
					my += obj.offsetTop;
				} while ((obj = obj.offsetParent));
				mx = (event || window.event).clientX - mx + ((window.scrollX === undefined) ? (document.body.parentNode.scrollLeft !== undefined) ? document.body.parentNode.scrollLeft : document.body.scrollLeft : window.scrollX);
				my = (event || window.event).clientY - my + ((window.scrollY === undefined) ? (document.body.parentNode.scrollTop !== undefined) ? document.body.parentNode.scrollTop : document.body.scrollTop : window.scrollY);
				vis.mouseMoved(mx, my);
			};
			this.main.canvas.onmouseout = function() {
				vis.mouseExited();
			};
			this.main.canvas.onmousedown = function() {
				vis.mousePressed();
			};
			this.main.canvas.onmouseup = function() {
				vis.mouseReleased();
			};
			window.onresize = function() {
				vis.resize();
			};
			Visualizer.prototype.focused = this;
			// move to a specific row and col
			if (this.options['row'] !== undefined && this.options['col'] !== undefined) {
				this.shiftX = this.mapCenterX = (0.5 * this.replay.cols - 0.5) * ZOOM_SCALE - (this.options['col'] % this.replay.cols) * ZOOM_SCALE;
				this.shiftY = this.mapCenterY = (0.5 * this.replay.rows - 0.5) * ZOOM_SCALE - (this.options['row'] % this.replay.rows) * ZOOM_SCALE;
			}
			this.log.style.display = 'none';
			this.loading = LoadingState.IDLE;
			this.setFullscreen(this.config['fullscreen']);
			if (this.replay.hasDuration) {
				if (this.options['turn']) {
					this.director.gotoTick(this.options['turn'] - 1);
				} else {
					this.director.play();
				}
			}
		}
	} else if (!(this.replay instanceof XMLHttpRequest)) {
		this.loadParseReplay();
	}
};
Visualizer.prototype.addPlayerButtons = function() {
	var scores, i;
	var bg = this.btnMgr.addTextGroup('players', TextButtonGroup.FLOW,
			ButtonGroup.MODE_NORMAL, 2);
	var gameId = this.replay.meta['game_id'] || this.options['game'];
	var vis = this;
	if (this.replay.meta['game_url'] && gameId !== undefined) {
		var func = function() {
			window.location.href =
					vis.replay.meta['game_url'].replace('~', gameId);
		};
	}
	if (gameId === undefined) {
		bg.addButton('Players:', '#000', func);
	} else {
		bg.addButton('Game #' + gameId + ':', '#000', func);
	}
	if (this.replay.meta['replaydata']['bonus']) {
		scores = new Array(this.replay.players);
		for (i = 0; i < this.replay.players; i++) {
			scores[i] = this.replay.scores[this.replay.duration][i];
			scores[i] += this.replay.meta['replaydata']['bonus'][i];
		}
	} else {
		scores = this.replay.scores[this.replay.duration];
	}
	var ranks = new Array(scores.length);
	var order = new Array(scores.length);
	for (i = 0; i < scores.length; i++) {
		ranks[i] = 1;
		for (var k = 0; k < scores.length; k++) {
			if (scores[i] < scores[k]) {
				ranks[i]++;
			}
		}
		k = ranks[i] - 1;
		while(order[k] !== undefined) k++;
		order[k] = i;
	}
	var buttonAdder = function(i) {
		var color = vis.replay.htmlPlayerColors[i];
		var func = null;
		if (vis.replay.meta['user_url'] && vis.replay.meta['user_ids']
				&& vis.replay.meta['user_ids'][i] !== undefined) {
			func = function() {
				window.location.href =
						vis.replay.meta['user_url'].replace('~',
								vis.replay.meta['user_ids'][i]);
			};
		}
		var caption = vis.replay.meta['playernames'][i];
		caption = ranks[i] + '. ' + caption;
		if (vis.replay.meta['status']) {
			caption += ' (' + vis.statusToGlyph(i) + ')';
		}
		bg.addButton(caption, color, func);
	}
	for (i = 0; i < this.replay.players; i++) {
		buttonAdder(order[i]);
	}
};
Visualizer.prototype.setReplaySpeed = function() {
	var speed = this.director.duration / this.config['duration'];
	speed = Math.max(speed, this.config['speedSlowest']);
	speed = Math.min(speed, this.config['speedFastest']);
	this.director.defaultSpeed = speed * Math.pow(1.5, this.config['speedFactor']);
	if (this.director.speed !== 0) {
		this.director.speed = this.director.defaultSpeed;
	}
	var hintText = function(base) {
		return 'set speed modifier to ' + ((base > 0) ? '+' + base : base);
	}
	if (this.options['interactive'] && this.replay.hasDuration) {
		var speedUpBtn = this.btnMgr.groups['toolbar'].getButton(6);
		speedUpBtn.hint = hintText(this.config['speedFactor'] + 1);
		var slowDownBtn = this.btnMgr.groups['toolbar'].getButton(7);
		slowDownBtn.hint = hintText(this.config['speedFactor'] - 1);
	}
};
Visualizer.prototype.calculateCanvasSize = function() {
	var result = {};
	if (typeof(window.innerWidth) == 'number') {
		//Non-IE
		result.width = window.innerWidth;
		result.height = window.innerHeight;
	} else if (document.documentElement && (document.documentElement.clientWidth || document.documentElement.clientHeight)) {
		//IE 6+ in 'standards compliant mode'
		result.width = document.documentElement.clientWidth;
		result.height = document.documentElement.clientHeight;
	}
	var embed = (window.isFullscreenSupported
			&& !window.isFullscreenSupported())
			|| !this.config['fullscreen'];
	result.width = (this.w && embed) ? this.w : result.width;
	result.height = (this.h && embed) ? this.h : result.height;
	return result;
};
Visualizer.prototype.createCanvas = function(obj) {
	if (!obj.canvas) {
		obj.canvas = document.createElement('canvas');
	}
	if (!obj.ctx) {
		obj.ctx = obj.canvas.getContext('2d');
	}
};
Visualizer.prototype.setFullscreen = function(enable) {
	if (!window.isFullscreenSupported || window.isFullscreenSupported()) {
		if (window.setFullscreen) {
			this.config['fullscreen'] = window.setFullscreen(enable);
		} else {
			this.config['fullscreen'] = enable;
			if (enable || this.savedBody) {
				var html = document.getElementsByTagName('html')[0];
				if (enable) {
					this.container.removeChild(this.main.canvas);
					this.savedOverflow = html.style.overflow;
					html.style.overflow = 'hidden';
					var tempBody = document.createElement('body');
					tempBody.appendChild(this.main.canvas);
					this.savedBody = html.replaceChild(tempBody, document.body);
				} else if (this.savedBody) {
					document.body.removeChild(this.main.canvas);
					this.container.appendChild(this.main.canvas);
					html.replaceChild(this.savedBody, document.body);
					html.style.overflow = this.savedOverflow;
					delete this.savedBody;
				}
			}
		}
	}
	this.resize(true);
};
Visualizer.prototype.setZoom = function(zoom) {
	var oldScale = this.scale;
	zoom = Math.max(1, zoom);
	this.config['zoom'] = zoom;
	this.scale = Math.max(1, Math.min(
		(this.loc.vis.w - 20) / (this.replay.cols),
		(this.loc.vis.h - 20) / (this.replay.rows))) | 0;
	this.scale = Math.min(ZOOM_SCALE, this.scale * zoom);
	if (oldScale) {
		this.shiftX = (this.shiftX * this.scale / oldScale) | 0;
		this.shiftY = (this.shiftY * this.scale / oldScale) | 0;
	}
	this.map.setSize(this.scale * this.replay.cols, this.scale * this.replay.rows);
	this.map.x = ((this.loc.vis.w - this.map.w) / 2 + this.loc.vis.x) | 0;
	this.map.y = ((this.loc.vis.h - this.map.h) / 2 + this.loc.vis.y) | 0;
	this.overlay.canvas.width  = Math.min(this.map.w, this.loc.vis.w);
	this.overlay.canvas.height = Math.min(this.map.h, this.loc.vis.h);
	this.border.canvas.width  = Math.min(this.loc.vis.w, this.map.w);
	this.border.canvas.height = Math.min(this.loc.vis.h, this.map.h);
	if (this.options['interactive']) {
		var zoomInBtn = this.btnMgr.groups['toolbar'].getButton(2);
		zoomInBtn.enabled = !(this.scale === ZOOM_SCALE);
		zoomInBtn.draw();
		var zoomOutBtn = this.btnMgr.groups['toolbar'].getButton(3);
		zoomOutBtn.enabled = !(zoom === 1);
		zoomOutBtn.draw();
	}
};
Visualizer.prototype.setAntLabels = function(mode) {
	this.config['label'] = mode;
};
Visualizer.prototype.resize = function(forced) {
	var y, w, h;
	var olds = {
		width: this.main.canvas.width,
		height: this.main.canvas.height
	};
	var news = this.calculateCanvasSize();
	var resizing = news.width != olds.width || news.height != olds.height;
	if (resizing || forced) {
		var canvas = this.main.canvas;
		var ctx = this.main.ctx;
		if (resizing) {
			canvas.width = news.width;
			canvas.height = news.height;
			ctx.fillStyle = '#fff';
			ctx.fillRect(0, 0, canvas.width, canvas.height);
		}
		ctx.font = FONT;
		ctx.textAlign = 'left';
		ctx.textBaseline = 'middle';
		if (this.replay.hasDuration) {
			// 1. player buttons
			y = this.btnMgr.groups['players'].cascade(news.width) + 4;
			// 2. scores bar & time line
			this.loc.graph = new Location(4, y + 66, news.width - 8, 64);
			this.loc.scorebar = new Location(95, y +  4, news.width - 4 - 95, 22);
			this.loc.countbar = new Location(95, y + 38, news.width - 4 - 95, 22);
			if (resizing) {
				ctx.strokeStyle = '#444';
				ctx.lineWidth = 2;
				shapeRoundedRect(ctx, 0, y, canvas.width, 30, 1, 5);
				ctx.stroke();
				shapeRoundedRect(ctx, 0, y + 34, canvas.width, 100, 1, 5);
				ctx.moveTo(0, y + 63);
				ctx.lineTo(canvas.width, y + 63);
				ctx.stroke();
				ctx.lineWidth = 1;
				ctx.fillStyle = '#888';
				ctx.fillText('scores', 4, y + 14);
				ctx.fillText('# of ants', 4, y + 48);
			} else {
				ctx.fillStyle = '#fff';
				ctx.fillRect(0, 0, canvas.width, y);
			}
			y += 134;
		} else {
			y = 0;
		}
		// 3. visualizer placement
		if (this.options['interactive']) {
			if (this.replay.hasDuration) {
				this.loc.vis = new Location(LEFT_PANEL_W, y,
					news.width - LEFT_PANEL_W - RIGHT_PANEL_W,
					news.height - y - BOTTOM_PANEL_H);
				var bg = this.btnMgr.groups['playback'];
				w = 8 * 64;
				if (w <= news.width) {
					bg.x = ((news.width - w) / 2) | 0;
				} else {
					bg.x = 0;
				}
				bg.y = this.loc.vis.y + this.loc.vis.h;
				bg = this.btnMgr.groups['fog'];
				bg.y = this.loc.vis.y + 8;
			} else {
				this.loc.vis = new Location(0, y, news.width - RIGHT_PANEL_W,
					news.height - y);
			}
			bg = this.btnMgr.groups['toolbar'];
			bg.x = this.loc.vis.x + this.loc.vis.w;
			bg.y = this.loc.vis.y + 8;
			// set button group extents
			if (this.replay.hasDuration) {
				bg = this.btnMgr.groups['fog'];
				bg.h = news.height - this.loc.vis.y - 8;
				bg = this.btnMgr.groups['playback'];
				bg.w = news.width - 2 * 48;
			}
			bg = this.btnMgr.groups['toolbar'];
			bg.h = news.height - this.loc.vis.y - 8;
		} else {
			this.loc.vis = new Location(0, y, news.width, news.height - y);
		}
		this.setZoom(this.config['zoom']);
		w = Math.min(this.loc.vis.w, this.map.w);
		h = Math.min(this.loc.vis.h, this.map.h);
		if (this.border.canvas.width !== w || this.border.canvas.height !== h) {
			this.border.canvas.width = w;
			this.border.canvas.height = h;
		}
		if (this.replay.hasDuration) {
			this.scores.canvas.width = this.loc.graph.w;
			this.scores.canvas.height = this.loc.graph.h;
			this.renderCounts();
		}
		this.minimap.loc = new Location(
				this.loc.vis.x + this.loc.vis.w - 2 - this.replay.cols,
				this.loc.vis.y + 2, this.replay.cols, this.replay.rows);
		this.minimap.canvas.width = this.replay.cols;
		this.minimap.canvas.height = this.replay.rows;
		// redraw everything
		this.btnMgr.draw();
		this.director.draw(true);
	}
};
/**
 * @private
 */
Visualizer.prototype.redFocusRectFun = function(ctx, scale, xs, ys) {
	var x, y, w, i;
	for (i = 0; i < 5; i++) {
		ctx.strokeStyle = 'rgba(255,0,0,' + (i + 1) / 5 + ')';
		w = scale + 9 - 2 * i;
		x = xs + i;
		y = ys + i;
		ctx.strokeRect(x, y, w, w);
	}
};
/**
 * @private
 */
Visualizer.prototype.renderMapScaled = function(ctx, scale) {
	var row, col, start, isWall, xs, ys;
	ctx.fillStyle = SAND_COLOR;
	ctx.fillRect(0, 0, this.map.w, this.map.h);
	ctx.fillStyle = ctx.createPattern(this.imgMgr.images[0], 'repeat');
	for (row = 0; row < this.replay.rows; row++) {
		start = undefined;
		for (col = 0; col < this.replay.cols; col++) {
			isWall = this.replay.walls[row][col];
			if (start === undefined && isWall) {
				start = col;
			} else if (start !== undefined && !isWall) {
				ctx.fillRect(scale * start, scale * row, scale * (col - start), scale);
				start = undefined;
			}
		}
		if (start !== undefined) {
			ctx.fillRect(scale * start, scale * row, scale * (col - start), scale);
		}
	}
	if (this.options['row'] !== undefined && this.options['col'] !== undefined) {
		xs = (this.options['col'] % this.replay.cols) * scale - 4.5;
		ys = (this.options['row'] % this.replay.rows) * scale - 4.5;
		this.drawWrapped(
				xs, ys, scale + 9, scale + 9, this.map.w, this.map.h,
				ctx, this.redFocusRectFun, [ctx, scale, xs, ys]
			);
	}
};
Visualizer.prototype.renderMap = function(ctx) {
	this.renderMapScaled(ctx, this.scale)
};
/**
 * @private
 */
Visualizer.prototype.renderCounts = function() {
	var ctx = this.scores.ctx;
	var w = this.scores.canvas.width - 1;
	var h = this.scores.canvas.height - 1;
	ctx.fillStyle = '#FFF';
	ctx.fillRect(0, 0, w + 1, h + 1);
	// find lowest and highest value
	var min = 0;
	var max = -Infinity;
	for (var i = 0; i <= this.replay.duration; i++) {
		for (var k = 0; k < this.replay.counts[i].length; k++) {
			if (max < this.replay.counts[i][k]) {
				max = this.replay.counts[i][k];
			}
		}
	}
	// draw lines
	var scaleX = (this.replay.duration === 0) ? 0 : w / this.replay.duration;
	ctx.strokeStyle = 'rgba(0,0,0,0.5)';
	ctx.beginPath();
	for (i = 0; i <= this.replay.duration + 1; i++) {
		var t = i + 1;
		ctx.moveTo(0.5 + scaleX * i, h - (t % 100 ? t % 10 ? 3 : 7 : 17));
		ctx.lineTo(0.5 + scaleX * i, h + 1);
	}
	ctx.moveTo(0.5 + 0, h + 0.5);
	ctx.lineTo(0.5 + scaleX * (this.replay.duration + 1), h + 0.5);
	ctx.stroke();
	var scaleY = h / (max - min);
	for (i = this.replay.players - 1; i >= 0; i--) {
		ctx.strokeStyle = this.replay.htmlPlayerColors[i];
		ctx.beginPath();
		ctx.moveTo(0.5, 0.5 + scaleY * (max - this.replay.counts[0][i]));
		for (k = 1; k <= this.replay.duration; k++) {
			ctx.lineTo(0.5 + scaleX * k, 0.5 + scaleY * (max - this.replay.counts[k][i]));
		}
		ctx.stroke();
	}
	if (!this.isStreaming && this.replay.meta['status']) {
		ctx.font = '10px Arial,Sans';
		for (i = this.replay.players - 1; i >= 0; i--) {
			ctx.fillStyle = this.replay.htmlPlayerColors[i];
			ctx.strokeStyle = this.replay.htmlPlayerColors[i];
			var status = this.statusToGlyph(i);
			k = this.replay.meta['replaydata']['scores'][i].length - 1;
			ctx.beginPath();
			var x = 0.5 + k * scaleX;
			var y = 0.5 + scaleY * (max - this.replay.counts[k][i]);
			ctx.moveTo(x, y);
			var tw = ctx.measureText(status).width;
			var tx = Math.min(x, w - tw);
			if (y < 30) {
				y = ((y + 12) | 0) + 0.5;
				ctx.lineTo(x, y - 8);
				ctx.moveTo(tx, y - 8);
				ctx.lineTo(tx + tw, y - 8);
				ctx.fillText(status, tx, y);
			} else {
				y = ((y - 7) | 0) + 0.5;
				ctx.lineTo(x, y + 2);
				ctx.moveTo(tx, y + 2);
				ctx.lineTo(tx + tw, y + 2);
				ctx.fillText(status, tx, y);
			}
			ctx.stroke();
		}
	}
};
Visualizer.prototype.statusToGlyph = function(i) {
	var status = this.replay.meta['status'][i];
	if (status === 'survived') {
		return '✓';
	} else if (status === 'eliminated') {
		return '✗';
	} else {
		return this.replay.meta['status'][i];
	}
};
/**
 * @private
 */
Visualizer.prototype.renderFog = function(turn) {
	var ctx = this.overlay.ctx;
	ctx.clearRect(0, 0, this.overlay.canvas.width, this.overlay.canvas.height);
	if (this.fog) {
		if (!this.fog.ctx) {
			this.createCanvas(this.fog);
			this.fog.canvas.width = 2;
			this.fog.canvas.height = 2;
			this.fog.ctx.fillStyle = this.replay.htmlPlayerColors[this.fog.player];
			this.fog.ctx.fillRect(0, 0, 1, 1);
			this.fog.ctx.fillRect(1, 1, 1, 1);
			this.fog.ptrn = ctx.createPattern(this.fog.canvas, 'repeat');
		}
		var loc = this.loc.vis;
		var rowPixels = this.scale * this.replay.rows;
		var colPixels = this.scale * this.replay.cols;
		if (loc.h < rowPixels) {
			var y = (0.5 * (this.loc.vis.h - rowPixels) + this.shiftY) | 0;
		} else {
			y = 0;
		}
		if (loc.w < colPixels) {
			var x = (0.5 * (this.loc.vis.w - colPixels) + this.shiftX) | 0;
		} else {
			x = 0;
		}
		ctx.fillStyle = this.fog.ptrn;
		var margin = this.scale - 1;
		var fog = this.replay.getFog(this.fog.player, turn);
		for (var row = 0; row < this.replay.rows; row++) {
			var fogRow = fog[row];
			var dy = row * this.scale + y + margin;
			dy -= Math.floor(dy / rowPixels) * rowPixels + margin;
			if (dy < loc.h) for (var col = 0; col < this.replay.cols; col++) {
				if (fogRow[col]) {
					var dx = col * this.scale + x + margin;
					dx -= Math.floor(dx / colPixels) * colPixels + margin;
					if (dx < loc.w) {
						ctx.fillRect(dx, dy, this.scale, this.scale);
					}
				}
			}
		}
	}
};
Visualizer.prototype.showFog = function(fog) {
	if (fog === undefined) {
		this.fog = undefined;
		this.renderFog(this.director.position | 0);
	} else {
		this.fog = {player: fog};
	}
	this.director.draw();
};
/**
 * @private
 */
Visualizer.prototype.drawColorBar = function(loc, values, bonus) {
	var sum = 0;
	this.main.ctx.save();
	this.main.ctx.beginPath();
	this.main.ctx.rect(loc.x, loc.y, loc.w, loc.h);
	this.main.ctx.clip();
	for (var i = 0; i < values.length; i++) {
		sum += bonus ? values[i] + bonus[i] : values[i];
	}
	var useValues = new Array(values.length);
	if (sum == 0) {
		for (i = 0; i < values.length; i++) {
			useValues[i] = 1;
		}
		sum = values.length;
	} else {
		for (i = 0; i < values.length; i++) {
			useValues[i] = bonus ? values[i] + bonus[i] : values[i];
		}
	}
	var scale = loc.w / sum;
	var offsetX = loc.x;
	for (i = 0; i < useValues.length; i++) {
		var amount = scale * useValues[i];
		this.main.ctx.fillStyle = this.replay.htmlPlayerColors[i];
		this.main.ctx.fillRect(offsetX, loc.y, loc.w - offsetX + loc.x, loc.h);
		offsetX += amount;
	}
	this.main.ctx.textAlign = 'left';
	this.main.ctx.textBaseline = 'top';
	this.main.ctx.font = 'bold 16px Monospace';
	this.main.ctx.fillStyle = 'rgba(0,0,0,0.5)';
	var offsetY = loc.y + 3;
	offsetX = loc.x + 2;
	for (i = 0; i < useValues.length; i++) {
		var text = Math.round(values[i]);
		if (this.config['label'] === 1) {
			text = String.fromCharCode(65 + i) + ':' + text;
		}
		var bonusText = (bonus && bonus[i]) ? '+' + Math.round(bonus[i]) : '';
		var textWidth = this.main.ctx.measureText(text).width
		if (bonusText) {
			this.main.ctx.font = 'bold italic 12px Monospace';
			var bonusTextWidth = this.main.ctx.measureText(bonusText).width;
			this.main.ctx.font = 'bold 16px Monospace';
		} else {
			bonusTextWidth = 0;
		}
		if (scale * useValues[i] >= textWidth + bonusTextWidth) {
			this.main.ctx.fillText(text, offsetX, offsetY);
			if (bonusText) {
				this.main.ctx.font = 'bold italic 12px Monospace';
				this.main.ctx.fillStyle = 'rgba(0,0,0,0.8)';
				this.main.ctx.fillText(bonusText, offsetX + textWidth, offsetY);
				this.main.ctx.font = 'bold 16px Monospace';
				this.main.ctx.fillStyle = 'rgba(0,0,0,0.5)';
			}
		}
		offsetX += scale * useValues[i];
	}
	this.main.ctx.restore();
};
/**
 * @private
 */
Visualizer.prototype.interpolate = function(array1, array2, delta) {
	if (delta === 0) return array1;
	var result = new Array(array1.length);
	for (var i = 0; i < result.length; i++) {
		result[i] = (1.0 - delta) * array1[i] + delta * array2[i];
	}
	return result;
};
/**
 * @private
 * helper function to draw items wrapped around the map borders
 */
Visualizer.prototype.drawWrapped = function(x, y, w, h, colPixels, rowPixels, ctx, func, args) {
	var delta_x, delta_y, tx, ty, sum;
	if (x < 0 || y < 0 || x + w > colPixels || y + h > rowPixels) {
		ctx.save();
		delta_x = -Math.floor((x + w) / colPixels) * colPixels;
		delta_y = -Math.floor((y + h) / rowPixels) * rowPixels;
		ctx.translate(delta_x, delta_y);
		for (ty = y + delta_y; ty < rowPixels; ty += rowPixels)
		{
			sum = 0;
			for (tx = x + delta_x; tx < colPixels; tx += colPixels)
			{
				func.apply(this, args);
				ctx.translate(colPixels, 0);
				sum -= colPixels;
			}
			ctx.translate(sum, rowPixels);
		}
		ctx.restore();
	} else {
		func.apply(this, args);
	}
}
/**
 * @private
 */
Visualizer.prototype.draw = function(time, tick) {
	var x, y, w, h, dx, dy, d, hash, ants, ant, i, img, offset, visibleAnts;
	var turn = (time | 0);
	if (this.replay.hasDuration) {
		// draw scores
		var duration = this.replay.duration;
		if (tick) {
			if (this.fog !== undefined) this.renderFog(turn);
			this.drawColorBar(this.loc.scorebar, this.replay.scores[turn], (turn === duration) ? this.replay.meta['replaydata']['bonus'] : undefined);
			this.drawColorBar(this.loc.countbar, this.replay.counts[turn]);
		}
		this.main.ctx.drawImage(this.scores.canvas, this.loc.graph.x, this.loc.graph.y);
		// time indicator
		x = this.loc.graph.x + 0.5 + (this.loc.graph.w - 1) * time / duration;
		this.main.ctx.lineWidth = 1;
		this.main.ctx.beginPath();
		this.main.ctx.moveTo(x, this.loc.graph.y + 0.5);
		this.main.ctx.lineTo(x, this.loc.graph.y + this.loc.graph.h - 0.5);
		this.main.ctx.stroke();
		// turn number
		this.main.ctx.fillStyle = '#888';
		this.main.ctx.textBaseline = 'middle';
		this.main.ctx.fillText('# of ants | ' + (turn === duration && !this.isStreaming ?
				'end' : 'turn ' + (turn + 1) + '/' + duration),
				this.loc.graph.x, this.loc.graph.y + 11);
	}
	// ants...
	var drawStates = {};
	var loc = this.loc.vis;
	var rowPixels = this.scale * this.replay.rows;
	var colPixels = this.scale * this.replay.cols;
	if (loc.h < rowPixels) {
		y = (0.5 * (this.loc.vis.h - rowPixels) + this.shiftY) | 0;
		y -= Math.ceil(y / rowPixels) * rowPixels;
	} else {
		y = 0;
	}
	if (loc.w < colPixels) {
		x = (0.5 * (this.loc.vis.w - colPixels) + this.shiftX) | 0;
		x -= Math.ceil(x / colPixels) * colPixels;
	} else {
		x = 0;
	}
	var drawMinimap = tick !== undefined && this.config['zoom'] !== 1;
	if (drawMinimap) {
		this.renderMapScaled(this.minimap.ctx, 1);
	}
	ants = this.replay.getTurn(turn);
	visibleAnts = new Array(ants.length);
	n = 0;
	for (i = ants.length - 1; i >= 0; i--) {
		if ((ant = ants[i].interpolate(time, Quality.LOW))) {
			hash = INT_TO_HEX[ant['r']] + INT_TO_HEX[ant['g']] + INT_TO_HEX[ant['b']];
			ant.baseX = Math.round(ant['x']);
			ant.baseY = Math.round(ant['y']);
			ant.baseX -= Math.floor(ant.baseX / this.replay.cols) * this.replay.cols;
			ant.baseY -= Math.floor(ant.baseY / this.replay.rows) * this.replay.rows;
			if (drawMinimap) {
				this.minimap.ctx.fillStyle = '#' + hash;
				this.minimap.ctx.fillRect(ant.baseX, ant.baseY, 1, 1);
			}
			ant['x'] = Math.round(this.scale * ant['x']) + x + this.scale - 1;
			ant['y'] = Math.round(this.scale * ant['y']) + y + this.scale - 1;
			// correct coordinates
			ant['x'] -= Math.floor(ant['x'] / colPixels) * colPixels + this.scale - 1;
			ant['y'] -= Math.floor(ant['y'] / rowPixels) * rowPixels + this.scale - 1;
			if (ant['x'] < loc.w && ant['y'] < loc.h) {
				if (!drawStates[hash]) drawStates[hash] = [];
				drawStates[hash].push(ant);
				visibleAnts[n++] = ant;
			}
		}
	}
	visibleAnts.length = n;
	// draw the map background
	this.map.validate();
	var ctx = this.border.ctx;
	for (dy = y; dy < this.loc.vis.h; dy += rowPixels) {
		for (dx = x; dx < this.loc.vis.w; dx += colPixels) {
			ctx.drawImage(this.map.canvas, dx, dy);
		}
	}
	if (this.shiftX || this.shiftY) {
		ctx.strokeStyle = '#000';
		ctx.lineWidth = 2;
		ctx.beginPath();
		for (dy = y; dy <= this.loc.vis.h; dy += rowPixels) {
			ctx.moveTo(0, dy);
			ctx.lineTo(this.loc.vis.w, dy);
		}
		for (dx = x; dx <= this.loc.vis.w; dx += colPixels) {
			ctx.moveTo(dx, 0);
			ctx.lineTo(dx, this.loc.vis.h);
		}
		ctx.stroke();
		ctx.lineWidth = 1;
	}
	// sorting by render state gives slight fps improvements
	var halfScale = 0.5 * this.scale;
	for (var key in drawStates) {
		ctx.fillStyle = '#' + key;
		var drawList = drawStates[key];
		for (var n = 0; n < drawList.length; n++) {
			ant = drawList[n];
			if (this.config['graphics'] && this.scale === ZOOM_SCALE) {
				ctx.save();
				//this.main.ctx.globalAlpha = ant.alpha;
				ctx.translate(ant['x'] + halfScale, ant['y'] + halfScale);
				if (ant['owner'] === undefined) {
					img = this.imgMgr.images[6];
					offset = 20 * (ant['id'] % 5);
				} else {
					//ctx.rotate(ant['angle'] + Math.sin(20 * time) * ant['jitter']);
					img = this.imgMgr.patterns[2];
					offset = ZOOM_SCALE * (ant['owner'] + 1);
				}
				ctx.drawImage(img, offset, 0, ZOOM_SCALE, ZOOM_SCALE, -halfScale * ant['size'], -halfScale * ant['size'], this.scale * ant['size'], this.scale * ant['size']);
				ctx.restore();
			} else {
				if (ant['owner'] === undefined) {
					w = halfScale;
					if (ant['size'] !== 1) w *= ant['size'];
					ctx.beginPath();
					ctx.arc(ant['x'] + halfScale, ant['y'] + halfScale, w, 0, 2 * Math.PI, false);
					ctx.fill();
				} else {
					w = this.scale;
					dx = ant['x'];
					dy = ant['y'];
					if (ant['size'] !== 1) {
						d = 0.5 * (1.0 - ant['size']) * this.scale;
						dx += d;
						dy += d;
						w *= ant['size'];
					}
					ctx.fillRect(dx, dy, w, w);
					if (dx < 0) {
						ctx.fillRect(dx + colPixels, dy, w, w);
						if (dy < 0) {
							ctx.fillRect(dx + colPixels, dy + rowPixels, w, w);
						}
					}
					if (dy < 0) {
						ctx.fillRect(dx, dy + rowPixels, w, w);
					}
				}
			}
		}
	}
	if (this.config['label']) {
		ctx.save();
		ctx.translate(halfScale, halfScale);
		var fontSize = Math.max(this.scale, 8);
		ctx.textBaseline = 'middle';
		ctx.textAlign = 'center';
		ctx.font = 'bold ' + Math.ceil(fontSize / this.config['label']) + 'px Arial';
		ctx.fillStyle = '#000';
		ctx.strokeStyle = '#fff';
		ctx.lineWidth = 0.2 * fontSize;
		for (key in drawStates) {
			drawList = drawStates[key];
			for (n = 0; n < drawList.length; n++) {
				ant = drawList[n];
				if (this.config['label'] === 1 && ant['owner'] !== undefined) {
					var letter = String.fromCharCode(65 + ant['owner']);
				} else if (this.config['label'] === 2) {
					letter = ant.id;
				} else {
					letter = undefined;
				}
				if (letter !== undefined) {
					ctx.strokeText(letter, ant['x'], ant['y']);
					ctx.fillText(letter, ant['x'], ant['y']);
					if (ant['x'] < 0) {
						ctx.strokeText(letter, ant['x'] + this.map.w, ant['y']);
						ctx.fillText(letter, ant['x'] + this.map.w, ant['y']);
						if (ant['y'] < 0) {
							ctx.strokeText(letter, ant['x'] + this.map.w, ant['y'] + this.map.h);
							ctx.fillText(letter, ant['x'] + this.map.w, ant['y'] + this.map.h);
						}
					}
					if (ant['y'] < 0) {
						ctx.strokeText(letter, ant['x'], ant['y'] + this.map.h);
						ctx.fillText(letter, ant['x'], ant['y'] + this.map.h);
					}
				}
			}
		}
		ctx.restore();
	}
	// calculate mouse position
	if (this.mouseOverVis) {
		var mx = Math.round(this.scale * this.mouseCol) + x + this.scale - 1;
		mx = Math.wrapAround(mx, colPixels) - this.scale + 1;
		var my = Math.round(this.scale * this.mouseRow) + y + this.scale - 1;
		my = Math.wrapAround(my, rowPixels) - this.scale + 1;
	}
	// draw attack and spawn radii
	var ar = this.scale * Math.sqrt(this.replay.meta['replaydata']['attackradius2']);
	var sr = this.scale * Math.sqrt(this.replay.meta['replaydata']['spawnradius2']);
	ctx.save();
	ctx.translate(halfScale, halfScale);
	ctx.lineWidth = 2 * this.scale / ZOOM_SCALE;
	for (var a1 = 0; a1 < visibleAnts.length; a1++) {
		var ant1 = visibleAnts[a1];
		if (ant1['owner'] !== undefined) {
			for (var a2 = a1 + 1; a2 < visibleAnts.length; a2++) {
				var ant2 = visibleAnts[a2];
				if (ant2['owner'] !== undefined && ant1['owner'] !== ant2['owner']) {
					dx = ant1.baseX - ant2.baseX;
					dy = ant1.baseY - ant2.baseY;
					dx -= (Math.floor(dx / this.replay.cols - 0.5) + 1) * this.replay.cols;
					dy -= (Math.floor(dy / this.replay.rows - 0.5) + 1) * this.replay.rows;
					if (dx * dx + dy * dy <= this.replay.meta['replaydata']['attackradius2']) {
						var allAnts = this.replay.meta['replaydata']['ants'];
						if (allAnts[ant1['id']][3] + 0.5 <= time && allAnts[ant2['id']][3] + 0.5 <= time) {
							ctx.beginPath();
							ctx.moveTo(ant1['x'], ant1['y']);
							ctx.lineTo(ant1['x'] - dx * halfScale, ant1['y'] - dy * halfScale);
							ctx.strokeStyle = this.replay.htmlPlayerColors[ant2['owner']];
							ctx.stroke();
							ctx.beginPath();
							ctx.moveTo(ant2['x'], ant2['y']);
							ctx.lineTo(ant2['x'] + dx * halfScale, ant2['y'] + dy * halfScale);
							ctx.strokeStyle = this.replay.htmlPlayerColors[ant1['owner']];
							ctx.stroke();
						}
					}
				}
			}
		}
	}
	if (this.mouseOverVis) {
		var drawEffectCircle = function(x, y, mx, my) {
			ctx.beginPath();
			ctx.arc(x, y, radius, 0, 2 * Math.PI, false);
			ctx.moveTo(x, y);
			ctx.lineTo(mx, my);
			ctx.stroke();
		}
		for (key in drawStates) {
			ctx.strokeStyle = '#' + key;
			drawList = drawStates[key];
			for (n = 0; n < drawList.length; n++) {
				ant = drawList[n];
				var radius = (ant['owner'] !== undefined) ? ar : sr;
				dx = ant['x'] - mx;
				dy = ant['y'] - my;
				if (radius * radius >= dx * dx + dy * dy) {
					this.drawWrapped(
							ant['x'] - radius, ant['y'] - radius,
							2 * radius, 2 * radius, colPixels, rowPixels,
							ctx, drawEffectCircle, [ant['x'], ant['y'], mx, my]
						);
				}
			}
		}
	}
	ctx.restore();
	// render fog onto map
	if (this.fog) ctx.drawImage(this.overlay.canvas, 0, 0);
	// draw mouse location
	if (this.mouseOverVis) {
		ctx.strokeStyle = '#fff';
		ctx.strokeRect(mx + 0.5, my + 0.5, this.scale - 1, this.scale - 1);
	}
	// draw buffer on screen
	ctx = this.main.ctx;
	ctx.save();
	ctx.beginPath();
	ctx.rect(loc.x, loc.y, loc.w, loc.h);
	ctx.clip();
	if (this.border.canvas.width < loc.w) {
		var sx = this.map.x - loc.x + this.shiftX;
		sx += loc.x - Math.ceil(sx / this.map.w) * this.map.w;
	} else {
		sx = loc.x;
	}
	if (this.border.canvas.height < loc.h) {
		var sy = this.map.y - loc.y + this.shiftY;
		sy += loc.y - Math.ceil(sy / this.map.h) * this.map.h;
	} else {
		sy = loc.y;
	}
	for (dy = sy; dy < loc.y + loc.h; dy += this.map.h) {
		for (dx = sx; dx < loc.x + loc.w; dx += this.map.w) {
			ctx.drawImage(this.border.canvas, dx, dy);
		}
	}
	ctx.restore();
	// shade repeated parts
	ctx.fillStyle = 'rgba(0,0,0,0.3)';
	w = this.map.y - loc.y;
	if (w > 0) ctx.fillRect(loc.x, loc.y, loc.w, w);
	w = loc.y + loc.h - this.map.y - this.map.h;
	if (w > 0) ctx.fillRect(loc.x, this.map.y + this.map.h, loc.w, w);
	w = this.map.x - loc.x;
	if (w > 0) ctx.fillRect(loc.x, loc.y, w, loc.h);
	w = loc.x + loc.w - this.map.x - this.map.w;
	if (w > 0) ctx.fillRect(this.map.x + this.map.w, loc.y, w, loc.h);
	// minimap
	if (this.config['zoom'] !== 1 && this.minimap.loc.h + 4 <= this.loc.vis.h && this.minimap.loc.w + 4 <= this.loc.vis.w) {
		ctx.save();
		// border
		ctx.strokeStyle = '#fff';
		ctx.lineWidth = 2;
		ctx.beginPath();
		loc = this.minimap.loc;
		ctx.rect(loc.x - 1, loc.y - 1, loc.w + 2, loc.h + 2);
		ctx.stroke();
		// map
		ctx.drawImage(this.minimap.canvas, loc.x, loc.y, loc.w, loc.h);
		// position indicator
		ctx.beginPath();
		ctx.rect(loc.x, loc.y, loc.w, loc.h);
		ctx.clip();
		w = this.loc.vis.w / this.scale;
		h = this.loc.vis.h / this.scale;
		dx = this.replay.cols / 2 - this.shiftX / this.scale - w / 2;
		dy = this.replay.rows / 2 - this.shiftY / this.scale - h / 2;
		dx -= Math.floor(dx / this.replay.cols) * this.replay.cols;
		dy -= Math.floor(dy / this.replay.rows) * this.replay.rows;
		ctx.beginPath();
		ctx.rect(loc.x + dx, loc.y + dy, w, h);
		ctx.rect(loc.x + dx - this.replay.cols, loc.y + dy, w, h);
		ctx.rect(loc.x + dx, loc.y + dy - this.replay.rows, w, h);
		ctx.rect(loc.x + dx - this.replay.cols, loc.y + dy - this.replay.rows, w, h);
		ctx.stroke();
		ctx.restore();
	}
	// draw hint text
	var hint = this.hint;
	if (hint) {
		if (ctx.measureText(hint).width > this.loc.vis.w) {
			do {
				hint = hint.substr(0, hint.length - 1);
			} while (hint && ctx.measureText(hint + '...').width > this.loc.vis.w);
			if (hint) hint += '...';
		}
		w = ctx.measureText(hint).width;
		ctx.fillRect(this.loc.vis.x, this.loc.vis.y, w, 20);
		ctx.fillStyle = '#fff';
		ctx.fillText(hint, this.loc.vis.x, this.loc.vis.y + 10);
	}
	// we were able to draw a frame, the engine may send us the next turn
	if (this.isStreaming) {
		var vis = this;
		window.setTimeout(function() {
			if (vis.isStreaming) vis.streamingStart();
		}, 0);
	}
};
Visualizer.prototype.mouseMoved = function(mx, my) {
	var deltaX = mx - this.mouseX;
	var deltaY = my - this.mouseY;
	this.mouseX = mx;
	this.mouseY = my;
	this.mouseCol = (Math.wrapAround(mx - this.map.x - this.shiftX,
			this.scale * this.replay.cols) / this.scale) | 0;
	this.mouseRow = (Math.wrapAround(my - this.map.y - this.shiftY,
			this.scale * this.replay.rows) / this.scale) | 0;
	var oldHint = this.hint;
	this.hint = '';
	if (this.options['interactive']) {
		this.mouseOverVis = this.map.contains(this.mouseX, this.mouseY)
				&& this.loc.vis.contains(this.mouseX, this.mouseY);
		if (this.mouseOverVis) {
			this.hint = 'row ' + this.mouseRow + ' | col ' +  this.mouseCol;
		}
		if (this.mouseDown === 1) {
			mx = (this.mouseX - this.loc.graph.x) / (this.loc.graph.w - 1);
			mx = Math.round(mx * this.replay.duration);
			this.director.gotoTick(mx);
		} else if (this.mouseDown === 2 || (this.mouseDown === 3 && this.minimap.loc.contains(this.mouseX, this.mouseY))) {
			if (this.mouseDown === 2) {
				this.shiftX += deltaX;
				this.shiftY += deltaY;
			} else {
				this.shiftX = (this.replay.cols / 2 - (this.mouseX - this.minimap.loc.x)) * this.scale;
				this.shiftY = (this.replay.rows / 2 - (this.mouseY - this.minimap.loc.y)) * this.scale;
			}
			var centerBtn = this.btnMgr.groups['toolbar'].getButton(4);
			centerBtn.enabled = this.shiftX || this.shiftY;
			centerBtn.draw();
			this.director.draw();
		} else {
			var btn = this.btnMgr.mouseMove(mx, my);
		}
	} else {
		btn = this.btnMgr.mouseMove(mx, my);
	}
	if (btn && btn.hint) {
		this.hint = btn.hint;
	}
	if (oldHint !== this.hint) {
		this.director.draw();
	}
};
Visualizer.prototype.mousePressed = function() {
	if (this.options['interactive']) {
		if (this.replay.hasDuration && this.loc.graph.contains(this.mouseX, this.mouseY)) {
			this.mouseDown = 1;
		} else {
			if (this.config['zoom'] !== 1 && this.minimap.loc.contains(this.mouseX, this.mouseY)) {
				this.mouseDown = 3;
			} else if (this.loc.vis.contains(this.mouseX, this.mouseY)) {
				this.mouseDown = 2;
			} else {
				this.btnMgr.mouseDown();
				return;
			}
		}
		this.mouseMoved(this.mouseX, this.mouseY);
	} else {
		this.btnMgr.mouseDown();
	}
};
Visualizer.prototype.mouseReleased = function() {
	this.mouseDown = 0;
	this.btnMgr.mouseUp();
	this.mouseMoved(this.mouseX, this.mouseY);
};
Visualizer.prototype.mouseExited = function() {
	this.btnMgr.mouseMove(-1, -1);
	this.btnMgr.mouseUp();
	this.mouseDown = 0;
};
Visualizer.prototype.keyPressed = function(key) {
	var d = this.director;
	switch (key) {
		case Key.SPACE:
			d.playStop();
			break;
		case Key.LEFT:
			d.gotoTick(Math.ceil(d.position) - 1);
			break;
		case Key.RIGHT:
			d.gotoTick(Math.floor(d.position) + 1);
			break;
		case Key.PGUP:
			d.gotoTick(Math.ceil(d.position) - 10);
			break;
		case Key.PGDOWN:
			d.gotoTick(Math.floor(d.position) + 10);
			break;
		case Key.HOME:
			d.gotoTick(0);
			break;
		case Key.END:
			d.gotoTick(d.duration);
			break;
		case Key.PLUS:
			this.btnMgr.groups['toolbar'].getButton(6).onclick();
			break;
		case Key.MINUS:
			this.btnMgr.groups['toolbar'].getButton(7).onclick();
			break;
		default:
			switch (String.fromCharCode(key)) {
				case 'F':
					this.setFullscreen(!this.config['fullscreen']);
					break;
			}
	}
};
Visualizer.prototype.keyReleased = function() {
};

// make some exported functions known to Closure Compiler
Visualizer.prototype['loadReplayData'] = Visualizer.prototype.loadReplayData;
Visualizer.prototype['loadReplayDataFromPHP'] = Visualizer.prototype.loadReplayDataFromPHP;
Visualizer.prototype['loadReplayDataFromURI'] = Visualizer.prototype.loadReplayDataFromURI;
