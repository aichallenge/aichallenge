/**
 * @fileoverview This is a visualizer for the ant game.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

/**
 * @todo FEAT: info button showing a message box with game meta data
 * @todo FEAT: menu items: toggle graph/score bars, cpu use
 * @todo FEAT: setting for cpu usage
 * @todo NICE: better player rank display
 * @todo COSMETIC: switch to console.log for debug and load messages
 * @todo COSMETIC: fix duplicate 'parsing replay...' messages
 * @todo COSMETIC: dirty rectangles for drawing
 * @todo COSMETIC: draw only visible ants when zoomed in
 */

/**
 * @namespace Enum for the different states, the visualizer can be in.
 */
LoadingState = {
	/**
	 * The visualizer is not currently loading a replay or map.
	 * 
	 * @const
	 */
	IDLE : 0,
	/**
	 * The visualizer is loading a replay or map and cannot take any load
	 * requests.
	 * 
	 * @const
	 */
	LOADING : 1,
	/**
	 * The visualizer is currently cleaning up.
	 * 
	 * @const
	 * @see Visualizer#cleanUp
	 */
	CLEANUP : 2
};

/**
 * @class A simple location class that stores information about a rectangle.
 * @constructor
 * @param {Number}
 *        x left coordinate
 * @param {Number}
 *        y top coordinate
 * @param {Number}
 *        w width
 * @param {Number}
 *        h height
 */
function Location(x, y, w, h) {
	this.x = x;
	this.y = y;
	this.w = w;
	this.h = h;
}

/**
 * Checks if a given coordinate pair is within the area described by this
 * object.
 * 
 * @param {Number}
 *        x left coordinate
 * @param {Number}
 *        y top coordinate
 * @returns {Boolean} true, if the point is inside the rectangle
 */
Location.prototype.contains = function(x, y) {
	return (x >= this.x && x < this.x + this.w && y >= this.y && y < this.y
			+ this.h);
};

/**
 * @class The main 'application' object that provides all necessary methods for
 *        the use in a web page. Usually you just construct an instance and then
 *        call {@link Visualizer#loadReplayData} or
 *        {@link Visualizer#loadReplayDataFromURI}.
 * @constructor
 * @param {Node}
 *        container the HTML element, that the visualizer will embed into
 * @param {String}
 *        dataDir This relative path to the visualizer data files. You will get
 *        an error message if you forget the tailing '/'.
 * @param {Boolean}
 *        interactive optional, if true or omitted, then the visualizer is
 *        interactive
 * @param {Number}
 *        w an optional maximum width or undefined
 * @param {Number}
 *        h an optional maximum height or undefined
 * @param {Object}
 *        configOverrides an optional configuration; each field overrides the
 *        respective value in the user's configuration or the default; see
 *        {@link Config} for possible options
 */
Visualizer = function(container, dataDir, interactive, w, h, configOverrides) {
	var parameters, equalPos, value, i, text;
	var key = undefined;
	var vis = this;

	/** @private */
	this.loading = LoadingState.LOADING;
	/*
	 * First of all get our logging up and running, so we can print possible
	 * error messages.
	 */
	/** @private */
	this.container = container;
	while (container.hasChildNodes()) {
		container.removeChild(container.lastChild);
	}
	/** @private */
	this.log = document.createElement('div');
	this.container.appendChild(this.log);

	// proceed with initialization
	try {
		/** @private */
		this.map = new CanvasElementMap(this);
		/** @private */
		this.fogPattern = new CanvasElementFogPattern(this);
		/** @private */
		this.fog = new CanvasElementFog(this, this.fogPattern);
		/** @private */
		this.antsMap = new CanvasElementAntsMap(this, this.map, this.fog);
		/** @private */
		this.shiftedMap = new CanvasElementShiftedMap(this, this.antsMap);
		/** @private */
		this.miniMap = new CanvasElementMiniMap(this);
		/** @private */
		this.scores = new CanvasElementStats(this, 'scores', 'scores');
		/** @private */
		this.counts = new CanvasElementStats(this, '# of ants', 'counts');
		/** @private */
		this.turns = undefined;
		/** @private */
		this.w = w;
		/** @private */
		this.h = h;
		/** @private */
		this.loc = {};
		/** @private */
		this.scale = undefined;
		/** @private */
		this.config = new Config();
		if (configOverrides) this.config.overrideFrom(configOverrides);
		/** @private */
		this.options = {};
		this.options['data_dir'] = dataDir;
		this.options['interactive'] = !(interactive === false);
		// read URL parameters and store them in the parameters object
		parameters = window.location.href;
		if ((i = parameters.indexOf('?')) !== -1) {
			parameters = parameters.substr(i + 1).split('#')[0].split('&');
			for (i = 0; i < parameters.length; i++) {
				equalPos = parameters[i].indexOf('=');
				key = parameters[i].substr(0, equalPos);
				value = parameters[i].substr(equalPos + 1);
				if (key === 'debug' || key === 'profile'
						|| key === 'interactive') {
					value = value == 'true' || value == '1';
				} else if (key === 'row' || key === 'col' || key === 'turn') {
					value = parseInt(value);
					if (!(value >= 0)) value = 0;
				} else if (key === 'config') {
					this.config.overrideFrom(JSON.parse(unescape(value)));
				}
				this.options[key] = value;
			}
		}
		// set default zoom to max if we are going to zoom in on a square
		if (this.options['row'] !== undefined
				&& this.options['col'] !== undefined) {
			this.config['zoom'] = 1 << Math.ceil(Math.log(ZOOM_SCALE)
					/ Math.LN2);
		}
		/** @private */
		this.director = new Director(this);
		/** @private */
		this.mouseX = -1;
		/** @private */
		this.mouseY = -1;
		/** @private */
		this.mouseDown = 0;
		/** @private */
		this.mouseOverVis = false;
		/** @private */
		this.shiftX = 0;
		/** @private */
		this.shiftY = 0;
		/** @private */
		this.mapCenterX = 0;
		/** @private */
		this.mapCenterY = 0;
		/** @private */
		this.btnMgr = new ButtonManager(this);

		// print out the configuration
		text = 'Loading visualizer...';
		text += Html.table(function() {
			var table = '';
			var key = undefined;
			for (key in vis.options) {
				var value = vis.options[key];
				table += Html.tr(function() {
					return Html.td(function() {
						return Html.bold(key);
					}) + Html.td(value) + Html.td(function() {
						var result = '<i>';
						if (key === 'data_dir') result += '(Image directory)';
						return result + '</i>';
					});
				});
			}
			return table;
		});
		this.log.innerHTML = text;
		text = (dataDir || '') + 'img/';

		/** @private */
		this.replay = undefined;
		/** @private */
		this.replayStr = undefined;
		/** @private */
		this.replayReq = undefined;
		/**
		 * the main canvas
		 * 
		 * @private
		 */
		this.main = {};
		/**
		 * a hint text overlay
		 * 
		 * @private
		 */
		this.hint = '';
		/** @private */
		this.fogPlayer = undefined;
		/** @private */
		this.isStreaming = false;
		/** @private */
		this.imgMgr = new ImageManager(text, this, this.completedImages);
		this.imgMgr.add('water.png');
		this.imgMgr.add('playback.png');
		this.imgMgr.add('fog.png');
		this.imgMgr.add('toolbar.png');
		this.imgMgr.add('rank.png');
		this.imgMgr.add('graph_options.png');

		// start loading images in the background and wait
		this.loading = LoadingState.IDLE;
		this.imgMgr.startRequests();
	} catch (error) {
		this.exceptionOut(error, false);
		throw error;
	}
}

/**
 * Prints a message on the screen and then executes a function. Usually the
 * screen is not updated until the current thread of execution has finished. To
 * work around that limitation, this method adds the function to be called to
 * the browser's timer queue. Additionally any thrown errors are also printed.
 * 
 * @private
 * @param {String}
 *        log a message to be logged before executing the function
 * @param {Function}
 *        func a function to be called after displaying the message
 */
Visualizer.prototype.progress = function(log, func) {
	if (this.loading !== LoadingState.LOADING) return;
	var vis = this;
	if (log) this.logOut(log);
	window.setTimeout(function() {
		try {
			func();
		} catch (error) {
			vis.exceptionOut(error, true);
			var selectedPosX = 0;
			var selectedPosY = 0;
			var obj = vis.log;
			if (obj.offsetParent) do {
				selectedPosX += obj.offsetLeft;
				selectedPosY += obj.offsetTop;
			} while ((obj = obj.offsetParent))
			window.scrollTo(selectedPosX, selectedPosY);
		}
	}, 50);
};

/**
 * Places a paragraph with a message in the visualizer DOM element.
 * 
 * @private
 * @param {String}
 *        text the message text
 */
Visualizer.prototype.logOut = function(text) {
	this.log.innerHTML += text.replace(/\n/g, '<br>') + '<br>';
};

/**
 * Stops loading, cleans up the instance and calls {@link Visualizer#logOut}
 * with the text in red.
 * 
 * @private
 * @param {string}
 *        text the error message text
 * @param {Boolean}
 *        cleanUp whether the visualizer should try to reset itself; this is
 *        only useful if the error is not coming from the constructor.
 */
Visualizer.prototype.errorOut = function(text, cleanUp) {
	this.logOut('<font style="color:red">' + text + '</font>');
	if (cleanUp) this.cleanUp();
};

/**
 * Converts a JavaScript Error into a HTML formatted string representation and
 * prints that on the web page.
 * 
 * @private
 * @param {Error|String}
 *        error a thrown error or string
 * @param {Boolean}
 *        cleanUp whether the visualizer should try to reset itself; this is
 *        only useful if the error is not coming from the constructor.
 */
Visualizer.prototype.exceptionOut = function(error, cleanUp) {
	var msg;
	var key = undefined;
	if (typeof error == 'string') {
		this.exceptionOut({
			message : error
		}, cleanUp);
		return;
	}
	msg = '<h4><u>' + (error.name ? error.name : 'Error') + '</u></h4>';
	msg += Html.table(function() {
		var escaped;
		var table = '';
		for (key in error) {
			if (key !== 'name') {
				try {
					escaped = new String(error[key]);
					escaped = escaped.replace('&', '&amp;');
					escaped = escaped.replace('<', '&lt;');
					escaped = escaped.replace('>', '&gt;');
					table += Html.tr(function() {
						return Html.td(function() {
							return Html.bold(key);
						}) + Html.td(escaped);
					});
				} catch (e) {
					// catch FireFox UnknownClass-wrapper errors silently
				}
			}
		}
		return table;
	});
	this.errorOut(msg, cleanUp);
};

/**
 * Resets the visualizer and associated objects to an initial state. This method
 * is also called in case of an error.
 * 
 * @private
 */
Visualizer.prototype.cleanUp = function() {
	this.loading = LoadingState.CLEANUP;
	this.imgMgr.cleanUp();
	this.director.cleanUp();
	if (this.replayReq) this.replayReq.abort();
	this.replay = undefined;
	this.replayStr = undefined;
	this.replayReq = undefined;
	if (this.main.canvas) {
		if (this.container.firstChild === this.main.canvas) {
			this.container.removeChild(this.main.canvas);
		}
	}
	this.fogPlayer = undefined;
	this.isStreaming = false;
	document.onkeydown = null;
	document.onkeyup = null;
	document.onkeypress = null;
	window.onresize = null;
	this.log.style.display = 'block';
};

/**
 * This is called before a replay or map is loaded to ensure the visualizer is
 * in an idle state at that time. It then sets the state to {@link LoadingState}.LOADING.
 * 
 * @private
 * @returns {Boolean} true, if the visualizer was idle.
 */
Visualizer.prototype.preload = function() {
	if (this.loading !== LoadingState.IDLE) return true;
	this.cleanUp();
	this.loading = LoadingState.LOADING;
	return false;
};

/**
 * Loads a replay or map file located on the same server using a XMLHttpRequest.
 * 
 * @param {string}
 *        file the relative file name
 */
Visualizer.prototype.loadReplayDataFromURI = function(file) {
	if (this.preload()) return;
	var vis = this;
	this.progress('Fetching replay from: ' + Html.italic(String(file)) + '...',
			function() {
				var request = new XMLHttpRequest();
				vis.replayReq = request;
				request.onreadystatechange = function() {
					if (request.readyState === 4) {
						if (vis.loading === LoadingState.LOADING) {
							if (request.status === 200) {
								vis.replayStr = '' + request.responseText;
								vis.replayReq = undefined;
								vis.loadParseReplay();
							} else {
								vis.errorOut('Status ' + request.status + ': '
										+ request.statusText, true);
							}
						}
					}
				};
				request.open("GET", file);
				if (vis.options['debug']) {
					request.setRequestHeader('Cache-Control', 'no-cache');
				}
				request.send();
				vis.loadCanvas(true);
			});
};
/**
 * Loads a replay string directly.
 * 
 * @param {string}
 *        data the replay string
 */
Visualizer.prototype.loadReplayData = function(data) {
	if (this.preload()) return;
	this.replayStr = data;
	this.loadCanvas(true);
};
Visualizer.prototype.streamingInit = function() {
	this.preload();
	this.isStreaming = true;
	this.replay = new Replay();
	return this.replay;
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
		resume = !this.director.playing()
				&& (this.director.position === this.director.duration);
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
		if (!(vis.replay || vis.replayReq || vis.replayStr)) {
			if (vis.loading !== LoadingState.CLEANUP) {
				throw new Error('Replay is undefined.');
			}
		} else if (vis.replay) { // has just been parsed
			return;
		} else if (vis.replayStr) { // string only
			vis.replay = new Replay(vis.replayStr, vis.options['debug'],
					vis.options['user']);
			vis.replayStr = undefined;
		} else if (vis.replayReq) { // wait for the reply
			return;
		}
		vis.tryStart();
	});
};
/**
 * Creates a canvas element
 * 
 * @private
 */
Visualizer.prototype.loadCanvas = function(prompt) {
	var vis = this;
	this.progress(prompt ? 'Creating canvas...' : undefined, function() {
		if (!vis.main.canvas) {
			vis.main.canvas = document.createElement('canvas');
			vis.main.canvas.style.display = 'none';
		}
		var c = vis.main.canvas;
		vis.main.ctx = c.getContext('2d');
		if (vis.container.firstChild !== c) {
			vis.container.insertBefore(c, vis.log);
		}
		vis.tryStart();
	});
};
/**
 * Called by the ImageManager when no more images are loading
 */
Visualizer.prototype.completedImages = function(error) {
	if (error) {
		this.errorOut(error, true);
	} else {
		this.tryStart();
	}
};
/**
 * Checks if we have a drawing context (canvas/applet), the images and the
 * replay. If all components are loaded it starts playback.
 */
Visualizer.prototype.tryStart = function() {
	var bg, stop, i;
	var vis = this;
	// we need to parse the replay, unless it has been parsed by the
	// XmlHttpRequest callback
	if (this.replay) {
		if (this.main.ctx && !this.imgMgr.error && !this.imgMgr.pending) {
			vis = this;
			if (this.options['interactive']) {
				// add static buttons
				if (!vis.btnMgr.groups['playback']) {
					if (this.replay.hasDuration) {
						bg = vis.btnMgr.addImageGroup('playback',
								vis.imgMgr.images[1],
								ImageButtonGroup.HORIZONTAL,
								ButtonGroup.MODE_NORMAL, 2, 0);
						bg.addButton(3, function() {
							vis.director.gotoTick(0);
						}, 'jump to start of first turn');
						bg.addSpace(32);
						bg
								.addButton(
										5,
										function() {
											stop = (Math
													.ceil(vis.director.position * 2) - 1) / 2;
											vis.director.slowmoTo(stop);
										},
										'play one move/attack phase backwards');
						// bg.addButton(0, function()
						// {vis.director.playStop()});
						bg.addSpace(64);
						bg.addButton(4, function() {
							vis.director.playStop();
						}, 'play/stop the game');
						// drawImage(this.imgMgr.images[1], 1 * 64, 0, 64, 64, x
						// + 4.5 * 64, y, 64, 64);
						bg.addSpace(64);
						bg
								.addButton(
										6,
										function() {
											var stop = (Math
													.floor(vis.director.position * 2) + 1) / 2;
											vis.director.slowmoTo(stop);
										}, 'play one move/attack phase');
						bg.addSpace(32);
						bg.addButton(2, function() {
							vis.director.gotoTick(vis.director.duration);
						}, 'jump to end of the last turn');
					}
					bg = vis.btnMgr.addImageGroup('toolbar',
							vis.imgMgr.images[3], ImageButtonGroup.VERTICAL,
							ButtonGroup.MODE_NORMAL, 2, 0);
					if (this.config.hasLocalStorage()) {
						bg.addButton(0, function() {
							vis.config.save();
						}, 'save and reuse the current settings');
					}
					if (!window.isFullscreenSupported
							|| window.isFullscreenSupported()) {
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
						} while (vis.scale == oldScale
								&& vis.config['zoom'] > 1);
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
					bg
							.addButton(
									5,
									function() {
										vis
												.setAntLabels((vis.config['label'] + 1) % 3);
										vis.director.draw();
									},
									'toggles: 1. player letters on ants, 2. global ids on ants');
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
				var colors = [ SAND_COLOR ];
				for (i = 0; i < this.replay.players; i++) {
					colors.push(this.replay.meta['playercolors'][i]);
				}
				this.imgMgr.colorize(4, colors);
				this.imgMgr.colorize(2, colors);
				if (this.replay.hasDuration) {
					bg = this.btnMgr.addImageGroup('fog',
							this.imgMgr.patterns[2], ImageButtonGroup.VERTICAL,
							ButtonGroup.MODE_RADIO, 2);
					var buttonAdder = function(fog) {
						return bg
								.addButton(
										i,
										function() {
											vis.showFog(fog);
										},
										(i == 0)
												? 'clear fog of war'
												: 'show fog of war for '
														+ vis.replay.meta['playernames'][i - 1]);
					};
					for ( var i = 0; i < colors.length; i++) {
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
					btn.offset = (vis.director.playing() ? 7 : 4)
							* vis.imgMgr.images[1].height;
					if (btn === vis.btnMgr.pinned) {
						vis.btnMgr.pinned = null;
					}
					btn.mouseUp();
				};
				// this will fire once in FireFox when a key is held down
				document.onkeydown = function(event) {
					if (!(event.shiftKey || event.ctrlKey || event.altKey || event.metaKey)) {
						if (vis.keyPressed(event.keyCode)) {
							if (event.preventDefault)
								event.preventDefault();
							else
								event.returnValue = false;
							return false;
						}
					}
					return true;
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
				} while ((obj = obj.offsetParent))
				mx = event.clientX
						- mx
						+ ((window.scrollX === undefined)
								? (document.body.parentNode.scrollLeft !== undefined)
										? document.body.parentNode.scrollLeft
										: document.body.scrollLeft
								: window.scrollX);
				my = event.clientY
						- my
						+ ((window.scrollY === undefined)
								? (document.body.parentNode.scrollTop !== undefined)
										? document.body.parentNode.scrollTop
										: document.body.scrollTop
								: window.scrollY);
				vis.mouseMoved(mx, my);
			};
			this.main.canvas.onmouseout = function() {
				vis.mouseExited();
			};
			this.main.canvas.onmousedown = function(event) {
				if (event.which === 1) {
					vis.mousePressed();
				}
			};
			this.main.canvas.onmouseup = function(event) {
				if (event.which === 1) {
					vis.mouseReleased();
				}
			};
			window.onresize = function() {
				vis.resize();
			};
			Visualizer.prototype.focused = this;
			// move to a specific row and col
			if (this.options['row'] !== undefined
					&& this.options['col'] !== undefined) {
				this.shiftX = this.mapCenterX = (0.5 * this.replay.cols - 0.5)
						* ZOOM_SCALE - (this.options['col'] % this.replay.cols)
						* ZOOM_SCALE;
				this.shiftY = this.mapCenterY = (0.5 * this.replay.rows - 0.5)
						* ZOOM_SCALE - (this.options['row'] % this.replay.rows)
						* ZOOM_SCALE;
			}
			this.log.style.display = 'none';
			this.main.canvas.style.display = 'inline';
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
	} else if (!this.replayReq) {
		this.loadParseReplay();
	}
};
Visualizer.prototype.addPlayerButtons = function() {
	var scores, i;
	var bg = this.btnMgr.addTextGroup('players', ButtonGroup.MODE_NORMAL, 2);
	var vis = this;
	var func = undefined;
	var gameId = this.replay.meta['game_id'] || this.options['game'];
	if (gameId !== undefined) {
		if (this.replay.meta['game_url']) {
			func = function() {
				window.location.href = vis.replay.meta['game_url'].replace('~',
						gameId);
			};
		}
		bg.addButton('Game #' + gameId + ':', '#000', func);
	} else {
		bg.addButton('Players:', '#000', undefined);
	}
	if (this.replay.meta['replaydata']['bonus']) {
		scores = new Array(this.replay.players);
		for (i = 0; i < this.replay.players; i++) {
			scores[i] = this.replay['scores'][this.replay.duration][i];
			scores[i] += this.replay.meta['replaydata']['bonus'][i];
		}
	} else {
		scores = this.replay['scores'][this.replay.duration];
	}
	var ranks = new Array(scores.length);
	var order = new Array(scores.length);
	for (i = 0; i < scores.length; i++) {
		ranks[i] = 1;
		for ( var k = 0; k < scores.length; k++) {
			if (scores[i] < scores[k]) {
				ranks[i]++;
			}
		}
		k = ranks[i] - 1;
		while (order[k] !== undefined)
			k++;
		order[k] = i;
	}
	var buttonAdder = function(idx) {
		var color = vis.replay.htmlPlayerColors[idx];
		var func = null;
		if (vis.replay.meta['user_url'] && vis.replay.meta['user_ids']
				&& vis.replay.meta['user_ids'][idx] !== undefined) {
			func = function() {
				window.location.href = vis.replay.meta['user_url'].replace('~',
						vis.replay.meta['user_ids'][idx]);
			};
		}
		var caption = vis.replay.meta['playernames'][idx];
		caption = ranks[idx] + '. ' + caption;
		if (vis.replay.meta['status']) {
			caption += ' (' + vis.statusToGlyph(idx) + ')';
		}
		bg.addButton(caption, color, func);
	};
	for (i = 0; i < this.replay.players; i++) {
		buttonAdder(order[i]);
	}
};
Visualizer.prototype.setReplaySpeed = function() {
	var speed = this.director.duration / this.config['duration'];
	speed = Math.max(speed, this.config['speedSlowest']);
	speed = Math.min(speed, this.config['speedFastest']);
	this.director.defaultSpeed = speed
			* Math.pow(1.5, this.config['speedFactor']);
	if (this.director.speed !== 0) {
		this.director.speed = this.director.defaultSpeed;
	}
	var hintText = function(base) {
		return 'set speed modifier to ' + ((base > 0) ? '+' + base : base);
	};
	if (this.options['interactive'] && this.replay.hasDuration) {
		var speedUpBtn = this.btnMgr.groups['toolbar'].getButton(6);
		speedUpBtn.hint = hintText(this.config['speedFactor'] + 1);
		var slowDownBtn = this.btnMgr.groups['toolbar'].getButton(7);
		slowDownBtn.hint = hintText(this.config['speedFactor'] - 1);
	}
};
Visualizer.prototype.calculateCanvasSize = function() {
	var result = {};
	if (typeof (window.innerWidth) == 'number') {
		// Non-IE
		result.width = window.innerWidth;
		result.height = window.innerHeight;
	}
	var embed = (window.isFullscreenSupported && !window
			.isFullscreenSupported())
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
	var effectiveZoom = Math.max(1, zoom);
	this.config['zoom'] = effectiveZoom;
	this.scale = Math.max(1, Math.min((this.loc.vis.w - 20)
			/ (this.replay.cols), (this.loc.vis.h - 20) / (this.replay.rows))) | 0;
	this.scale = Math.min(ZOOM_SCALE, this.scale * effectiveZoom);
	if (oldScale) {
		this.shiftX = (this.shiftX * this.scale / oldScale) | 0;
		this.shiftY = (this.shiftY * this.scale / oldScale) | 0;
	}
	this.map.setSize(this.scale * this.replay.cols, this.scale
			* this.replay.rows);
	this.map.x = ((this.loc.vis.w - this.map.w) / 2 + this.loc.vis.x) | 0;
	this.map.y = ((this.loc.vis.h - this.map.h) / 2 + this.loc.vis.y) | 0;
	this.antsMap.setSize(this.map.w, this.map.h);
	this.fog.setSize(Math.min(this.map.w, this.loc.vis.w), Math.min(this.map.h,
			this.loc.vis.h));
	if (this.options['interactive']) {
		var zoomInBtn = this.btnMgr.groups['toolbar'].getButton(2);
		zoomInBtn.enabled = !(this.scale === ZOOM_SCALE);
		zoomInBtn.draw();
		var zoomOutBtn = this.btnMgr.groups['toolbar'].getButton(3);
		zoomOutBtn.enabled = !(effectiveZoom === 1);
		zoomOutBtn.draw();
	}
};
Visualizer.prototype.setAntLabels = function(mode) {
	this.config['label'] = mode;
};
Visualizer.prototype.resize = function(forced) {
	var y, w;
	var olds = {
		width : this.main.canvas.width,
		height : this.main.canvas.height
	};
	var newSize = this.calculateCanvasSize();
	var resizing = newSize.width != olds.width || newSize.height != olds.height;
	if (resizing || forced) {
		var canvas = this.main.canvas;
		var ctx = this.main.ctx;
		if (resizing) {
			canvas.width = newSize.width;
			canvas.height = newSize.height;
			ctx.fillStyle = '#fff';
			ctx.fillRect(0, 0, canvas.width, canvas.height);
		}
		if (this.replay.hasDuration) {
			// 1. player buttons
			y = this.btnMgr.groups['players'].cascade(newSize.width) + 4;
			// 2. scores bar & time line
			this.scores.x = 0;
			this.scores.y = y;
			this.scores.setSize(newSize.width, CanvasElementStats.MIN_HEIGHT);
			this.counts.x = 0;
			this.counts.y = this.scores.y + this.scores.h + 4;
			this.counts.setSize(newSize.width, CanvasElementStats.MAX_HEIGHT);
			y = this.counts.y + this.counts.h;
		} else {
			y = 0;
		}
		// 3. visualizer placement
		if (this.options['interactive']) {
			if (this.replay.hasDuration) {
				this.loc.vis = new Location(LEFT_PANEL_W, y, newSize.width
						- LEFT_PANEL_W - RIGHT_PANEL_W, newSize.height - y
						- BOTTOM_PANEL_H);
				var bg = this.btnMgr.groups['playback'];
				w = 8 * 64;
				if (w <= newSize.width) {
					bg.x = ((newSize.width - w) / 2) | 0;
				} else {
					bg.x = 0;
				}
				bg.y = this.loc.vis.y + this.loc.vis.h;
				bg = this.btnMgr.groups['fog'];
				bg.y = this.loc.vis.y + 8;
			} else {
				this.loc.vis = new Location(0, y,
						newSize.width - RIGHT_PANEL_W, newSize.height - y);
			}
			bg = this.btnMgr.groups['toolbar'];
			bg.x = this.loc.vis.x + this.loc.vis.w;
			bg.y = this.loc.vis.y + 8;
			// set button group extents
			if (this.replay.hasDuration) {
				bg = this.btnMgr.groups['fog'];
				bg.h = newSize.height - this.loc.vis.y - 8;
				bg = this.btnMgr.groups['playback'];
				bg.w = newSize.width - 2 * 48;
			}
			bg = this.btnMgr.groups['toolbar'];
			bg.h = newSize.height - this.loc.vis.y - 8;
		} else {
			this.loc.vis = new Location(0, y, newSize.width, newSize.height - y);
		}
		this.shiftedMap.setSize(this.loc.vis.w, this.loc.vis.h);
		this.setZoom(this.config['zoom']);
				this.miniMap.x = this.loc.vis.x + this.loc.vis.w - 2
						- this.replay.cols, this.miniMap.y = this.loc.vis.y + 2;
		this.miniMap.setSize(this.replay.cols, this.replay.rows);
		// redraw everything
		this.btnMgr.draw();
		this.director.draw(true);
	}
};
Visualizer.prototype.statusToGlyph = function(i) {
	var status = this.replay.meta['status'][i];
	if (status === 'survived') {
		return '\u2713';
	} else if (status === 'eliminated') {
		return '\u2717';
	} else {
		return this.replay.meta['status'][i];
	}
};
Visualizer.prototype.showFog = function(fogPlayer) {
	if (fogPlayer === undefined) {
		this.fogPlayer = undefined;
	} else {
		this.fogPlayer = fogPlayer;
	}
	this.director.draw();
};
/**
 * @private
 */
Visualizer.prototype.draw = function() {
	var ctx, w, h, mx, my, x, y/* , ar, sr */;
	var loc = this.loc.vis;

	// map
	this.shiftedMap.validate();
	this.main.ctx.drawImage(this.shiftedMap.canvas, loc.x, loc.y);

	// mouse cursor (super complicated position calculation)
	ctx = this.main.ctx;
	if (this.mouseOverVis) {
		ctx.save();
		ctx.beginPath();
		ctx.rect(loc.x, loc.y, loc.w, loc.h);
		ctx.clip();
		mx = this.mouseX - this.map.x - this.shiftX;
		my = this.mouseY - this.map.y - this.shiftY;
		mx = Math.floor(mx / this.scale) * this.scale + this.map.x
				+ this.shiftX;
		my = Math.floor(my / this.scale) * this.scale + this.map.y
				+ this.shiftY;
		mx -= loc.x - this.scale + 1;
		my -= loc.y - this.scale + 1;
		mx = Math.wrapAround(mx, this.map.w);
		my = Math.wrapAround(my, this.map.h);
		mx += loc.x - this.scale + 1;
		my += loc.y - this.scale + 1;
		ctx.strokeStyle = '#fff';
		ctx.beginPath();
		for (y = my; y < loc.y + loc.h; y += this.map.h) {
			for (x = mx; x < loc.x + loc.w; x += this.map.w) {
				ctx.rect(x + 0.5, y + 0.5, this.scale - 1, this.scale - 1);
			}
		}
		ctx.stroke();
		ctx.restore();
	}

	// minimap
	if (this.config['zoom'] !== 1 && this.miniMap.h + 4 <= loc.h
			&& this.miniMap.w + 4 <= loc.w) {
		this.miniMap.validate();
		ctx.save();
		// border
		ctx.strokeStyle = '#fff';
		ctx.lineWidth = 2;
		ctx.beginPath();
		ctx.rect(this.miniMap.x - 1, this.miniMap.y - 1, this.miniMap.w + 2,
				this.miniMap.h + 2);
		ctx.stroke();
		// map
		ctx.drawImage(this.miniMap.canvas, this.miniMap.x, this.miniMap.y);
		// position indicator
		ctx.beginPath();
		ctx
				.rect(this.miniMap.x, this.miniMap.y, this.miniMap.w,
						this.miniMap.h);
		ctx.clip();
		w = loc.w / this.scale;
		h = loc.h / this.scale;
		x = this.replay.cols / 2 - this.shiftX / this.scale - w / 2;
		y = this.replay.rows / 2 - this.shiftY / this.scale - h / 2;
		x -= Math.floor(x / this.replay.cols) * this.replay.cols;
		y -= Math.floor(y / this.replay.rows) * this.replay.rows;
		ctx.beginPath();
		ctx.rect(this.miniMap.x + x, this.miniMap.y + y, w, h);
		ctx.rect(this.miniMap.x + x - this.replay.cols, this.miniMap.y + y, w,
				h);
		ctx.rect(this.miniMap.x + x, this.miniMap.y + y - this.replay.rows, w,
				h);
		ctx.rect(this.miniMap.x + x - this.replay.cols, this.miniMap.y + y
				- this.replay.rows, w, h);
		ctx.stroke();
		ctx.restore();
	}

	if (this.replay.hasDuration) {
		if (this.scores.validate()) {
			ctx.drawImage(this.scores.canvas, this.scores.x, this.scores.y);
		}
		if (this.counts.validate()) {
			ctx.drawImage(this.counts.canvas, this.counts.x, this.counts.y);
		}
	}

	// draw hint text
	var hint = this.hint;
	if (hint) {
		ctx.font = FONT;
		ctx.textAlign = 'left';
		ctx.textBaseline = 'middle';
		if (ctx.measureText(hint).width > this.loc.vis.w) {
			do {
				hint = hint.substr(0, hint.length - 1);
			} while (hint
					&& ctx.measureText(hint + '...').width > this.loc.vis.w);
			if (hint) hint += '...';
		}
		w = ctx.measureText(hint).width;
		ctx.fillStyle = 'rgba(0,0,0,0.3)';
		ctx.fillRect(loc.x, loc.y, w, 22);
		ctx.fillStyle = '#fff';
		ctx.fillText(hint, loc.x, loc.y + 10);
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
	var oldHint = this.hint;
	var btn = null;
	this.mouseX = mx;
	this.mouseY = my;
	this.mouseCol = (Math.wrapAround(mx - this.map.x - this.shiftX, this.scale
			* this.replay.cols) / this.scale) | 0;
	this.mouseRow = (Math.wrapAround(my - this.map.y - this.shiftY, this.scale
			* this.replay.rows) / this.scale) | 0;
	this.hint = '';
	if (this.options['interactive']) {
		if ((this.mouseOverVis = this.map.contains(this.mouseX, this.mouseY)
				&& this.loc.vis.contains(this.mouseX, this.mouseY))) {
			this.hint = 'row ' + this.mouseRow + ' | col ' + this.mouseCol;
		}
		if (this.mouseDown === 1) {
			mx = (this.mouseX - this.scores.graph.x)
					/ (this.scores.graph.w - 1);
			mx = Math.round(mx * this.replay.duration);
			this.director.gotoTick(mx);
		} else if (this.mouseDown === 2
				|| (this.mouseDown === 3 && this.miniMap.contains(this.mouseX,
						this.mouseY))) {
			if (this.mouseDown === 2) {
				this.shiftX += deltaX;
				this.shiftY += deltaY;
			} else {
				this.shiftX = (this.replay.cols / 2 - (this.mouseX - this.miniMap.x))
						* this.scale;
				this.shiftY = (this.replay.rows / 2 - (this.mouseY - this.miniMap.y))
						* this.scale;
			}
			var centerBtn = this.btnMgr.groups['toolbar'].getButton(4);
			centerBtn.enabled = this.shiftX !== 0 || this.shiftY !== 0;
			centerBtn.draw();
			this.director.draw();
		} else {
			btn = this.btnMgr.mouseMove(mx, my);
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
		if (this.replay.hasDuration
				&& (this.counts.graph.contains(this.mouseX, this.mouseY) || this.scores.graph
						.contains(this.mouseX, this.mouseY))) {
			this.mouseDown = 1;
		} else {
			if (this.config['zoom'] !== 1
					&& this.miniMap.contains(this.mouseX, this.mouseY)) {
				this.mouseDown = 3;
			} else if (this.loc.vis.contains(this.mouseX, this.mouseY)) {
				this.mouseDown = 2;
			} else {
				this.btnMgr.mouseDown();
				return;
			}
		}
		this.mouseMoved(this.mouseX, this.mouseY);
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
		case Key.PLUS_OPERA:
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
				default:
					return false;
			}
	}
	return true;
};

// getters for the drawing functions in CanvasElement

Visualizer.prototype.getRows = function() {
	return this.replay.rows;
};
Visualizer.prototype.getCols = function() {
	return this.replay.cols;
};
Visualizer.prototype.getScale = function() {
	return this.scale;
};
Visualizer.prototype.getImage = function(id) {
	return this.imgMgr.images[id];
};
Visualizer.prototype.isWall = function(row, col) {
	return this.replay.walls[row][col];
};
Visualizer.prototype.getOption = function(name) {
	return this.options[name];
};
Visualizer.prototype.getConfig = function(name) {
	return this.config[name];
};
Visualizer.prototype.getTime = function() {
	return this.director.position;
};
Visualizer.prototype.getShiftX = function() {
	return this.shiftX;
};
Visualizer.prototype.getShiftY = function() {
	return this.shiftY;
};
Visualizer.prototype.getFogPlayer = function() {
	return this.fogPlayer;
};
Visualizer.prototype.getTurn = function(turn) {
	return this.replay.getTurn(turn);
};
Visualizer.prototype.getSpawnRadius2 = function() {
	return this.replay.meta['replaydata']['spawnradius2'];
};
Visualizer.prototype.getAttackRadius2 = function() {
	return this.replay.meta['replaydata']['attackradius2'];
};
Visualizer.prototype.getFogMap = function() {
	return this.replay.getFog(this.fogPlayer, this.director.position | 0);
};
Visualizer.prototype.getHtmlPlayerColor = function(player) {
	return this.replay.htmlPlayerColors[player];
};
Visualizer.prototype.getReplayDuration = function() {
	return this.replay.duration;
};
Visualizer.prototype.getReplay = function() {
	return this.replay;
};
Visualizer.prototype.getStats = function(name) {
	return {
		values : this.replay[name],
		bonus : name === 'scores' ? this.replay.meta['replaydata']['bonus']
				: undefined
	};
};
Visualizer.prototype.getMouseOverVis = function() {
	return this.mouseOverVis;
};
Visualizer.prototype.getMouseCol = function() {
	return this.mouseCol;
};
Visualizer.prototype.getMouseRow = function() {
	return this.mouseRow;
};

// make some exported functions known to JS minifiers

Visualizer.prototype['loadReplayData'] = Visualizer.prototype.loadReplayData;
Visualizer.prototype['loadReplayDataFromURI'] = Visualizer.prototype.loadReplayDataFromURI;
