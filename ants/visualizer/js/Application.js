/**
 * @fileoverview This is a visualizer for the ant game.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 */

// TODO: FEAT: info button showing a message box with game meta data
// TODO: FEAT: menu items: toggle graph/score bars, cpu use
// TODO: FEAT: setting for cpu usage
// TODO: NICE: better player rank display
// TODO: COSMETIC: draw only visible ants when zoomed in
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
	 * The visualizer is loading a replay or map and cannot take any load requests.
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
 * @class The main 'application' object that provides all necessary methods for the use in a web
 *        page. Usually you just construct an instance and then call
 *        {@link Visualizer#loadReplayData} or {@link Visualizer#loadReplayDataFromURI}.
 * @constructor
 * @param {Node}
 *        container the HTML element, that the visualizer will embed into
 * @param {Options}
 *        options Adds immutable options. These can be overridden via URL parameters. The visualizer
 *        will not copy this {@link Options} instance, but instead use it directly. Modifications to
 *        the object at a later point will result in undefined behavior.
 * @param {Number}
 *        w an optional maximum width or undefined
 * @param {Number}
 *        h an optional maximum height or undefined
 * @param {Object}
 *        configOverrides an optional configuration; each field overrides the respective value in
 *        the user's configuration or the default; see {@link Config} for possible options
 */
Visualizer = function(container, options, w, h, configOverrides) {
	var parameters, equalPos, value, i, text, imgDir;
	var key = undefined;
	var vis = this;
	/** @private */
	this.loading = LoadingState.LOADING;
	/*
	 * First of all get our logging up and running, so we can print possible error messages.
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
		this.state = new State();
		/** @private */
		this.map = new CanvasElementMap(this.state);
		/** @private */
		this.fogPattern = new CanvasElementFogPattern(this.state);
		/** @private */
		this.fog = new CanvasElementFog(this.state, this.fogPattern);
		/** @private */
		this.antsMap = new CanvasElementAntsMap(this.state, this.map, this.fog);
		/** @private */
		this.shiftedMap = new CanvasElementShiftedMap(this.state, this.antsMap);
		/** @private */
		this.miniMap = new CanvasElementMiniMap(this.state);
		/** @private */
		this.turns = undefined;
		/** @private */
		this.w = w;
		/** @private */
		this.h = h;
		/** @private */
		this.resizing = false;
		if (configOverrides) this.state.config.overrideFrom(configOverrides);
		/** @private */
		this.state.options = options;
		// read URL parameters and store them in the parameters object
		parameters = window.location.href;
		if ((i = parameters.indexOf('?')) !== -1) {
			parameters = parameters.substr(i + 1).split('#')[0].split('&');
			for (i = 0; i < parameters.length; i++) {
				equalPos = parameters[i].indexOf('=');
				key = parameters[i].substr(0, equalPos);
				value = parameters[i].substr(equalPos + 1);
				switch (key) {
				case 'debug':
					this.state.options['debug'] = Options.toBool(value);
					break;
				case 'interactive':
					this.state.options['interactive'] = Options.toBool(value);
					break;
				case 'profile':
					this.state.options['profile'] = Options.toBool(value);
					break;
				case 'decorated':
					this.state.options['decorated'] = Options.toBool(value);
					break;
				case 'col':
					this.state.options['col'] = parseInt(value);
					break;
				case 'row':
					this.state.options['row'] = parseInt(value);
					break;
				case 'turn':
					this.state.options['turn'] = parseInt(value);
					break;
				case 'data_dir':
					this.state.options['data_dir'] = value;
					break;
				case 'game':
					this.state.options['game'] = value;
					break;
				case 'user':
					this.state.options['user'] = value;
					break;
				case 'config':
					this.state.config.overrideFrom(JSON.parse(unescape(value)));
				}
			}
		}
		// set default zoom to max if we are going to zoom in on a square
		if (!isNaN(this.state.options['row']) && !isNaN(this.state.options['col'])) {
			this.state.config['zoom'] = 1 << Math.ceil(Math.log(ZOOM_SCALE) / Math.LN2);
		}
		imgDir = (this.state.options['data_dir'] || '') + 'img/';
		/** @private */
		this.imgMgr = new ImageManager(imgDir, new Delegate(this, this.completedImages));
		this.imgMgr.add('water.png');
		this.imgMgr.add('hill.png');
		if (this.state.options['decorated']) {
			this.imgMgr.add('playback.png');
			this.imgMgr.add('fog.png');
			this.imgMgr.add('toolbar.png');
			/** @private */
			this.btnMgr = new ButtonManager(null);
			/** @private */
			this.scores = new CanvasElementStats(this.state, 'scores', 'scores', 'bonus');
			/** @private */
			this.counts = new CanvasElementStats(this.state, '# of ants', 'counts', 'hive');
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
		this.mapCenterX = 0;
		/** @private */
		this.mapCenterY = 0;
		/** @private */
		this.progressList = [];

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

		// start loading images in the background and wait
		this.loading = LoadingState.IDLE;
		this.imgMgr.startRequests();
	} catch (error) {
		this.exceptionOut(error, false);
		throw error;
	}
};

/**
 * Prints a message on the screen and then executes a function. Usually the screen is not updated
 * until the current thread of execution has finished. To work around that limitation, this method
 * adds the function to be called to the browser's timer queue. Additionally any thrown errors are
 * also printed.
 * 
 * @private
 * @param {String}
 *        log a message to be logged before executing the function
 * @param {Function}
 *        func a function to be called after displaying the message
 * @param {String}
 *        id An identification of the progress that will be used to filter duplicates in the queue.
 */
Visualizer.prototype.progress = function(log, func, id) {
	var i;
	if (this.loading !== LoadingState.LOADING) return;
	for (i = 0; i < this.progressList.length; i++) {
		if (id === this.progressList[i]) return;
	}
	this.progressList.push(id);
	var vis = this;
	if (log) this.logOut(log);
	window.setTimeout(function() {
		var k;
		try {
			func();
			for (k = 0; k < vis.progressList.length; k++) {
				if (id === vis.progressList[k]) {
					vis.progressList.splice(k, 1);
					break;
				}
			}
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
 * Stops loading, cleans up the instance and calls {@link Visualizer#logOut} with the text in red.
 * 
 * @private
 * @param {string}
 *        text the error message text
 * @param {Boolean}
 *        cleanUp whether the visualizer should try to reset itself; this is only useful if the
 *        error is not coming from the constructor.
 */
Visualizer.prototype.errorOut = function(text, cleanUp) {
	this.logOut('<font style="color:red">' + text + '</font>');
	if (cleanUp) this.cleanUp();
};

/**
 * Converts a JavaScript Error into a HTML formatted string representation and prints that on the
 * web page.
 * 
 * @private
 * @param {Error|String}
 *        error a thrown error or string
 * @param {Boolean}
 *        cleanUp whether the visualizer should try to reset itself; this is only useful if the
 *        error is not coming from the constructor.
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
 * Resets the visualizer and associated objects to an initial state. This method is also called in
 * case of an error.
 * 
 * @private
 */
Visualizer.prototype.cleanUp = function() {
	this.loading = LoadingState.CLEANUP;
	if (this.replayReq) this.replayReq.abort();
	if (this.state.options['decorated']) {
		this.imgMgr.cleanUp();
	}
	this.director.cleanUp();
	this.state.cleanUp();
	this.replayStr = undefined;
	this.replayReq = undefined;
	if (this.main.canvas) {
		if (this.container.firstChild === this.main.canvas) {
			this.container.removeChild(this.main.canvas);
		}
	}
	this.resizing = false;
	document.onkeydown = null;
	document.onkeyup = null;
	document.onkeypress = null;
	window.onresize = null;
	this.log.style.display = 'block';
};

/**
 * This is called before a replay or map is loaded to ensure the visualizer is in an idle state at
 * that time. It then sets the state to {@link LoadingState}.LOADING.
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
	this.progress('Fetching replay from: ' + Html.italic(String(file)) + '...', function() {
		var request = new XMLHttpRequest();
		vis.replayReq = request;
		/** @ignore */
		request.onreadystatechange = function() {
			if (request.readyState === 4) {
				if (vis.loading === LoadingState.LOADING) {
					if (request.status === 200) {
						vis.replayStr = '' + request.responseText;
						vis.replayReq = undefined;
						vis.loadParseReplay();
					} else {
						vis.errorOut('Status ' + request.status + ': ' + request.statusText, true);
					}
				}
			}
		};
		request.open("GET", file);
		if (vis.state.options['debug']) {
			request.setRequestHeader('Cache-Control', 'no-cache');
		}
		request.send();
		vis.loadCanvas();
	}, "FETCH");
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
	this.loadCanvas();
};

/**
 * This initializes the visualizer to accept data pushed to it from the Java wrapper. For
 * performance reasons native Java code in Stream.java will incrementally fill up the replay. Since
 * visualizer is really slow in some situations, it used to be unresponsive when streaming at a high
 * rate. To avoid that it now requests a single new turn from the wrapper for every rendered frame.<br>
 * <b>Called by the Java streaming visualizer.</b>
 * 
 * @returns {Replay} the replay object is exposed to the Java wrapper
 */
Visualizer.prototype.streamingInit = function() {
	this.preload();
	this.state.isStreaming = true;
	return this.state.replay = new Replay();
};

/**
 * This is called internally by {@link Visualizer#draw} to request the next turn after a rendered
 * frame (throttling the data stream) and as a one-time, asynchronous call from Stream.java, where
 * it is issued after the Java wrapper has set up a global 'stream' variable in the JavaScript
 * context that refers to the Java Stream instance. First the visualizer will request new data from
 * the wrapper and set state.isStreaming to false if the game ended. Then, if the visualizer has not
 * finished loading yet, it will fully initialize itself based on the replay data or start/resume
 * playback.<br>
 * <b>Called by the Java streaming visualizer.</b>
 */
Visualizer.prototype.streamingStart = function() {
	this.state.isStreaming = stream.visualizerReady();
	if (this.loading === LoadingState.LOADING) {
		if (this.state.replay.hasDuration) {
			// set CPU to 100%, we need it
			this.director.cpu = 1;
			this.loadCanvas();
		}
	} else {
		// call resize() in forced mode to update the GUI (graphs)
		this.resize(true);
		// resume playback if we are at the end
		resume = !this.director.playing() && (this.state.time === this.director.duration);
		if (this.director.stopAt === this.director.duration) {
			this.director.stopAt = this.state.replay.duration;
		}
		this.director.duration = this.state.replay.duration;
		if (resume) {
			this.director.play();
		}
	}
};

/**
 * Makes the visualizer output video frames at a fixed rate
 * 
 * @param {Number}
 *        fpt the number of frames per turn
 */
Visualizer.prototype.javaVideoOutput = function(fpt) {
	this.director.fixedFpt = fpt;
};

/**
 * In this method the replay string that has been passed directly or downloaded is parsed into a
 * {@link Replay}. Afterwards an attempt is made to start the visualization ({@link Visualizer#tryStart}).
 * 
 * @private
 */
Visualizer.prototype.loadParseReplay = function() {
	var vis = this;
	this.progress('Parsing the replay...', function() {
		var debug = vis.state.options['debug'];
		var user = vis.state.options['user'];
		if (user === '') user = undefined;
		if (vis.replayStr) {
			vis.state.replay = new Replay(vis.replayStr, debug, user);
			vis.replayStr = undefined;
		} else if (vis.loading !== LoadingState.CLEANUP) {
			throw new Error('Replay is undefined.');
		}
		vis.tryStart();
	}, "PARSE");
};

/**
 * Creates the main canvas element and insert it into the web page. An attempt is made to start the
 * visualization ({@link Visualizer#tryStart}).
 * 
 * @private
 */
Visualizer.prototype.loadCanvas = function() {
	var vis = this;
	this.progress('Creating canvas...', function() {
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
	}, "CANVAS");
};

/**
 * Called by the ImageManager when no more images are loading. Since image loading is a background
 * operation, an attempt is made to start the visualization ({@link Visualizer#tryStart}). If some
 * images didn't load, the visualizer is stopped with an error message.
 * 
 * @param error
 *        {String} Contains the error message for images that didn't load or is empty.
 */
Visualizer.prototype.completedImages = function(error) {
	if (error) {
		this.errorOut(error, true);
	} else {
		this.map.water = this.imgMgr.images[0];
		this.miniMap.water = this.map.water;
		this.tryStart();
	}
};

/**
 * Checks if we have a drawing context (canvas/applet), the images and the replay. If all components
 * are loaded, some remaining items that depend on them are created and playback is started.
 * tryStart() is called after any long during action that runs in the background, like downloading
 * images and the replay to check if that was the last missing component.
 * 
 * @private
 */
Visualizer.prototype.tryStart = function() {
	var bg, i, k, dlg, scores;
	var vis = this;
	// we need to parse the replay, unless it has been parsed by the
	// XmlHttpRequest callback
	if (this.state.replay) {
		if (!this.main.ctx) return;
		if (this.imgMgr.pending !== 0) return;
		// colorize ant hill
		colors = [];
		for (i = 0; i < this.state.replay.players; i++) {
			colors.push(this.state.replay.meta['playercolors'][i]);
		}
		this.imgMgr.colorize(1, colors);
		this.antsMap.setHillImage(this.imgMgr.patterns[1]);
		// add GUI
		if (this.state.options['decorated']) {
			if (this.imgMgr.error) return;
			if (this.imgMgr.pending) return;
			this.btnMgr.ctx = this.main.ctx;
			// calculate player order
			if (this.state.replay.meta['replaydata']['bonus']) {
				scores = new Array(this.state.replay.players);
				for (i = 0; i < this.state.replay.players; i++) {
					scores[i] = this.state.replay['scores'][this.state.replay.duration][i];
					scores[i] += this.state.replay.meta['replaydata']['bonus'][i];
				}
			} else {
				scores = this.state.replay['scores'][this.state.replay.duration];
			}
			this.state.ranks = new Array(scores.length);
			this.state.order = new Array(scores.length);
			for (i = 0; i < scores.length; i++) {
				this.state.ranks[i] = 1;
				for (k = 0; k < scores.length; k++) {
					if (scores[i] < scores[k]) {
						this.state.ranks[i]++;
					}
				}
				k = this.state.ranks[i] - 1;
				while (this.state.order[k] !== undefined)
					k++;
				this.state.order[k] = i;
			}
			// add player buttons
			if (this.state.replay.hasDuration) {
				this.addPlayerButtons();
			}
			// add static buttons
			if (this.state.options['interactive']) {
				if (!this.btnMgr.groups['playback']) {
					if (this.state.replay.hasDuration) {
						bg = this.btnMgr.addImageGroup('playback', this.imgMgr.images[2],
								ImageButtonGroup.HORIZONTAL, ButtonGroup.MODE_NORMAL, 2, 0);

						dlg = new Delegate(this, function() {
							this.director.gotoTick(0);
						});
						bg.addButton(3, dlg, 'jump to start of first turn');
						bg.addSpace(32);

						dlg = new Delegate(this, function() {
							var stop = Math.ceil(this.state.time * 2) - 1;
							this.director.slowmoTo(stop / 2);
						});
						bg.addButton(5, dlg, 'play one move/attack phase backwards');
						bg.addSpace(64);

						dlg = new Delegate(this.director, this.director.playStop);
						bg.addButton(4, dlg, 'play/stop the game');
						bg.addSpace(64);

						dlg = new Delegate(this, function() {
							var stop = Math.floor(this.state.time * 2) + 1;
							this.director.slowmoTo(stop / 2);
						});
						bg.addButton(6, dlg, 'play one move/attack phase');
						bg.addSpace(32);

						dlg = new Delegate(this, function() {
							this.director.gotoTick(this.director.duration);
						});
						bg.addButton(2, dlg, 'jump to end of the last turn');
					}

					bg = this.btnMgr.addImageGroup('toolbar', this.imgMgr.images[4],
							ImageButtonGroup.VERTICAL, ButtonGroup.MODE_NORMAL, 2, 0);

					if (this.state.config.hasLocalStorage()) {
						dlg = new Delegate(this, function() {
							this.state.config.save();
						});
						bg.addButton(0, dlg, 'save and reuse the current settings');
					}

					if (!this.state.options['embedded']) {
						dlg = new Delegate(this, function() {
							var fs = this.state.config['fullscreen'];
							this.setFullscreen(!fs);
						});
						bg.addButton(1, dlg, 'toggle fullscreen mode');
					}

					dlg = new Delegate(this, function() {
						this.setZoom(2 * this.state.config['zoom']);
						this.director.draw();
					});
					bg.addButton(2, dlg, 'zoom in');

					dlg = new Delegate(this, function() {
						var oldScale = this.state.scale;
						do {
							this.setZoom(0.5 * this.state.config['zoom']);
						} while (this.state.scale === oldScale && this.state.config['zoom'] > 1);
						this.director.draw();
					});
					bg.addButton(3, dlg, 'zoom out');

					dlg = new Delegate(this, this.centerMap);
					bg.addButton(4, dlg, 'center the map').enabled = false;

					dlg = new Delegate(this, function() {
						var lbl = this.state.config['label'];
						this.setAntLabels((lbl + 1) % 3);
						this.director.draw();
					});
					bg.addButton(5, dlg,
							'toggles: 1. player letters on ants, 2. global ids on ants');

					if (this.state.replay.hasDuration) {
						dlg = new Delegate(this, this.modifySpeed, [ +1 ]);
						bg.addButton(6, dlg);

						dlg = new Delegate(this, this.modifySpeed, [ -1 ]);
						bg.addButton(7, dlg);

						dlg = new Delegate(this, this.generateBotInput);
						bg.addButton(8, dlg, 'regenerates bot input from this replay');
					}
				}
				// generate fog images
				if (this.state.replay.hasDuration) {
					this.imgMgr.colorize(3, colors);
					bg = this.btnMgr.addImageGroup('fog', this.imgMgr.patterns[3],
							ImageButtonGroup.VERTICAL, ButtonGroup.MODE_RADIO, 2);
					var buttonAdder = function(fog) {
						var dlg = new Delegate(vis, vis.showFog);
						var hint = 'show/clear fog of war for ';
						hint += vis.state.replay.meta['playernames'][fog];
						return bg.addButton(fog, dlg, hint);
					};
					for (i = 0; i < colors.length; i++) {
						buttonAdder(this.state.order[i]);
					}
				}
			}
		}
		// calculate speed from duration and config settings
		this.director.duration = this.state.replay.duration;
		this.calculateReplaySpeed();
		if (this.state.options['interactive']) {
			if (this.state.options['decorated']) {
				this.director.onstate = function() {
					var btn = vis.btnMgr.groups['playback'].buttons[4];
					btn.offset = (vis.director.playing() ? 7 : 4) * vis.imgMgr.images[2].height;
					if (btn === vis.btnMgr.pinned) {
						vis.btnMgr.pinned = null;
					}
					btn.down = 0;
					btn.draw();
				};
			}
			// this will fire once in FireFox when a key is held down
			/**
			 * @ignore
			 * @param event
			 *        The input event.
			 * @returns {Boolean} True, if the browser should handle the event.
			 */
			document.onkeydown = function(event) {
				if (!(event.shiftKey || event.ctrlKey || event.altKey || event.metaKey || (document.activeElement || {}).tagName == "INPUT")) {
					if (Visualizer.focused.keyPressed(event.keyCode)) {
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
		/**
		 * @ignore
		 * @param event
		 *        The input event.
		 */
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
					+ ((window.scrollX === undefined) ? (document.body.parentNode.scrollLeft !== undefined) ? document.body.parentNode.scrollLeft
							: document.body.scrollLeft
							: window.scrollX);
			my = event.clientY
					- my
					+ ((window.scrollY === undefined) ? (document.body.parentNode.scrollTop !== undefined) ? document.body.parentNode.scrollTop
							: document.body.scrollTop
							: window.scrollY);
			vis.mouseMoved(mx, my);
		};
		/** @ignore */
		this.main.canvas.onmouseout = function() {
			vis.mouseExited();
		};
		/**
		 * @ignore
		 * @param event
		 *        The input event.
		 */
		this.main.canvas.onmousedown = function(event) {
			if (event.which === 1) {
				Visualizer.focused = vis;
				vis.mousePressed();
			}
		};
		/**
		 * @ignore
		 * @param event
		 *        The input event.
		 */
		this.main.canvas.onmouseup = function(event) {
			if (event.which === 1) {
				vis.mouseReleased();
			}
		};
		/**
		 * @ignore
		 * @param event
		 *        The input event.
		 */
		this.main.canvas.ondblclick = function(event) {
			if (vis.shiftedMap.contains(vis.mouseX, vis.mouseY)) vis.centerMap();
		};
		/** @ignore */
		window.onresize = function() {
			vis.resize();
		};
		Visualizer.focused = this;
		// move to a specific row and col
		this.calculateMapCenter(ZOOM_SCALE);
		this.state.shiftX = this.mapCenterX;
		this.state.shiftY = this.mapCenterY;

		this.log.style.display = 'none';
		this.main.canvas.style.display = 'inline';
		this.loading = LoadingState.IDLE;
		this.setFullscreen(this.state.config['fullscreen']);
		if (this.state.replay.hasDuration) {
			if (!isNaN(this.state.options['turn'])) {
				this.director.gotoTick(this.state.options['turn'] - 1);
			} else {
				this.director.play();
			}
		}
	} else if (this.replayStr) {
		this.loadParseReplay();
	}
};

Visualizer.prototype.centerMap = function() {
	this.state.shiftX = this.mapCenterX;
	this.state.shiftY = this.mapCenterY;
	if (this.state.options['decorated']) {
		var btn = this.btnMgr.groups['toolbar'].getButton(4);
		btn.enabled = false;
		btn.draw();
	}
	this.director.draw();
};

/**
 * Changes the replay speed.
 * 
 * @private
 * @param {Number}
 *        modifier {@link Config#speedFactor} is changed by this amount.
 */
Visualizer.prototype.modifySpeed = function(modifier) {
	this.state.config['speedFactor'] += modifier;
	this.calculateReplaySpeed();
};

/**
 * Calculates the position that the map should be moved to if it is centered. The map is centered
 * once at the start and on a click of the center button. The center is usually (0;0), unless
 * {@link Options#row} and {@link Options#col} are set.
 * 
 * @private
 * @param scale
 *        {Number} Since the position is in pixels, it depends on the map scale.
 */
Visualizer.prototype.calculateMapCenter = function(scale) {
	var options = this.state.options;
	var cols = this.state.replay.cols;
	var rows = this.state.replay.rows;
	if (!isNaN(options.row) && !isNaN(options.col)) {
		this.mapCenterX = scale * (0.5 * cols - 0.5 - options.col % cols);
		this.mapCenterY = scale * (0.5 * rows - 0.5 - options.row % rows);
	} else {
		this.mapCenterX = 0;
		this.mapCenterY = 0;
	}
};

/**
 * Adds the game and player link buttons at the top of the display.
 * 
 * @private
 */
Visualizer.prototype.addPlayerButtons = function() {
	var i;
	var bg = this.btnMgr.addTextGroup('players', ButtonGroup.MODE_NORMAL, 2);
	var vis = this;
	var dlg = undefined;
	var gameId = this.state.replay.meta['game_id'];
	if (gameId === undefined && this.state.options['game']) {
		gameId = this.state.options['game'];
	}
	if (gameId === undefined) {
		bg.addButton('Players:', '#000', undefined);
	} else {
		if (this.state.replay.meta['game_url']) {
			dlg = new Delegate(this, function() {
				window.location.href = this.state.replay.meta['game_url'].replace('~', gameId);
			});
		}
		bg.addButton('Game #' + gameId + ':', '#000', dlg);
	}
	var buttonAdder = function(idx) {
		var color = vis.state.replay.htmlPlayerColors[idx];
		var dlg = undefined;
		if (vis.state.replay.meta['user_url'] && vis.state.replay.meta['user_ids']
				&& vis.state.replay.meta['user_ids'][idx] !== undefined) {
			dlg = new Delegate(vis, function() {
				window.location.href = this.state.replay.meta['user_url'].replace('~',
						this.state.replay.meta['user_ids'][idx]);
			});
		}
		bg.addButton('', color, dlg);
	};
	for (i = 0; i < this.state.replay.players; i++) {
		buttonAdder(this.state.order[i]);
	}
	this.updatePlayerButtonText();
};

/**
 * This calculates the playback speed from the configuration values {@link Config#duration},
 * {@link Config#speedSlowest}, {@link Config#speedFastest} and {@link Config#speedFactor}. The
 * latter can be controlled by the speed buttons.
 * 
 * @private
 */
Visualizer.prototype.calculateReplaySpeed = function() {
	var speed = this.director.duration / this.state.config['duration'];
	speed = Math.max(speed, this.state.config['speedSlowest']);
	speed = Math.min(speed, this.state.config['speedFastest']);
	this.director.defaultSpeed = speed * Math.pow(1.5, this.state.config['speedFactor']);
	if (this.director.speed !== 0) {
		this.director.speed = this.director.defaultSpeed;
	}
	var hintText = function(base) {
		return 'set speed modifier to ' + ((base > 0) ? '+' + base : base);
	};
	if (this.state.options['interactive'] && this.state.options['decorated']
			&& this.state.replay.hasDuration) {
		var speedUpBtn = this.btnMgr.groups['toolbar'].getButton(6);
		speedUpBtn.hint = hintText(this.state.config['speedFactor'] + 1);
		var slowDownBtn = this.btnMgr.groups['toolbar'].getButton(7);
		slowDownBtn.hint = hintText(this.state.config['speedFactor'] - 1);
	}
};

/**
 * Calculates the visualizer display size depending on the constructor arguments and whether
 * fullscreen mode is supported and enabled.
 * 
 * @private
 * @returns {Size} the size the visualizer should have
 */
Visualizer.prototype.calculateCanvasSize = function() {
	var width, height;
	var embed = this.state.options['embedded'];
	embed = embed || !this.state.config['fullscreen'];
	width = (this.w && embed) ? this.w : window.innerWidth;
	height = (this.h && embed) ? this.h : window.innerHeight;
	return new Size(width, height);
};

/**
 * Enables or disables fullscreen mode. In fullscreen mode the &lt;body&gt; element is replaced with
 * a new one that contains only the visualizer. For the Java/Rhino version a special setFullscreen()
 * method on the window object is called.
 * 
 * @private
 * @param enable
 *        {Boolean} If true, the visualizer will switch to fullscreen mode if supported.
 */
Visualizer.prototype.setFullscreen = function(enable) {
	if (!this.state.options['embedded']) {
		if (window.setFullscreen) {
			this.state.config['fullscreen'] = window.setFullscreen(enable);
		} else {
			this.state.config['fullscreen'] = enable;
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

/**
 * Sets a new map zoom. At zoom level 1, the map is displayed such that
 * <ul>
 * <li>it has at least a border of 10 pixels on each side</li>
 * <li>map squares are displayed at an integer size</li>
 * <li>map squares are at least 1 pixel in size</li>
 * </ul>
 * This value is then multiplied by the zoom given to this function and ultimately clamped to a
 * range of [1..20].
 * 
 * @private
 * @param zoom
 *        {Number} The new zoom level in pixels. Map squares will be scaled to this value. It will
 *        be clamped to the range [1..20].
 */
Visualizer.prototype.setZoom = function(zoom) {
	var oldScale = this.state.scale;
	var effectiveZoom = Math.max(1, zoom);
	if (this.director.fixedFpt === undefined) {
		this.state.config['zoom'] = effectiveZoom;
		this.state.scale = Math.max(1, Math.min((this.shiftedMap.w - 20) / this.state.replay.cols,
				(this.shiftedMap.h - 20) / this.state.replay.rows)) | 0;
		this.state.scale = Math.min(ZOOM_SCALE, this.state.scale * effectiveZoom);
	} else {
		this.state.scale = Math.max(1, Math.min(this.shiftedMap.w / this.state.replay.cols,
				this.shiftedMap.h / this.state.replay.rows)) | 0;
		this.state.scale = Math.pow(2, (Math.log(this.state.scale) / Math.LN2) | 0);
	}
	if (oldScale) {
		this.state.shiftX = (this.state.shiftX * this.state.scale / oldScale) | 0;
		this.state.shiftY = (this.state.shiftY * this.state.scale / oldScale) | 0;
	}
	this.calculateMapCenter(this.state.scale);
	this.map.setSize(this.state.scale * this.state.replay.cols, this.state.scale
			* this.state.replay.rows);
	this.map.x = (((this.shiftedMap.w - this.map.w) >> 1) + this.shiftedMap.x) | 0;
	this.map.y = (((this.shiftedMap.h - this.map.h) >> 1) + this.shiftedMap.y) | 0;
	this.antsMap.setSize(this.map.w, this.map.h);
	this.fog.setSize(Math.min(this.map.w, this.shiftedMap.w), Math.min(this.map.h,
			this.shiftedMap.h));
	if (this.state.options['interactive'] && this.state.options['decorated']) {
		var zoomInBtn = this.btnMgr.groups['toolbar'].getButton(2);
		zoomInBtn.enabled = !(this.state.scale === ZOOM_SCALE);
		zoomInBtn.draw();
		var zoomOutBtn = this.btnMgr.groups['toolbar'].getButton(3);
		zoomOutBtn.enabled = !(effectiveZoom === 1);
		zoomOutBtn.draw();
	}
};

/**
 * Sets the ant label display mode to a new value.
 * 
 * @private
 * @param mode
 *        {Number} 0 = no display, 1 = letters, 2 = global ant ids
 */
Visualizer.prototype.setAntLabels = function(mode) {
	var hasDuration = this.state.replay.hasDuration;
	var recap = hasDuration && (mode === 1) !== (this.state.config['label'] === 1);
	this.state.config['label'] = mode;
	if (recap) {
		this.updatePlayerButtonText();
		this.main.ctx.fillStyle = '#fff';
		this.main.ctx.fillRect(0, 0, this.main.canvas.width, this.main.canvas.height);
		this.resize(true);
	}
};

/**
 * Updates the captions of player link buttons.
 * 
 * @private
 */
Visualizer.prototype.updatePlayerButtonText = function() {
	var i, idx, caption;
	var btnGrp = this.btnMgr.getGroup('players');
	for (i = 0; i < this.state.replay.players; i++) {
		idx = this.state.order[i];
		caption = this.state.replay.meta['playernames'][idx];
		if (this.state.config['label'] === 1) {
			caption = String.fromCharCode(0x3b1 + i) + ' ' + caption;
		} else {
			caption = this.state.ranks[idx] + '. ' + caption;
		}
		btnGrp.buttons[i + 1].setText(caption);
	}
};

/**
 * Called upon window size changes to layout the visualizer elements.
 * 
 * @private
 * @param forced
 *        {Boolean} If true, the layouting and redrawing is performed even if no size change can be
 *        detected. This is useful on startup or if the canvas content has been invalidated.
 */
Visualizer.prototype.resize = function(forced) {
	var y, w;
	var olds = new Size(this.main.canvas.width, this.main.canvas.height);
	var newSize = this.calculateCanvasSize();
	var resizing = newSize.w != olds.w || newSize.h != olds.h;
	if (resizing || forced) {
		var canvas = this.main.canvas;
		var ctx = this.main.ctx;
		if (resizing) {
			canvas.width = newSize.w;
			canvas.height = newSize.h;
			ctx.fillStyle = '#fff';
			ctx.fillRect(0, 0, canvas.width, canvas.height);
		}
		this.resizing = true;
		if (this.state.replay.hasDuration && this.state.options['decorated']) {
			// 1. player buttons
			y = this.btnMgr.groups['players'].cascade(newSize.w) + 4;
			// 2. scores bar & time line
			this.scores.x = 0;
			this.scores.y = y;
			this.scores.setSize(newSize.w, CanvasElementStats.MIN_HEIGHT);
			this.counts.x = 0;
			this.counts.y = this.scores.y + this.scores.h + 4;
			this.counts.setSize(newSize.w, CanvasElementStats.MAX_HEIGHT);
			y = this.counts.y + this.counts.h;
		} else {
			y = 0;
		}
		// 3. visualizer placement
		if (this.state.options['interactive'] && this.state.options['decorated']) {
			if (this.state.replay.hasDuration) {
				this.shiftedMap.x = LEFT_PANEL_W;
				this.shiftedMap.y = y;
				this.shiftedMap.setSize(newSize.w - LEFT_PANEL_W - RIGHT_PANEL_W, newSize.h - y
						- BOTTOM_PANEL_H);
				// playback buttons are center, unless they would exceed the right border of the map
				var bg = this.btnMgr.groups['playback'];
				w = 8 * 64;
				bg.x = ((newSize.w - w) / 2) | 0;
				bg.x = Math.min(bg.x, this.shiftedMap.x + this.shiftedMap.w - w);
				bg.x = Math.max(bg.x, 0);
				bg.y = this.shiftedMap.y + this.shiftedMap.h;
				bg = this.btnMgr.groups['fog'];
				bg.y = this.shiftedMap.y + 8;
			} else {
				this.shiftedMap.x = 0;
				this.shiftedMap.y = y;
				this.shiftedMap.setSize(newSize.w - RIGHT_PANEL_W, newSize.h - y);
			}
			bg = this.btnMgr.groups['toolbar'];
			bg.x = this.shiftedMap.x + this.shiftedMap.w;
			bg.y = this.shiftedMap.y + 8;
			// set button group extents
			if (this.state.replay.hasDuration) {
				bg = this.btnMgr.groups['fog'];
				bg.h = newSize.h - this.shiftedMap.y - 8;
				bg = this.btnMgr.groups['playback'];
				bg.w = this.shiftedMap.x + this.shiftedMap.w - bg.x;
			}
			bg = this.btnMgr.groups['toolbar'];
			bg.h = newSize.h - this.shiftedMap.y - 8;
		} else {
			this.shiftedMap.x = 0;
			this.shiftedMap.y = y;
			this.shiftedMap.setSize(newSize.w, newSize.h - y);
		}
		this.setZoom(this.state.config['zoom']);
		this.miniMap.x = this.shiftedMap.x + this.shiftedMap.w - 2 - this.state.replay.cols,
				this.miniMap.y = this.shiftedMap.y + 2;
		this.miniMap.setSize(this.state.replay.cols, this.state.replay.rows);
		// redraw everything
		if (this.state.options['decorated']) this.btnMgr.draw();
		this.director.draw(true);
		this.resizing = false;
	}
};

/**
 * Enables or disables fog of war display.
 * 
 * @private
 * @param idx
 *        {Number} The index of the player for which fog is to be displayed or undefined.
 */
Visualizer.prototype.showFog = function(idx) {
	this.state.fogPlayer = idx;
	this.director.draw();
};

/**
 * Redraws the map display and it's overlays. It is called by the {@link Director} and resembles the
 * core of the visualization.
 */
Visualizer.prototype.draw = function() {
	var ctx, w, h, mx, my, x, y;
	var loc = this.shiftedMap;

	// map
	this.shiftedMap.validate();
	this.main.ctx.drawImage(this.shiftedMap.canvas, loc.x, loc.y);

	// mouse cursor (super complicated position calculation)
	ctx = this.main.ctx;
	if (this.state.mouseOverVis) {
		ctx.save();
		ctx.beginPath();
		ctx.rect(loc.x, loc.y, loc.w, loc.h);
		ctx.clip();
		mx = this.mouseX - this.map.x - this.state.shiftX;
		my = this.mouseY - this.map.y - this.state.shiftY;
		mx = Math.floor(mx / this.state.scale) * this.state.scale + this.map.x + this.state.shiftX;
		my = Math.floor(my / this.state.scale) * this.state.scale + this.map.y + this.state.shiftY;
		mx -= loc.x - this.state.scale + 1;
		my -= loc.y - this.state.scale + 1;
		mx = Math.wrapAround(mx, this.map.w);
		my = Math.wrapAround(my, this.map.h);
		mx += loc.x - this.state.scale + 1;
		my += loc.y - this.state.scale + 1;
		ctx.strokeStyle = '#fff';
		ctx.beginPath();
		for (y = my; y < loc.y + loc.h; y += this.map.h) {
			for (x = mx; x < loc.x + loc.w; x += this.map.w) {
				ctx.rect(x + 0.5, y + 0.5, this.state.scale - 1, this.state.scale - 1);
			}
		}
		ctx.stroke();
		ctx.restore();
	}

	// minimap
	if (this.state.config['zoom'] !== 1 && this.miniMap.h + 4 <= loc.h
			&& this.miniMap.w + 4 <= loc.w) {
		this.miniMap.validate();
		ctx.save();
		// border
		ctx.strokeStyle = '#fff';
		ctx.lineWidth = 2;
		ctx.beginPath();
		ctx.rect(this.miniMap.x - 1, this.miniMap.y - 1, this.miniMap.w + 2, this.miniMap.h + 2);
		ctx.stroke();
		// map
		ctx.drawImage(this.miniMap.canvas, this.miniMap.x, this.miniMap.y);
		// position indicator
		ctx.beginPath();
		ctx.rect(this.miniMap.x, this.miniMap.y, this.miniMap.w, this.miniMap.h);
		ctx.clip();
		w = loc.w / this.state.scale;
		h = loc.h / this.state.scale;
		x = this.state.replay.cols / 2 - this.state.shiftX / this.state.scale - w / 2;
		y = this.state.replay.rows / 2 - this.state.shiftY / this.state.scale - h / 2;
		x -= Math.floor(x / this.state.replay.cols) * this.state.replay.cols;
		y -= Math.floor(y / this.state.replay.rows) * this.state.replay.rows;
		ctx.beginPath();
		ctx.rect(this.miniMap.x + x, this.miniMap.y + y, w, h);
		ctx.rect(this.miniMap.x + x - this.state.replay.cols, this.miniMap.y + y, w, h);
		ctx.rect(this.miniMap.x + x, this.miniMap.y + y - this.state.replay.rows, w, h);
		ctx.rect(this.miniMap.x + x - this.state.replay.cols, this.miniMap.y + y
				- this.state.replay.rows, w, h);
		ctx.stroke();
		ctx.restore();
	}

	if (this.state.replay.hasDuration && this.state.options['decorated']) {
		if (this.scores.validate() || this.resizing) {
			ctx.drawImage(this.scores.canvas, this.scores.x, this.scores.y);
		}
		if (this.counts.validate() || this.resizing) {
			ctx.drawImage(this.counts.canvas, this.counts.x, this.counts.y);
		}
	}

	// draw hint text
	var hint = this.hint;
	if (hint) {
		ctx.font = FONT;
		ctx.textAlign = 'left';
		ctx.textBaseline = 'middle';
		if (ctx.measureText(hint).width > loc.w) {
			do {
				hint = hint.substr(0, hint.length - 1);
			} while (hint && ctx.measureText(hint + '...').width > loc.w);
			if (hint) hint += '...';
		}
		w = ctx.measureText(hint).width;
		ctx.fillStyle = 'rgba(0,0,0,0.3)';
		ctx.fillRect(loc.x, loc.y, w, 22);
		ctx.fillStyle = '#fff';
		ctx.fillText(hint, loc.x, loc.y + 10);
	}
	if (this.state.isStreaming) {
		// we were able to draw a frame, the engine may send us the next turn
		var vis = this;
		window.setTimeout(function() {
			if (vis.state.isStreaming) vis.streamingStart();
		}, 0);
	} else if (this.director.fixedFpt !== undefined) {
		// store frame
		video.captureFrame(this.director.time, this.director.duration);
	}
};

/**
 * Internal wrapper around mouse move events.
 * 
 * @private
 * @param mx
 *        {Number} the X coordinate of the mouse relative to the upper-left corner of the
 *        visualizer.
 * @param my
 *        {Number} the Y coordinate of the mouse relative to the upper-left corner of the
 *        visualizer.
 */
Visualizer.prototype.mouseMoved = function(mx, my) {
	var tick;
	var deltaX = mx - this.mouseX;
	var deltaY = my - this.mouseY;
	var oldHint = this.hint;
	var btn = null;
	this.mouseX = mx;
	this.mouseY = my;
	this.state.mouseCol = (Math.wrapAround(mx - this.map.x - this.state.shiftX, this.state.scale
			* this.state.replay.cols) / this.state.scale) | 0;
	this.state.mouseRow = (Math.wrapAround(my - this.map.y - this.state.shiftY, this.state.scale
			* this.state.replay.rows) / this.state.scale) | 0;
	this.hint = '';
	if (this.state.options['interactive']) {
		if ((this.state.mouseOverVis = this.map.contains(this.mouseX, this.mouseY)
				&& this.shiftedMap.contains(this.mouseX, this.mouseY))) {
			this.hint = 'row ' + this.state.mouseRow + ' | col ' + this.state.mouseCol;
		}
		if (this.mouseDown === 1) {
			tick = this.mouseX - this.scores.graph.x;
			tick /= (this.scores.graph.w - 1);
			tick = Math.round(tick * this.state.replay.duration);
			this.director.gotoTick(tick);
		} else if (this.mouseDown === 2
				|| (this.mouseDown === 3 && this.miniMap.contains(this.mouseX, this.mouseY))) {
			if (this.mouseDown === 2) {
				this.state.shiftX += deltaX;
				this.state.shiftY += deltaY;
			} else {
				this.state.shiftX = (this.state.replay.cols / 2 - (this.mouseX - this.miniMap.x))
						* this.state.scale;
				this.state.shiftY = (this.state.replay.rows / 2 - (this.mouseY - this.miniMap.y))
						* this.state.scale;
			}
			if (this.state.options['decorated']) {
				var centerBtn = this.btnMgr.groups['toolbar'].getButton(4);
				centerBtn.enabled = this.state.shiftX !== 0 || this.state.shiftY !== 0;
				centerBtn.draw();
			}
			this.director.draw();
		} else if (this.state.options['decorated']) {
			btn = this.btnMgr.mouseMove(mx, my);
		}
	} else if (this.state.options['decorated']) {
		btn = this.btnMgr.mouseMove(mx, my);
	}
	if (btn && btn.hint) {
		this.hint = btn.hint;
	}
	if (oldHint !== this.hint) {
		this.director.draw();
	}
};

/**
 * Internal wrapper around mouse down events.
 * 
 * @private
 */
Visualizer.prototype.mousePressed = function() {
	if (this.state.options['interactive']) {
		if (this.state.replay.hasDuration
				&& this.state.options['decorated']
				&& (this.counts.graph.contains(this.mouseX, this.mouseY) || this.scores.graph
						.contains(this.mouseX, this.mouseY))) {
			this.mouseDown = 1;
		} else if (this.state.config['zoom'] !== 1
				&& this.miniMap.contains(this.mouseX, this.mouseY)) {
			this.mouseDown = 3;
		} else if (this.shiftedMap.contains(this.mouseX, this.mouseY)) {
			this.mouseDown = 2;
		} else {
			this.btnMgr.mouseDown();
			return;
		}
		this.mouseMoved(this.mouseX, this.mouseY);
	} else if (this.state.options['decorated']) {
		// handle game/player links if in non-interactive mode
		this.btnMgr.mouseDown();
	}
};

/**
 * Internal wrapper around mouse button release events.
 * 
 * @private
 */
Visualizer.prototype.mouseReleased = function() {
	this.mouseDown = 0;
	if (this.state.options['decorated']) {
		this.btnMgr.mouseUp();
	}
	this.mouseMoved(this.mouseX, this.mouseY);
};

/**
 * Internal wrapper around mouse exit window events.
 * 
 * @private
 */
Visualizer.prototype.mouseExited = function() {
	if (this.state.options['decorated']) {
		this.btnMgr.mouseMove(-1, -1);
		this.btnMgr.mouseUp();
	}
	this.mouseDown = 0;
};

/**
 * Internal wrapper around key press events.
 * 
 * @private
 * @param key
 *        A key code for the pressed button.
 * @returns {Boolean} false, if the browser should handle this key and true, if the visualizer
 *          handled the key
 */
Visualizer.prototype.keyPressed = function(key) {
	var d = this.director;
	var tryOthers = true;
	if (!this.state.options['embedded']) {
		tryOthers = false;
		switch (key) {
		case Key.PGUP:
			d.gotoTick(Math.ceil(this.state.time) - 10);
			break;
		case Key.PGDOWN:
			d.gotoTick(Math.floor(this.state.time) + 10);
			break;
		case Key.HOME:
			d.gotoTick(0);
			break;
		case Key.END:
			d.gotoTick(d.duration);
			break;
		default:
			tryOthers = true;
		}
	}
	if (tryOthers) {
		switch (key) {
		case Key.SPACE:
			d.playStop();
			break;
		case Key.LEFT:
			d.gotoTick(Math.ceil(this.state.time) - 1);
			break;
		case Key.RIGHT:
			d.gotoTick(Math.floor(this.state.time) + 1);
			break;
		case Key.PLUS:
		case Key.PLUS_OPERA:
		case Key.PLUS_JAVA:
			this.modifySpeed(+1);
			break;
		case Key.MINUS:
		case Key.MINUS_JAVA:
			this.modifySpeed(-1);
			break;
		default:
			switch (String.fromCharCode(key)) {
			case 'F':
				this.setFullscreen(!this.state.config['fullscreen']);
				break;
			case 'C':
				this.centerMap();
				break;
			case 'I':
				this.generateBotInput();
				break;
			default:
				return false;
			}
		}
	}
	return true;
};

/**
 * This method will ask the user for some input to convert the replay into a bot input string for
 * debugging.
 */
Visualizer.prototype.generateBotInput = function() {
	var player, user_id, user_index, min, max, current, turns, valid, i;
	var botInput, dataUri, gameId;
	try {
		if (!this.state.replay.hasDuration) return;
		// pause the replay
		this.director.stop();
		// first we need to know the player
		player = '';
		user_id = this.state.options['user'];
		if (user_id) {
			user_index = this.state.replay.meta['user_ids'].indexOf(user_id, 0);
			if (user_index !== -1) {
				player = this.state.replay.meta['playernames'][user_index];
			}
		}
		do {
			player = prompt('Enter the player name for which you want to generate bot input.',
					player);
			valid = true;
			if (player) {
				user_index = this.state.replay.meta['playernames'].indexOf(player, 0);
				valid = user_index !== -1;
				if (valid) {
					min = 1;
					max = this.state.replay.duration;
					current = 1 + (this.state.time | 0);
					do {
						turns = prompt(
								'Enter a range of turns or a single turn number to be included in the output.\nThe largest valid range for this replay is '
										+ min + '-' + max + '.', min + '-' + current);
						valid = true;
						if (turns) {
							turns = turns.split('-');
							valid = turns.length <= 2;
							for (i = 0; i < turns.length; i++) {
								valid = valid && /^[0-9]+$/.test(turns[i]);
								valid = valid && !isNaN(turns[i] = parseInt(turns[i]));
							}
							valid = valid && turns[0] >= min;
							valid = valid && turns[turns.length - 1] <= max;
							if (valid) {
								min = turns[0] - 1;
								max = turns[turns.length - 1] - 1;
								botInput = this.state.replay.generateBotInput(user_index, min, max);
								if (window.saveText) {
									// Java version
									gameId = this.state.replay.meta['game_id']
											|| this.state.options['game'] || 0;
									window.saveText(botInput, gameId + '.bot' + user_index
											+ '.input');
								} else {
									dataUri = 'data:text/plain;charset=utf-8,' + escape(botInput);
									window.open(dataUri, '_blank');
								}
							} else {
								alert('Invalid input.');
							}
						}
					} while (!valid);
				} else {
					alert('Invalid user name.');
				}
			}
		} while (!valid);
	} catch (e) {
		alert('Error while generating bot input: ' + e);
	}
};

/**
 * @class This class defines startup options that are enabling debugging features or set immutable
 *        configuration items for the visualizer instance. The available options are listed in the
 *        field summary and can be set by appending them as a parameter to the URL. For example
 *        '...?game=1&turn=20' will display game 1 and jump to turn 20 immediately. For boolean
 *        values 'true' or '1' are interpreted as true, everything else as false. Be aware that it
 *        is also possible to add a parameter named 'config' to the URL that will be handled
 *        specially by {@link Visualizer} to override {@link Config} settings. Also note that any
 *        additional options should have an initial value that makes it clear wether the setting is
 *        a number, a boolean or a string, because options are passed as strings to the Java applet
 *        and it will parse these strings to the data type it finds in the Options object.
 * @constructor
 * @property {String} data_dir The directory that contains the 'img' directory as a relative or
 *           absolute path. You will get an error message if you forget the tailing '/'.
 * @property {Boolean} interactive Set this to false to disable mouse and keyboard input and hide
 *           the buttons from view.
 * @property {Boolean} decorated Set this to false to hide buttons and statistics. This results in a
 *           'naked' visualizer suitable for small embedded sample maps.
 * @property {Boolean} debug Set this to true, to enable loading of some kinds of partially corrupt
 *           replays and display an FPS counter in the title bar.
 * @property {Boolean} profile Set this to true, to enable rendering code profiling though
 *           'console.profile()' in execution environments that support it.
 * @property {Boolean} embedded Set this to true, to disable the fullscreen option.
 * @property {String} game This is the game number that is used by the game link button for display
 *           and as to create the link URL.
 * @property {Number} col If row and col are both set, the visualizer will draw a marker around this
 *           location on the map and zoom in on it. The value is automatically wrapped around to
 *           match the map dimensions.
 * @property {Number} row See {@link Options#col}.
 * @property {Number} turn If this is set, the visualizer will jump to this turn when playback
 *           starts and stop there. This is often used with {@link Options#col} and
 *           {@link Options#row} to jump to a specific event.
 * @property {String} user If set, the replay will give this user id the first color in the list so
 *           it can easily be identified on the map.
 * @property {Boolean} loop If set, the replay will fade out and start again at the end.
 */
Options = function() {
	this['data_dir'] = '';
	this['interactive'] = true;
	this['decorated'] = true;
	this['debug'] = false;
	this['profile'] = false;
	this['embedded'] = false;
	this['game'] = '';
	this['col'] = NaN;
	this['row'] = NaN;
	this['turn'] = NaN;
	this['user'] = '';
	this['loop'] = false;
};

/**
 * Converts a string parameter in the URL to a boolean value.
 * 
 * @param value
 *        {String} the parameter
 * @returns {Boolean} true, if the parameter is either '1' or 'true'
 */
Options.toBool = function(value) {
	return value == '1' || value == 'true';
};

/**
 * @class Holds public variables that need to be accessed from multiple modules of the visualizer.
 * @constructor
 * @property {Number} scale The size of map squares in pixels.
 * @property {Array} ranks Stores the rank of each player.
 * @property {Array} order Stores the ranking order of each player.
 * @property {Replay} replay The currently loaded replay.
 * @property {Number} fogPlayer The array index of the player for which fog of war is enabled. It is
 *           undefined if fog of war is disabled.
 * @property {Number} time The current visualizer time or position in turns, starting with 0 at the
 *           start of 'turn 1'.
 * @property {Number} shiftX X coordinate displacement of the map.
 * @property {Number} shiftY Y coordinate displacement of the map.
 * @property {Boolean} mouseOverVis True, if the mouse is currently in the active area of the map.
 *           This is used to quickly check if mouse-over effects need to be drawn.
 * @property {Number} mouseCol The current wrapped map column, the mouse is hovering over. This
 *           value is only valid when {@link State#mouseOverVis} is true.
 * @property {Number} mouseRow The current wrapped map row, the mouse is hovering over. This value
 *           is only valid when {@link State#mouseOverVis} is true.
 * @property {Boolean} isStreaming This should be true as long as the visualizer is receiving data
 *           from a game in progress and be set to false when the last turn has been sent as
 *           indicated by the result of Stream.visualizerReady() in Stream.java.
 * @property fade Undefined, unless a fade out/in effect is to be drawn. Then this is set to a
 *           rgba() fill style.
 */
function State() {
	this.cleanUp();
	this.options = null;
	this.config = new Config();
}

/**
 * Resets the state to initial values.
 */
State.prototype.cleanUp = function() {
	this.scale = NaN;
	this.ranks = undefined;
	this.order = undefined;
	this.replay = null;
	this.fogPlayer = undefined;
	this.time = 0;
	this.shiftX = 0;
	this.shiftY = 0;
	this.mouseOverVis = false;
	this.mouseCol = undefined;
	this.mouseRow = undefined;
	this.isStreaming = false;
	this.fade = undefined;
};

/**
 * Helper function to ask the {@link Replay} for the fog of war of the chosen
 * {@link State#fogPlayer} at the current {@link State#time}.
 * 
 * @returns {Boolean[][]} See {@link Replay#getFog}.
 */
State.prototype.getFogMap = function() {
	return this.replay.getFog(this.fogPlayer, this.time | 0);
};
