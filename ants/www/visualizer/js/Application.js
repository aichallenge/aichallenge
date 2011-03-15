/**
 * @fileoverview This is a visualizer for the ant game.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 * @todo focus button for each player
 * @todo fog of war option
 * @todo scrolling player names if too long; fade to the left and right
 * @todo zoom in to 20x20 squares with animated ants
 * @todo save settings
 * @todo animated single steps if possible (how?)
 * @todo graph showing ants and scores
 * @todo menu items: fullscreen, save, clear, show fog, show attack, show birth,
 *     zoom, toggle graph/score bars, cpu use
 * @todo duplicate sprites that wrap around the map edge
 * @todo improve resource deallocation in case of errors; reset objects
 * @todo keep a minimum size to allow the controls to render
 * @todo setting for cpu usage
 * @todo make loading multiple replays in sequence more reliable
 * @todo set player colors from replay file (awaiting answer)
 * @todo show when a bot crashed (awaiting answer)
 * @todo clickable player names (requires user_id)
 */

$import('Director');
$import('Config');
$import('Buttons');
$import('ImageManager');
$import('AppletManager');
$import('Replay');

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
	END: 35
};

/**
 * @class The main 'application' object that provides all necessary methods for
 *     the use in a web page.
 * @param {Node} container the html element, that the visualizer will embed into
 * @param {String} dataDir This relative path to the visualizer data files. You
 *     will get an error message if you forget the tailing '/'.
 * @param {String} codebase The java applet will be loaded from this location.
 * @param {Number} w an optional maximum width or undefined
 * @param {Number} h an optional maximum height or undefined
 * @param {Boolean} java if set to true or false, this overrides the
 *     auto-detection of the Java mode
 */
function Visualizer(container, dataDir, codebase, w, h, java) {
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
	 * manages playback commands and timing
	 * @private
	 */
	this.director = new Director(this);
	/**
	 * presistable configuration values
	 * @private
	 */
	this.config = new Config();
	/**
	 * Options from URL GET parameters or the constructor arguments
	 * @private
	 */
	this.options = {};
	this.options.java = java;
	this.options.data_dir = dataDir;
	this.options.codebase = codebase;
	// read URL parameters and store them in the parameters object
	var equalPos, value, key, i;
	var parameters = window.location.href.split('?');
	if (parameters.length > 1) {
		parameters = parameters[1].split('#')[0].split('&');
		for (i = 0; i < parameters.length; i++) {
			equalPos = parameters[i].indexOf('=');
			key = parameters[i].substr(0, equalPos);
			value = parameters[i].substr(equalPos + 1);
			if (key === 'java' || key === 'debug') value = new Boolean(value);
			this.options[key] = value;
		}
	}
	/**
	 * buttons
	 * @private
	 */
	this.btnMgr = new ButtonManager(this);
	/**
	 * @private
	 */
	this.log = document.createElement('div');
	// print some information text
	var text = 'Loading visualizer...';
	text += '<table>';
	for (key in this.options) {
		value = this.options[key];
		text += '<tr><td>-&nbsp;</td><td>' + key + '&nbsp;&nbsp;</td><td><b>' + value + '&nbsp;&nbsp;</b></td><td><i>';
		if (key == "java") {
			text += '(Display method: ';
			if (value === undefined) {
				this.options.java = !document.createElement('canvas').getContext;
				text += (this.options.java ? 'Java Applet [autodetected]' : 'HTML Canvas [autodetected]') + ')';
			} else {
				text += (value ? 'Java Applet' : 'HTML Canvas') + ')';
			}
		} else if (key == "data_dir") {
			text += '(Image directory)';
		} else if (key == "codebase") {
			text += '(Java codebase)';
		}
		text += '</i></td></tr>';
	}
	text += '</table>';
	while(this.container.hasChildNodes()){
		this.container.removeChild(this.container.lastChild);
	}
	this.log.innerHTML = text;
	this.container.appendChild(this.log);
	/**
	 * images used by the visualizer
	 * @private
	 */
	this.imgMgr = new ImageManager((dataDir || '') + 'img/', this, 'completedImages');
	this.imgMgr.add('wood.jpg');
	this.imgMgr.add('playback.png');
	this.imgMgr.add('fog.png');
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
	 * @private
	 */
	this.fog = undefined;
	this.loading = LoadingState.IDLE;
	// If we use a Java applet, we have to delay the image requests until we
	// can pass the image URLs over to it.
	if (!this.options.java) this.imgMgr.startRequests();
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
				msg += '<p><u><b>Error ' + key + ':</b></u>\n' + error[key] + '</p>';
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
	if (this.main.element && !this.options.java) {
		if (this.container.firstChild === this.main.element) {
			this.container.removeChild(this.main.element);
		}
	}
	this.fog = undefined;
	document.onkeydown = null;
	document.onkeyup = null;
	document.onkeypress = null;
	window.onresize = null;
	this.log.style.display = 'block';
};
Visualizer.prototype.preload = function() {
	if (this.loading !== LoadingState.IDLE) return true;
	while (this.log.firstElement !== this.log.lastElement) {
		this.log.removeElement(this.log.lastElement);
	}
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
		var vis = this;
		this.replay = new XMLHttpRequest();
		this.replay.onreadystatechange = function() {
			if (vis.replay.readyState === 4) {
				if (vis.loading === LoadingState.LOADING) {
					if (vis.replay.status === 200) {
						vis.replay = vis.replay.responseText;
						vis.loadParseReplay();
					} else {
						vis.errorOut('Status ' + p.status + ': ' + p.statusText);
					}
				}
			}
		};
		this.replay.open("GET", file);
		this.replay.send(null);
		this.loadCanvas(true);
	});
};
/**
 * Loads a replay file through the php site. This data has html special
 * characters encoded and has a general structure of key=value.
 * @param {string} data the encoded replay data
 */
Visualizer.prototype.loadReplayDataFromPHP = function(data) {
	if (this.preload()) return;
	var vis = this;
	this.progress('Reading replay passed in from php...', function() {
		var parameters = {};
		var unquot = data.replace(/&lt;/g, '<').replace(/&gt;/g, '>')
		unquot = unquot.replace(/&quot;/g, '"').replace(/&amp;/g, '&');
		var matches = unquot.match(/[a-zA-Z_]+=.*((?!\n[a-zA-Z_]+=)\n.*)*(?=\n)/gm);
		var replay = undefined;
		if (matches) {
			for (var i = 0; i < matches.length; i++) {
				var ep = matches[i].indexOf('=');
				var key = matches[i].substr(0, ep);
				var value = matches[i].substr(ep + 1);
				if (key == 'playback_string') {
					replay = value;
				} else {
					parameters[key] = value;
				}
			}
			if (!replay) {
				vis.errorOut('game_info.php did not feed any playback_string.');
			} else {
				vis.replay = {replay: replay, parameters: parameters};
				vis.loadCanvas(true);
			}
		} else if (data) {
			vis.errorOut(data);
		} else {
			vis.errorOut('no data');
		}
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
/**
 * @private
 */
Visualizer.prototype.loadParseReplay = function() {
	if (this.replay && this.replay instanceof Replay) return;
	var vis = this;
	this.progress('Parsing the replay...', function() {
		if (!vis.replay) {
			throw new Error('Replay is undefined.');
		} else if (typeof vis.replay == 'string') { // string only
			vis.replay = new Replay(vis.replay);
		} else if (vis.replay instanceof XMLHttpRequest) {
			throw new Error('Attempted to manualy trigger the replay parsing process while waiting for the download.');
		} else if (vis.replay instanceof Object) { // string + param
			vis.replay = new Replay(vis.replay.replay, vis.replay.parameters);
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
		var size = vis.calculateCanvasSize();
		if (!vis.main.element) {
			if (vis.options.java) {
				var token = appletManager.add(vis);
				var e = document.createElement('applet');
				vis.main.element = e;
				e.setAttribute('codebase', vis.options.codebase);
				e.setAttribute('code', 'com.aicontest.visualizer.CanvasApplet');
				e.setAttribute('width', size.width);
				e.setAttribute('height', size.height);
				e.setAttribute('mayscript', 'true');
				var param = function(name, value) {
					var p = document.createElement('param');
					p.setAttribute('name', name);
					p.setAttribute('value', value);
					e.appendChild(p);
					e.setAttribute(name, value);
				}
				if (vis.options.debug) {
					param('separate_jvm', 'true');
					param('classloader_cache', 'false');
					param('debug', true);
				}
				param('token', token);
				vis.container.insertBefore(e, vis.log);
				// wait for the applet to call back
				return;
			} else {
				vis.main.element = document.createElement('canvas');
				vis.main.canvas = vis.main.element;
			}
		}
		// from here on I can handle both canvases the same
		vis.main.ctx = vis.main.canvas.getContext('2d');
		e = vis.main.element;
		if (vis.container.firstChild !== e) {
			vis.container.insertBefore(e, vis.log);
		}
		vis.createCanvas(vis.map);
		vis.createCanvas(vis.border);
		if (!vis.btnMgr.groups.playback) {
			with (vis.btnMgr.addGroup('playback', vis.imgMgr.images[1], ButtonGroup.HORIZONTAL, ButtonGroup.MODE_NORMAL, 2)) {
				addButton(3, function() {vis.director.gotoTick(0)});
				addSpace(32);
				addButton(5, function() {vis.director.gotoTick(Math.ceil(vis.director.position) - 1)});
				//drawImage(this.imgMgr.images[1], 0 * 64, 0, 64, 64, x + 2.5 * 64, y, 64, 64);
				addSpace(64);
				addButton(4, function() {vis.director.playStop()});
				//drawImage(this.imgMgr.images[1], 1 * 64, 0, 64, 64, x + 4.5 * 64, y, 64, 64);
				addSpace(64);
				addButton(6, function() {vis.director.gotoTick(Math.floor(vis.director.position) + 1)});
				addSpace(32);
				addButton(2, function() {vis.director.gotoTick(vis.director.duration)});
			}
		}
		vis.tryStart();
	});
};
/**
 * Called by the AppletManager when the applet is initialized
 */
Visualizer.prototype.initializedApplet = function() {
	this.main.canvas = this.main.element.getMainCanvas();
	this.imgMgr.javaApplet = this.main.element;
	this.imgMgr.startRequests();
	this.loadCanvas(false);
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
	// we need to parse the replay, unless it has been parsed by the
	// XmlHttpRequest callback
	if (this.replay && this.replay instanceof Replay) {
		if (this.main.ctx && !this.imgMgr.error && !this.imgMgr.pending) {
			var vis = this;
			// generate fog images
			var colors = [[255, 255, 255]].concat(Const.PLAYER_COLORS.slice(0, this.replay.players.length));
			this.imgMgr.colorize(2, colors);
			var bg = this.btnMgr.addGroup('fog', this.imgMgr.patterns[2], ButtonGroup.VERTICAL, ButtonGroup.MODE_RADIO, 2);
			bg.y = Const.TOP_PANEL_H;
			for (var i = 0; i < colors.length; i++) {
				var buttonAdder = function(fog) {
					return bg.addButton(i, function() { vis.showFog(fog); });
				}
				if (i == 0) {
					buttonAdder(undefined).down = true;
				} else {
					buttonAdder(i - 1);
				}
			}
			// try to make the replays play 1 minute, but the turns take no more than a second
			this.director.duration = this.replay.turns.length - 1;
			this.director.defaultSpeed = Math.max(this.director.duration / 60, 1);
			this.director.onstate = function() {
				var btn = vis.btnMgr.groups.playback.buttons[4];
				btn.offset = (vis.director.playing() ? 7 : 4) * vis.imgMgr.images[1].height;
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
			if (this.options.java) {
				this.main.element.setInputHandler(this);
			} else {
				// setup mouse handlers
				this.main.element.onmousemove = function(event) {
					var mx = 0;
					var my = 0;
					var obj = this;
					if (this.offsetParent) do {
						mx += obj.offsetLeft;
						my += obj.offsetTop;
					} while ((obj = obj.offsetParent));
					with (event || window.event) {
						mx = clientX - mx + ((window.scrollX === undefined) ? (document.body.parentNode.scrollLeft !== undefined) ? document.body.parentNode.scrollLeft : document.body.scrollLeft : window.scrollX);
						my = clientY - my + ((window.scrollY === undefined) ? (document.body.parentNode.scrollTop !== undefined) ? document.body.parentNode.scrollTop : document.body.scrollTop : window.scrollY);
					}
					vis.mouseMoved(mx, my);
				};
				this.main.element.onmouseout = function() {
					vis.mouseExited();
				};
				this.main.element.onmousedown = function() {
					vis.mousePressed();
				};
				this.main.element.onmouseup = function() {
					vis.mouseReleased();
				};
			}
			window.onresize = function() {
				vis.resize();
			};
			Visualizer.prototype.focused = this;
			this.setFullscreen(this.config.fullscreen);
			this.director.play();
			this.log.style.display = 'none';
			this.loading = LoadingState.IDLE;
		}
	} else {
		this.loadParseReplay();
	}
};
Visualizer.prototype.calculateCanvasSize = function() {
	var result = {};
	if (typeof(window.innerWidth) == 'number' ) {
		//Non-IE
		result.width = window.innerWidth;
		result.height = window.innerHeight;
	} else if (document.documentElement && (document.documentElement.clientWidth || document.documentElement.clientHeight)) {
		//IE 6+ in 'standards compliant mode'
		result.width = document.documentElement.clientWidth;
		result.height = document.documentElement.clientHeight;
	}
	result.width = (this.w && !this.config.fullscreen ? this.w : result.width);
	result.height = (this.h && !this.config.fullscreen ? this.h : result.height - 4);
	return result;
};
Visualizer.prototype.createCanvas = function(obj) {
	if (!obj.canvas) {
		if (this.options.java) {
			obj.canvas = this.main.element.createCanvas();
		} else {
			obj.canvas = document.createElement('canvas');
		}
	}
	if (!obj.ctx) {
		obj.ctx = obj.canvas.getContext('2d');
	}
};
Visualizer.prototype.setFullscreen = function(enable) {
	if (!this.options.java) {
		var html = document.getElementsByTagName("html")[0];
		this.config.fullscreen = enable;
		if (enable) {
			this.container.removeChild(this.main.element);
			var tempBody = document.createElement("body");
			tempBody.appendChild(this.main.element);
			this.savedBody = html.replaceChild(tempBody, document.body);
		} else if (this.savedBody) {
			document.body.removeChild(this.main.element);
			this.container.appendChild(this.main.element);
			html.replaceChild(this.savedBody, document.body);
			delete this.savedBody;
		}
	}
	this.resize(true);
};
Visualizer.prototype.resize = function(forced) {
	var olds = {
		width: this.main.element.getAttribute('width'),
		height: this.main.element.getAttribute('height')
	};
	var news = this.calculateCanvasSize();
	var resizing = news.width != olds.width || news.height != olds.height;
	if (resizing || forced) {
		if (resizing) {
			this.main.element.setAttribute('width', news.width);
			this.main.element.setAttribute('height', news.height);
			if (this.options.java) {
				this.main.element.setSize(news.width, news.height);
			}
		}
		this.loc.vis = {
			w: news.width - Const.LEFT_PANEL_W,
			h: news.height - Const.TOP_PANEL_H - Const.BOTTOM_PANEL_H,
			x: Const.LEFT_PANEL_W,
			y: Const.TOP_PANEL_H
		};
		this.scale = Math.min(10, Math.max(1, Math.min(
			(this.loc.vis.w - 2 * Const.ZOOM_SCALE) / (this.replay.cols),
			(this.loc.vis.h - 2 * Const.ZOOM_SCALE) / (this.replay.rows)
		))) | 0;
		this.loc.map = {
			w: this.scale * (this.replay.cols),
			h: this.scale * (this.replay.rows)
		};
		this.loc.map.x = ((this.loc.vis.w - this.loc.map.w) / 2 + this.loc.vis.x) | 0;
		this.loc.map.y = ((this.loc.vis.h - this.loc.map.h) / 2 + this.loc.vis.y) | 0;
		this.border.canvas.width = this.loc.map.w + 2 * Const.ZOOM_SCALE;
		this.border.canvas.height = this.loc.map.h + 2 * Const.ZOOM_SCALE;
		this.renderBorder();
		this.map.canvas.width = this.loc.map.w;
		this.map.canvas.height = this.loc.map.h;
		this.renderMap();
		with (this.btnMgr.groups) {
			playback.x = ((news.width - 8 * 64) / 2) | 0;
			playback.y = this.loc.vis.y + this.loc.vis.h;
			fog.y = (this.loc.vis.y + (this.loc.vis.h - fog.h) / 2) | 0
		}
		// redraw everything
		this.btnMgr.draw();
		// draw player names and captions
		var colors = Const.PLAYER_COLORS;
		this.main.ctx.textAlign = 'left';
		this.main.ctx.textBaseline = 'top';
		this.main.ctx.font = 'bold 20px Arial';
		var x = 0;
		for (var i = 0; i < this.replay.players.length; i++) {
			this.main.ctx.fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
			this.main.ctx.fillText(this.replay.players[i].name, x, 2);
			x += this.main.ctx.measureText(this.replay.players[i].name).width;
			if (i != this.replay.players.length - 1) {
				this.main.ctx.fillStyle = '#888';
				this.main.ctx.fillText(' vs ', x, 2);
				x += this.main.ctx.measureText(' vs ').width;
			}
		}
		var w = this.main.canvas.width;
		this.main.ctx.fillStyle = '#000';
		this.main.ctx.textAlign = 'center';
		this.main.ctx.textBaseline = 'middle';
		this.main.ctx.font = 'bold 12px Arial';
		this.main.ctx.fillText('ants'  , 30          , Const.TOP_PANEL_H - 10);
		this.main.ctx.fillText('scores', 30 + 0.5 * w, Const.TOP_PANEL_H - 10);
		this.director.draw();
	}
};
/**
 * @private
 */
Visualizer.prototype.renderMap = function() {
	var ctx = this.map.ctx;
	ctx.fillStyle = Const.COLOR_SAND;
	ctx.fillRect(0, 0, this.loc.map.w, this.loc.map.h);
	ctx.fillStyle = Const.COLOR_WATER;
	for (var row = 0; row < this.replay.rows; row++) {
		var start = undefined;
		for (var col = 0; col < this.replay.cols; col++) {
			var isWall = this.replay.walls[row][col];
			if (start === undefined && isWall) {
				start = col;
			} else if (start !== undefined && !isWall) {
				ctx.fillRect(this.scale * start, this.scale * row, this.scale * (col - start), this.scale);
				start = undefined;
			}
		}
		if (start !== undefined) {
			ctx.fillRect(this.scale * start, this.scale * row, this.scale * (col - start), this.scale);
		}
	}
};
/**
 * @private
 */
Visualizer.prototype.renderBorder = function() {
	var ctx = this.border.ctx;
	with(Const) {
		ctx.save();
			this.imgMgr.pattern(0, ctx, 'repeat');
			ctx.translate(ZOOM_SCALE, ZOOM_SCALE);
			with (this.loc.map) {
				ctx.save();
					ctx.beginPath();
					ctx.moveTo(0, 0);
					ctx.lineTo(w, 0);
					ctx.lineTo(w + ZOOM_SCALE, -ZOOM_SCALE);
					ctx.lineTo(-ZOOM_SCALE, -ZOOM_SCALE);
					ctx.closePath();
					ctx.fill();
				ctx.restore();
				ctx.save();
					ctx.translate(0, h);
					ctx.beginPath();
					ctx.moveTo(0, 0);
					ctx.lineTo(w, 0);
					ctx.lineTo(w + ZOOM_SCALE, +ZOOM_SCALE);
					ctx.lineTo(-ZOOM_SCALE, +ZOOM_SCALE);
					ctx.closePath();
					ctx.fill();
				ctx.restore();
				ctx.rotate(0.5 * Math.PI);
				ctx.save();
					ctx.beginPath();
					ctx.moveTo(0, 0);
					ctx.lineTo(h, 0);
					ctx.lineTo(h + ZOOM_SCALE, ZOOM_SCALE);
					ctx.lineTo(-ZOOM_SCALE, ZOOM_SCALE);
					ctx.closePath();
					ctx.fill();
				ctx.restore();
				ctx.save();
					ctx.translate(0, -w);
					ctx.beginPath();
					ctx.moveTo(0, 0);
					ctx.lineTo(h, 0);
					ctx.lineTo(h + ZOOM_SCALE, -ZOOM_SCALE);
					ctx.lineTo(-ZOOM_SCALE, -ZOOM_SCALE);
					ctx.closePath();
					ctx.fill();
			   ctx.restore();
			}
		ctx.restore();
	}
};
Visualizer.prototype.renderFog = function(turn) {
	this.border.ctx.clearRect(Const.ZOOM_SCALE, Const.ZOOM_SCALE, this.scale * this.replay.cols, this.scale * this.replay.rows);
	if (this.fog) {
		if (!this.fog.ctx) {
			this.createCanvas(this.fog);
			this.fog.canvas.width = 2;
			this.fog.canvas.height = 2;
			var color = Const.PLAYER_COLORS[this.fog.player];
			this.fog.ctx.fillStyle = 'rgb(' + color[0] + ',' + color[1] + ',' + color[2] + ')';
			this.fog.ctx.fillRect(0, 0, 1, 1);
			this.fog.ctx.fillRect(1, 1, 1, 1);
			this.fog.ptrn = this.border.ctx.createPattern(this.fog.canvas, 'repeat');
		}
		this.border.ctx.fillStyle = this.fog.ptrn;
		for (var row = 0; row < this.replay.rows; row++) {
			var y = Const.ZOOM_SCALE + row * this.scale;
			var start = undefined;
			for (var col = 0; col < this.replay.cols; col++) {
				var x = Const.ZOOM_SCALE + col * this.scale;
				var isFog = this.replay.turns[turn].fogs[this.fog.player][row][col];
				if (start === undefined && isFog) {
					start = x;
				} else if (start !== undefined && !isFog) {
					this.border.ctx.fillRect(start, y, x - start, this.scale);
					start = undefined;
				}
			}
			if (start !== undefined) {
				this.border.ctx.fillRect(start, y, Const.ZOOM_SCALE + this.replay.cols * this.scale - start, this.scale);
			}
		}
	}
};
Visualizer.prototype.showFog = function(fog) {
	if (fog === undefined) {
		this.fog = undefined;
		this.renderFog(this.director.position | 0);
	} else {
		this.fog = { player: fog };
	}
	this.director.draw();
};
/**
 * @private
 */
Visualizer.prototype.drawColorBar = function(x, y, w, h, values, colors) {
	var sum = 0;
	this.main.ctx.save();
	this.main.ctx.beginPath();
	this.main.ctx.rect(x, y, w, h);
	this.main.ctx.clip();
	for (var i = 0; i < values.length; i++) {
		sum += values[i];
	}
	var useValues = values;
	if (sum == 0) {
		useValues = new Array(values.length);
		for (i = 0; i < values.length; i++) {
			useValues[i] = 1;
		}
		sum = values.length;
	}
	var scale = w / sum;
	var offsetX = x;
	for (i = 0; i < useValues.length; i++) {
		var amount = scale * useValues[i];
		this.main.ctx.fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
		this.main.ctx.fillRect(offsetX, y, w - offsetX + x, h);
		offsetX += amount;
	}
	this.main.ctx.textAlign = 'left';
	this.main.ctx.textBaseline = 'middle';
	this.main.ctx.font = 'bold 16px Monospace';
	this.main.ctx.fillStyle = 'rgba(0,0,0,0.5)';
	var offsetY = y + 0.5 * h;
	offsetX = x + 2;
	for (i = 0; i < useValues.length; i++) {
		if (useValues[i] != 0) {
			this.main.ctx.fillText(Math.round(values[i]), offsetX, offsetY);
		}
		offsetX += scale * useValues[i];
	}
	this.main.ctx.restore();
};
/**
 * @private
 */
Visualizer.prototype.correctCoords = function(obj, w, h) {
	obj.x -= Math.floor(obj.x / w) * w;
	obj.y -= Math.floor(obj.y / h) * h;
};
/**
 * @private
 */
Visualizer.prototype.interpolate = function(turn, time, array) {
	if (time == (time | 0)) {
		return this.replay.turns[time][array];
	}
	var delta = time - turn;
	var result = new Array(this.replay.turns[0][array].length);
	for (var i = 0; i < result.length; i++) {
		result[i] = (1.0 - delta) * this.replay.turns[turn][array][i] + delta * this.replay.turns[turn + 1][array][i];
	}
	return result;
};
/**
 * @private
 */
Visualizer.prototype.draw = function(time, tick) {
	var cx, cy, mx, my;
	var drawOrder = [];
	var turn = (time | 0);
	// draw the map background
	this.main.ctx.fillStyle = '#ff3';
	this.main.ctx.drawImage(this.map.canvas, this.loc.map.x, this.loc.map.y);
	var w = this.main.canvas.width;
	if (tick !== undefined) {
		//document.getElementById('lblTurn').innerHTML = ((turn > this.replay.turns.length - 2) ? 'end result' : turn + ' / ' + (this.replay.turns.length - 2));
		if (this.fog !== undefined) this.renderFog(turn);
	}
	var counts = this.interpolate(turn, time, 'counts');
	this.drawColorBar(60,           Const.TOP_PANEL_H - 20, 0.5 * w - 60, 20, counts, Const.PLAYER_COLORS);
	var scores = this.interpolate(turn, time, 'scores');
	this.drawColorBar(60 + 0.5 * w, Const.TOP_PANEL_H - 20, 0.5 * w - 60, 20, scores, Const.PLAYER_COLORS);
	for (var i = 0; i < this.replay.turns[turn].ants.length; i++) {
		var antObj = this.replay.turns[turn].ants[i].interpolate(time, Quality.LOW);
		this.correctCoords(antObj, this.replay.cols, this.replay.rows);
		drawOrder.push(antObj);
	}
	for (var n = 0; n < drawOrder.length; n++) {
		antObj = drawOrder[n];
		if (this.config.zoom) {
			this.main.ctx.save();
			this.main.ctx.globalAlpha = antObj.alpha;
			cx = Const.ZOOM_SCALE * (antObj.x + 0.5);
			cy = Const.ZOOM_SCALE * (antObj.y + 0.5);
			this.main.ctx.translate(cx, cy);
			this.main.ctx.rotate(antObj.angle + Math.sin(20 * time) * antObj.jitter);
			this.main.ctx.drawImage(this.imgMgr.ants[antObj.type], -10, -10);
			this.main.ctx.restore();
			mx = cx + 3 * Math.tan(2 * (Math.random() - 0.5));
			my = cy + 3 * Math.tan(2 * (Math.random() - 0.5));
			if (antObj.alpha == 1) {
				var sin = -Math.sin(antObj.angle);
				var cos = +Math.cos(antObj.angle);
				this.ctxMap.moveTo(mx - sin, my - cos);
				this.ctxMap.lineTo(mx + sin, my + cos);
			}
		} else {
			mx = Math.round(this.scale * antObj.x) + this.loc.map.x;
			my = Math.round(this.scale * antObj.y) + this.loc.map.y;
			this.main.ctx.fillStyle = 'rgba(' + antObj.r + ', ' + antObj.g + ', ' + antObj.b + ', ' + antObj.a + ')';
			this.main.ctx.fillRect(mx, my, this.scale, this.scale);
		}
	}
	// draw border over ants, that moved out of the map
	this.main.ctx.drawImage(this.border.canvas, this.loc.map.x - Const.ZOOM_SCALE, this.loc.map.y - Const.ZOOM_SCALE);
	if (this.options.java) {
		this.main.element.repaint();
	}
};
Visualizer.prototype.mouseMoved = function(mx, my) {
	this.btnMgr.mouseMove(mx, my);
};
Visualizer.prototype.mousePressed = function() {
	this.btnMgr.mouseDown();
};
Visualizer.prototype.mouseReleased = function() {
	this.btnMgr.mouseUp();
};
Visualizer.prototype.mouseExited = function() {
	this.btnMgr.mouseMove(-1, -1);
};
Visualizer.prototype.mouseEntered = function(mx, my, down) {
	if (!down) this.btnMgr.mouseUp();
	this.btnMgr.mouseMove(mx, my);
};
Visualizer.prototype.keyPressed = function(key) {
	var d = this.director;
	switch(key) {
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
		default:
			switch (String.fromCharCode(key)) {
				case 'F':
					this.setFullscreen(!this.config.fullscreen);
					break;
			}
	}
};
Visualizer.prototype.keyReleased = function() {
};
