var ants = require('./Ants').ants;

var bot = {
    'onReady': function() {
		ants.finishTurn();
    },
    'onTurn': function() {
		var myAnts = ants.myAnts();
		var directions = ['N', 'E', 'S', 'W'];
		for (var i in myAnts) {
			var ant = myAnts[i];
			for (dirI in directions) {
				var dir = directions[dirI];
				var loc = ants.destination(ant.row, ant.col, dir);
				if (ants.passable(loc[0], loc[1])) {
					ants.issueOrder(ant.row, ant.col, dir);
					break;
				}
			}
		}
		ants.finishTurn();
    },
    'onEnd': function() {
    
    }
}
ants.start(bot);