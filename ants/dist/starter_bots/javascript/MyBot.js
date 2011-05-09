var ants = require('./Ants');

var bot = {
    'onReady': function() {
		ants.finishTurn();
    },
    'onTurn': function() {
		for (var i in ants.ants) {
			var ant = ants.ants[i];
			if (ant.owner == 0) {
				if (ants.tileInDirection(ant.row, ant.col, 'N') != ants.landTypes.WATER) {
					ants.issueOrder(ant.row, ant.col, 'N');
				}
			}
		}
		ants.finishTurn();
    },
    'onEnd': function() {
    
    }
}
ants.start(bot);