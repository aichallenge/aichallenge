#import("Ants.dart");

class MyBot implements Bot {
  Ants ants;
  
  MyBot(Ants this.ants);
  
  void onReady() {
    ants.finishTurn();
  }
  
  void onTurn() {
    var myAnts = ants.myAnts();
		var directions = ['N', 'E', 'S', 'W'];
		for (int i = 0; i < myAnts.length; i++) {
			Ant ant = myAnts[i];
			for (int dirI = 0; dirI < directions.length; dirI++) {
				var dir = directions[dirI];
				var loc = ants.destination(ant.row, ant.col, dir);
				if (ants.passable(loc[0], loc[1])) {
					ants.issueOrder(ant.row, ant.col, dir);
					break;
				}
			}
		}
		ants.finishTurn();
  }
  
  void onEnd() {
  }
}

void main() {
  Ants ants = new Ants();
  ants.start(new MyBot(ants));
}