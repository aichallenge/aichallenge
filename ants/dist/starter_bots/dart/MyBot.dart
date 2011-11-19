#import("Ants.dart");

class MyBot implements Bot {
  Ants ants;
  
  MyBot(Ants this.ants);
  
  void onReady() {
    ants.finishTurn();
  }
  
  void onTurn() {
    List<Ants> myAnts = ants.myAnts();
		var directions = ['N', 'E', 'S', 'W'];
		for (Ant ant in myAnts) {
			Point antLoc = ant.pos;
			for (String dir in directions) {
				Point newLoc = ants.destination(antLoc, dir);
				if (ants.passable(newLoc)) {
					ants.issueOrder(antLoc, dir);
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