import java.util.*;
import java.util.Map.Entry;

public class Ants {
	private int turn = 0;
	private int turns = 0;
	private int rows = 0;
	private int cols = 0;
	private int loadtime = 0;
	private int turntime = 0;
	private int viewradius2 = 0;
	private int attackradius2 = 0;
	private int spawnradius2 = 0;
	private Ilk map[][];
	private Map<Tile, Ilk> antList = new HashMap<Tile, Ilk>();
	private Set<Tile> foodList = new HashSet<Tile>();
	private Set<Tile> deadList = new HashSet<Tile>();
	
	public int turn() {
		return this.turn;
	}

	public int turns() {
		return this.turns;
	}

	public int rows() {
		return this.rows;
	}

	public int cols() {
		return this.cols;
	}

	public int loadtime() {
		return this.loadtime;
	}

	public int turntime() {
		return this.turntime;
	}

	public int viewradius2() {
		return this.viewradius2;
	}

	public int attackradius2() {
		return this.attackradius2;
	}

	public int spawnradius2() {
		return this.spawnradius2;
	}

	public boolean setup(List<String> data) {
		try {
			for (String line : data) {
			    String tokens[] = line.toLowerCase().split(" ");
			    if (tokens[0].equals("cols")) {
			    	this.cols = Integer.parseInt(tokens[1]);		    	
			    } else if (tokens[0].equals("rows")) {
			    	this.rows = Integer.parseInt(tokens[1]);
			    } else if (tokens[0].equals("turns")) {
			    	this.turns = Integer.parseInt(tokens[1]);
			    } else if (tokens[0].equals("loadtime")) {
			    	this.loadtime = Integer.parseInt(tokens[1]);
			    } else if (tokens[0].equals("turntime")) {
			    	this.turntime = Integer.parseInt(tokens[1]);
			    } else if (tokens[0].equals("viewradius2")) {
			    	this.viewradius2 = Integer.parseInt(tokens[1]);
			    } else if (tokens[0].equals("attackradius2")) {
			    	this.attackradius2 = Integer.parseInt(tokens[1]);
			    } else if (tokens[0].equals("spawnradius2")) {
			    	this.spawnradius2 = Integer.parseInt(tokens[1]);
			    }
			}
			this.map = new Ilk[this.rows][this.cols];
			for (Ilk[] row : this.map) {
				Arrays.fill(row, Ilk.LAND);
			}
			return true;
		} catch (Exception ex) {
			return false;
		}
	}
	
	private boolean update(List<String> data) {
		// clear ants and food
		for (Tile ant : this.antList.keySet()) {
			this.map[ant.row()][ant.col()] = Ilk.LAND;
		}
		this.antList.clear();
		for (Tile food : this.foodList) {
			this.map[food.row()][food.col()] = Ilk.LAND;
		}
		this.foodList.clear();
		for (Tile dead : this.deadList) {
			this.map[dead.row()][dead.col()] = Ilk.LAND;
		}
		this.deadList.clear();
		// get new tile ilks
		for (String line : data) {
			String tokens[] = line.split(" ");
			if (tokens.length > 2) {
				int row = Integer.parseInt(tokens[1]);
				int col = Integer.parseInt(tokens[2]);
				if (tokens[0].equals("w")) {
			    	this.map[row][col] = Ilk.WATER;
			    } else if (tokens[0].equals("a")) {
			    	Ilk ilk = Ilk.fromId(Integer.parseInt(tokens[3]));
			    	this.map[row][col] = ilk;
			    	this.antList.put(new Tile(row, col), ilk);
			    } else if (tokens[0].equals("f")) {
			    	this.map[row][col] = Ilk.FOOD;
			    	this.foodList.add(new Tile(row, col));
			    } else if (tokens[0].equals("d")) {
			    	this.map[row][col] = Ilk.DEAD;
			    	this.deadList.add(new Tile(row, col));
			    }			
			}
		}
		return true;
	}
	
	public void issueOrder(int row, int col, Aim direction) {
		System.out.println("o " + row + " " + col + " " + direction.symbol);
		System.out.flush();
	}

	public void issueOrder(Tile ant, Aim direction) {
		System.out.println("o " + ant.row() + " " + ant.col() + " " + direction.symbol);
		System.out.flush();
	}
	
	public void finishTurn() {
		System.out.println("go");
		System.out.flush();
		this.turn++;
	}
	
	public Set<Tile> myAnts() {
		Set<Tile> myAnts = new HashSet<Tile>();
		for (Entry<Tile, Ilk> ant : this.antList.entrySet()) {
			if (ant.getValue() == Ilk.MY_ANT) {
				myAnts.add(ant.getKey());
			}
		}
		return myAnts;
	}
	
	public Set<Tile> enemyAnts() {
		Set<Tile> enemyAnts = new HashSet<Tile>();
		for (Entry<Tile, Ilk> ant : this.antList.entrySet()) {
			if (ant.getValue().isEnemy()) {
				enemyAnts.add(ant.getKey());
			}
		}
		return enemyAnts;
	}
	
	public Set<Tile> food() {
		return new HashSet<Tile>(this.foodList);
	}
	
	public int distance (Tile t1, Tile t2) {
		int dRow = Math.abs(t1.row() - t2.row());
		int dCol = Math.abs(t1.col() - t2.col());

		dRow = Math.min(dRow, this.rows - dRow);
		dCol = Math.min(dCol, this.cols - dCol);
		
		return dRow * dRow + dCol * dCol;
	}
	
	public List<Aim> directions (Tile t1, Tile t2) {
		List<Aim> directions = new ArrayList<Aim>();
		
		if (t1.row() < t2.row()) {
			if (t2.row() - t1.row() >= this.rows / 2) {
				directions.add(Aim.NORTH);
			} else {
				directions.add(Aim.SOUTH);
			}
		} else if (t1.row() > t2.row()) {
			if (t1.row() - t2.row() >= this.rows / 2) {
				directions.add(Aim.SOUTH);
			} else {
				directions.add(Aim.NORTH);
			}
		}

		if (t1.col() < t2.col()) {
			if (t2.col() - t1.col() >= this.cols / 2) {
				directions.add(Aim.WEST);
			} else {
				directions.add(Aim.EAST);
			}
		} else if (t1.col() > t2.col()) {
			if (t1.col() - t2.col() >= this.cols / 2) {
				directions.add(Aim.EAST);
			} else {
				directions.add(Aim.WEST);
			}
		}
		
		return directions;
	}
	
	public Ilk ilk(Tile location, Aim direction) {
		Tile new_location = this.tile(location, direction);
		return this.map[new_location.row()][new_location.col()];
	}
	
	public Ilk ilk(Tile location) {
		return this.map[location.row()][location.col()];
	}
	
	public Tile tile(Tile location, Aim direction) {
		int nRow = (location.row() + direction.dRow) % this.rows;
		if (nRow < 0) {
			nRow += this.rows;
		}
		int nCol = (location.col() + direction.dCol) % this.cols;
		if (nCol < 0) {
			nCol += this.cols;
		}
		return new Tile(nRow, nCol);
	}

	public static void run(Bot bot) {
		Ants ants = new Ants();
		StringBuffer line = new StringBuffer();
		ArrayList<String> data = new ArrayList<String>();
		int c;
		try {
		    while ((c = System.in.read()) >= 0) {
				switch (c) {
				case '\n':
				case '\r':
					if (line.length() > 0) {
						String full_line = line.toString();
						if (full_line.equals("ready")) {
							ants.setup(data);
							ants.finishTurn();
							data.clear();
						} else if (full_line.equals("go")) {
							ants.update(data);
							bot.do_turn(ants);
							ants.finishTurn();
							data.clear();
					    } else {
					    	if (line.length() > 0) {
					    		data.add(full_line);
					    	}
					    }
						line = new StringBuffer();
					}
				    break;
				default:
				    line.append((char)c);
				    break;
				}
		    }
		} catch (Exception e) {
			e.printStackTrace(System.err);
		}		
	}
}
