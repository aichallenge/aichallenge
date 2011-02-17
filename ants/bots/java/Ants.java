import java.util.*;
import java.util.Map.Entry;

public class Ants {
	private int turn = 0;
	private int width = 0;
	private int height = 0;
	private Ilk map[][];
	private Map<Tile, Ilk> antList = new HashMap<Tile, Ilk>();
	private Set<Tile> foodList = new HashSet<Tile>();
	private Set<Tile> deadList = new HashSet<Tile>();
	
	public int turn() {
		return this.turn;
	}
	
	public int width() {
		return this.width;
	}
	
	public int height() {
		return this.height;
	}
		
	public boolean setup(List<String> data) {
		try {
			for (String line : data) {
			    String tokens[] = line.toUpperCase().split(" ");
				tokens[0] = tokens[0].toUpperCase();
			    if (tokens[0].equals("WIDTH")) {
			    	this.width = Integer.parseInt(tokens[1]);		    	
			    } else if (tokens[0].equals("HEIGHT")) {
			    	this.height = Integer.parseInt(tokens[1]);
			    }
			}
			this.map = new Ilk[this.height][this.width];
			for (Ilk[] row : this.map) {
				Arrays.fill(row, Ilk.UNSEEN);
			}
			return true;
		} catch (Exception ex) {
			return false;
		}
	}
		
	public boolean update(List<String> data) {
		return this.updateChanges(data);
	}
	
	private boolean updateChanges(List<String> data) {
		// clear ants and food
		//for (Tile ant : this.antList.keySet()) {
		//	this.map[ant.row()][ant.col()] = Ilk.LAND;
		//}
		this.antList.clear();
		//for (Tile food : this.foodList) {
		//	this.map[food.row()][food.col()] = Ilk.LAND;
		//}
		this.foodList.clear();
		//for (Tile dead : this.deadList) {
		//	this.map[dead.row()][dead.col()] = Ilk.LAND;
		//}
		this.deadList.clear();
		// get new tile ilks
		for (String line : data) {
			String tokens[] = line.split(" ");
			if (tokens.length > 2) {
				int col = Integer.parseInt(tokens[1]);
				int row = Integer.parseInt(tokens[2]);
				if (tokens[0].equals("L")) {
					this.map[row][col] = Ilk.LAND;
			    } else if (tokens[0].equals("W")) {
			    	this.map[row][col] = Ilk.WATER;
			    } else if (tokens[0].equals("A")) {
			    	Ilk ilk = Ilk.fromId(Integer.parseInt(tokens[3]));
			    	this.map[row][col] = ilk;
			    	this.antList.put(new Tile(col, row), ilk);
			    } else if (tokens[0].equals("F")) {
			    	this.map[row][col] = Ilk.FOOD;
			    	this.foodList.add(new Tile(col, row));
			    } else if (tokens[0].equals("D")) {
			    	this.map[row][col] = Ilk.DEAD;
			    	this.deadList.add(new Tile(col, row));
			    }			
			}
		}
		return true;
	}
	
	private boolean updateMap(List<String> data) {
		int row = 0;
		for (String line : data) {
			String tokens[] = line.split(" ");
			if (tokens[0].equals("M")) {
				int len = tokens[1].length();
				for (int col = 0; col < len; ++col) {
					char c = tokens[1].charAt(col);
					Ilk ilk = Ilk.fromSymbol(c);
					if (ilk == Ilk.UNSEEN) {
						if (this.map[row][col].isPassable()) {
							this.map[row][col] = Ilk.LAND;
						}
					} else {
						this.map[row][col] = Ilk.fromSymbol(c);
					}
				}
			}
			row++;
		}
		return true;
	}
	
	public void issueOrder(int row, int col, Aim direction) {
		System.out.println("O " + col + " " + row + " " + direction.symbol);
		System.out.flush();
	}

	public void issueOrder(Tile ant, Aim direction) {
		System.out.println("O " + ant.col() + " " + ant.row() + " " + direction.symbol);
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

		dRow = Math.min(dRow, this.height - dRow);
		dCol = Math.min(dCol, this.width - dCol);
		
		return dRow * dRow + dCol * dCol;
	}
	
	public List<Aim> directions (Tile t1, Tile t2) {
		List<Aim> directions = new ArrayList<Aim>();
		
		if (t1.col() < t2.col()) {
			if (t2.col() - t1.col() >= this.width / 2) {
				directions.add(Aim.WEST);
			} else {
				directions.add(Aim.EAST);
			}
		} else if (t1.col() > t2.col()) {
			if (t1.col() - t2.col() >= this.width / 2) {
				directions.add(Aim.EAST);
			} else {
				directions.add(Aim.WEST);
			}
		}
		
		if (t1.row() < t2.row()) {
			if (t2.row() - t1.row() >= this.height / 2) {
				directions.add(Aim.NORTH);
			} else {
				directions.add(Aim.SOUTH);
			}
		} else if (t1.row() > t2.row()) {
			if (t1.row() - t2.row() >= this.height / 2) {
				directions.add(Aim.SOUTH);
			} else {
				directions.add(Aim.NORTH);
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
		int nRow = (location.row() + direction.dRow) % this.height;
		if (nRow < 0) {
			nRow += this.height;
		}
		int nCol = (location.col() + direction.dCol) % this.width;
		if (nCol < 0) {
			nCol += this.width;
		}
		return new Tile(nCol, nRow);
	}

	public static void run(Runnable bot, Ants ants) {
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
							bot.run();
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
