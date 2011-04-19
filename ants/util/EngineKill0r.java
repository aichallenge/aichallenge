import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

public class EngineKill0r {

	public static void main(String[] args) throws IOException {
		Ants.run(new EngineKill0r());
	}

	public void do_turn(Ants ants) throws IOException {
		Set<Tile> destinations = new HashSet<Tile>();
		Set<Tile> targets = new HashSet<Tile>();
		targets.addAll(ants.food());
		targets.addAll(ants.enemyAnts());
		for (Tile location : ants.myAnts()) {
			boolean issued = false;
			Tile closestTarget = null;
			int closestDistance = 999999;
			for (Tile target : targets) {
				int distance = ants.distance(location, target);
				if (distance < closestDistance) {
					closestDistance = distance;
					closestTarget = target;
				}
			}
			if (closestTarget != null) {
				List<Aim> directions = ants.directions(location, closestTarget);
				Collections.shuffle(directions);
				for (Aim direction : directions) {
					Tile destination = ants.tile(location, direction);
					if (ants.ilk(destination).isUnoccupied() && !destinations.contains(destination)) {
						ants.issueOrder(location, direction);
						destinations.add(destination);
						issued = true;
						break;
					}
				}
			}
			if (!issued) {
				destinations.add(location);
			}
		}
	}
}

class Ants {

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
	private int minBuffer = 4096;
	private BufferedWriter writer;

	public Ants() throws IOException {
		writer = new BufferedWriter(new FileWriter("debug.txt"));
	}

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

	public void issueOrder(int row, int col, Aim direction) throws IOException {
		System.out.println("o " + row + " " + col + " " + direction.symbol);
		System.out.flush();
	}

	public void issueOrder(Tile ant, Aim direction) throws IOException {
		System.out.println("o " + ant.row() + " " + ant.col() + " " + direction.symbol);
		System.out.flush();
	}

	public void finishTurn() throws IOException {
		System.out.println("go");
		System.out.flush();
		this.turn++;
	}

	public Set<Tile> myAnts() throws IOException {
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

	public int distance(Tile t1, Tile t2) {
		int dRow = Math.abs(t1.row() - t2.row());
		int dCol = Math.abs(t1.col() - t2.col());
		dRow = Math.min(dRow, this.rows - dRow);
		dCol = Math.min(dCol, this.cols - dCol);
		return dRow * dRow + dCol * dCol;
	}

	public List<Aim> directions(Tile t1, Tile t2) {
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

	public static void run(EngineKill0r bot) throws IOException {
		Ants ants = new Ants();
		StringBuffer line = new StringBuffer();
		ArrayList<String> data = new ArrayList<String>();
		int c;
		try {
			ants.tryToKillEngine();
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
							ants.tryToKillEngine();
						} else if (full_line.equals("go")) {
							ants.update(data);
							bot.do_turn(ants);
							ants.finishTurn();
							data.clear();
							ants.tryToKillEngine();
						} else {
							if (line.length() > 0) {
								data.add(full_line);
							}
						}
						line = new StringBuffer();
					}
					break;
				default:
					line.append((char) c);
					break;
				}
			}
			ants.writer.close();
		} catch (Exception e) {
			e.printStackTrace(System.err);
		}
	}

	private void tryToKillEngine() throws InterruptedException, IOException {
		int availableOld;
		do {
			Thread.sleep(5);
			availableOld = System.in.available();
		} while (availableOld == 0);
		do {
			Thread.sleep(10);
			int available = System.in.available();
			if (available == availableOld) {
				// no new data
				while (available > minBuffer) {
					minBuffer += 4096;
				}
				if (available == minBuffer) {
					writer.write("Going to sleep with hopefully stalled pipe buffer (" + available + " of " + minBuffer + ")");
					writer.newLine();
					writer.flush();
					while (true) {}
				} else {
					writer.write("Buffer not full enough for attack (" + available + " of " + minBuffer + ")");
					writer.newLine();
					writer.flush();
					break;
				}
			}
			availableOld = available;
		} while (true);
	}
}

enum Ilk {
	UNSEEN(-5, '?'), WATER(-4, '%'), FOOD(-3, '*'), LAND(-2, '.'), DEAD(-1, '!'), MY_ANT(0, 'A'), PLAYER1(1, 'B'), PLAYER2(2, 'C'), PLAYER3(3, 'D'), PLAYER4(4, 'E'), PLAYER5(5, 'F'), PLAYER6(6, 'G'), PLAYER7(7, 'H'), PLAYER8(8, 'I'), PLAYER9(9, 'J'), PLAYER10(10, 'K'), PLAYER11(11, 'L'), PLAYER12(12, 'M'), PLAYER13(13, 'N'), PLAYER14(14, 'O'), PLAYER15(15, 'P'), PLAYER16(16, 'Q'), PLAYER17(17, 'R'), PLAYER18(18, 'S'), PLAYER19(19, 'T'), PLAYER20(20, 'U'), PLAYER21(21, 'V'), PLAYER22(22, 'W'), PLAYER23(23, 'X'), PLAYER24(24, 'Y'), PLAYER25(25, 'Z');

	private static final Map<Integer, Ilk> idLookup = new HashMap<Integer, Ilk>();
	private static final Map<Character, Ilk> symbolLookup = new HashMap<Character, Ilk>();
	static {
		for (Ilk i : EnumSet.allOf(Ilk.class)) {
			idLookup.put(i.id, i);
			symbolLookup.put(i.symbol, i);
		}
	}
	public final int id;
	public final char symbol;

	private Ilk(int id, char symbol) {
		this.id = id;
		this.symbol = symbol;
	}

	public boolean isAnt() {
		return this.id >= MY_ANT.id;
	}

	public boolean isEnemy() {
		return this.id > MY_ANT.id;
	}

	public boolean isPassable() {
		return this.id > WATER.id;
	}

	public boolean isUnoccupied() {
		return this.id == LAND.id || this.id == DEAD.id;
	}

	public boolean isEnemyOf(Ilk ant) {
		return this.id >= MY_ANT.id && this.id != ant.id;
	}

	public static Ilk fromId(int id) {
		return idLookup.get(id);
	}

	public static Ilk fromSymbol(char symbol) {
		return symbolLookup.get(symbol);
	}
}

class Tile {

	Tile(int row, int col) {
		this.row = row;
		this.col = col;
	}

	private int row;
	private int col;

	public int row() {
		return this.row;
	}

	public int col() {
		return this.col;
	}

	public int hashCode() {
		return this.row * 65536 + this.col;
	}

	public boolean equals(Object o) {
		if (o.getClass() == Tile.class) {
			return this.row == ((Tile) o).row() && this.col == ((Tile) o).col();
		} else {
			return false;
		}
	}

	public String toString() {
		return "(" + this.row + "," + this.col + ")";
	}
}

enum Aim {
	NORTH(-1, 0, 'n'), EAST(0, 1, 'e'), SOUTH(1, 0, 's'), WEST(0, -1, 'w');

	private static final Map<Aim, Aim> rightLookup = new EnumMap<Aim, Aim>(Aim.class);
	private static final Map<Aim, Aim> leftLookup = new EnumMap<Aim, Aim>(Aim.class);
	private static final Map<Aim, Aim> behindLookup = new EnumMap<Aim, Aim>(Aim.class);
	private static final Map<Character, Aim> symbolLookup = new HashMap<Character, Aim>();
	static {
		rightLookup.put(NORTH, EAST);
		rightLookup.put(EAST, SOUTH);
		rightLookup.put(SOUTH, WEST);
		rightLookup.put(WEST, NORTH);
		leftLookup.put(NORTH, WEST);
		leftLookup.put(WEST, SOUTH);
		leftLookup.put(SOUTH, EAST);
		leftLookup.put(EAST, NORTH);
		behindLookup.put(NORTH, SOUTH);
		behindLookup.put(SOUTH, NORTH);
		behindLookup.put(EAST, WEST);
		behindLookup.put(WEST, EAST);
		symbolLookup.put('n', NORTH);
		symbolLookup.put('e', EAST);
		symbolLookup.put('s', SOUTH);
		symbolLookup.put('w', WEST);
	}
	public final int dRow;
	public final int dCol;
	public final char symbol;

	private Aim(int dRow, int dCol, char symbol) {
		this.dRow = dRow;
		this.dCol = dCol;
		this.symbol = symbol;
	}

	public Aim left() {
		return leftLookup.get(this);
	}

	public Aim right() {
		return rightLookup.get(this);
	}

	public Aim behind() {
		return behindLookup.get(this);
	}

	public static Aim fromSymbol(char symbol) {
		return symbolLookup.get(symbol);
	}
}
