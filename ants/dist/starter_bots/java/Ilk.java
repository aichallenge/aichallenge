import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public enum Ilk {
	UNSEEN (-5, '?'),
	WATER (-4, '%'),
	FOOD (-3, '*'),
	LAND (-2, '.'),
	DEAD (-1, '!'),
	MY_ANT (0, 'A'),
	PLAYER1 (1, 'B'),
	PLAYER2 (2, 'C'),
	PLAYER3 (3, 'D'),
	PLAYER4 (4, 'E'),
	PLAYER5 (5, 'F'),
	PLAYER6 (6, 'G'),
	PLAYER7 (7, 'H'),
	PLAYER8 (8, 'I'),
	PLAYER9 (9, 'J'),
	PLAYER10 (10, 'K'),
	PLAYER11 (11, 'L'),
	PLAYER12 (12, 'M'),
	PLAYER13 (13, 'N'),
	PLAYER14 (14, 'O'),
	PLAYER15 (15, 'P'),
	PLAYER16 (16, 'Q'),
	PLAYER17 (17, 'R'),
	PLAYER18 (18, 'S'),
	PLAYER19 (19, 'T'),
	PLAYER20 (20, 'U'),
	PLAYER21 (21, 'V'),
	PLAYER22 (22, 'W'),
	PLAYER23 (23, 'X'),
	PLAYER24 (24, 'Y'),
	PLAYER25 (25, 'Z');

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