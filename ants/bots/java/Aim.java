import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;


public enum Aim {
	NORTH (0, -1, 'N'),
	EAST (1, 0, 'E'),
	SOUTH (0, 1, 'S'),
	WEST (-1, 0, 'W');
	
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
		symbolLookup.put('N', NORTH);
		symbolLookup.put('E', EAST);
		symbolLookup.put('S', SOUTH);
		symbolLookup.put('W', WEST);
	}
	
	public final int dCol;
	public final int dRow;
	public final char symbol;
	
	private Aim(int dCol, int dRow, char symbol) {
		this.dCol = dCol;
		this.dRow = dRow;
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
