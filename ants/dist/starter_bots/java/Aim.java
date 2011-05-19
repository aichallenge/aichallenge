import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;


public enum Aim {
	NORTH (-1, 0, 'n'),
	EAST (0, 1, 'e'),
	SOUTH (1, 0, 's'),
	WEST (0, -1, 'w');
	
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
	
	public final int dCol;
	public final int dRow;
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
