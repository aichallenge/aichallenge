import java.util.Random;
import java.util.Stack;

/**
 * Uses "walkers" that randomly moves around a water filled map and adds lands
 */
public class MapWalker {

    private static Random rnd = new Random();

    //Land proportions
    private final static double MIN_LAND_PROPORTION = 0.5;
    private final static double MAX_LAND_PROPORTION = 0.8;

    private final static int[][] DIRECTIONS = {{-1, 0}, {1, 0}, {0, 1}, {0, -1}};

    //Amount of walkers to use
    private int nbrOfWalkers;

    //Some temp values
    private int[][] tMap;
    private int[][] tMapWalkers;
    int tLandSquares = 0;

    /**
     * Uses walkers to fill a water map with land. Specify amount of walkers and
     * if it's a diagonal map.
     */
    public MapWalker(int nbrOfWalkers) {
        this.nbrOfWalkers = nbrOfWalkers;
    }

    /**
     * Fills the provided map with land
     */
    public int[][] fillMap(int[][] map, int[] startPos) {
        tMap = map;

        tMapWalkers = new int[nbrOfWalkers][2];
        tMapWalkers[0] = startPos;

        for (int i = 1; i < nbrOfWalkers; i++) {
            tMapWalkers[i] = pickRandomSquare();
        }

        addLand();

        return map;
    }

    /**
     * Randomly picks a valid location inside the map
     */
    private int[] pickRandomSquare() {
        int[] pos;
        do {
            pos = new int[] {rnd.nextInt(tMap.length - 1), rnd.nextInt(tMap[0].length - 1)};
        } while (tMap[pos[0]][pos[1]] == -1);

        return pos;
    }

    /**
     * Adds land to a map of water
     */
    private void addLand() {
        int waterSquares = countWaterSquares();
        int min = (int)(MIN_LAND_PROPORTION * (double)waterSquares);
        int max = (int)(MAX_LAND_PROPORTION * (double)waterSquares);

        int nbrOfLandSquares = rnd.nextInt(max - min) + min;

        while(tLandSquares < nbrOfLandSquares || !isValidMap()) {
            walkersInRandomDirection();

            for (int i = 0; i < tMapWalkers.length; i++) {
                if (tMap[tMapWalkers[i][0]][tMapWalkers[i][1]] == MapGenerator.WATER_TILE) {
                    tLandSquares++;
                    tMap[tMapWalkers[i][0]][tMapWalkers[i][1]] = MapGenerator.LAND_TILE;
                }
            }
        }
    }

    /**
     * Count amount of water squares in the map
     */
    private int countWaterSquares() {
        int amount = 0;

        for (int r = 0; r  < tMap.length; r++) {
            for (int c = 0; c < tMap[0].length; c++) {
                amount += tMap[r][c] == MapGenerator.WATER_TILE ? 1 : 0;
            }
        }

        return amount;
    }

    /**
     * Make all walkers move in a random direction
     */
    private void walkersInRandomDirection() {
        for (int i = 0; i < tMapWalkers.length; i++) {
            int[] direction = DIRECTIONS[rnd.nextInt(DIRECTIONS.length)];
            tMapWalkers[i] = getValidLocation(tMapWalkers[i], direction);
        }
    }

    /**
     * Returns a valid location based on the old location and a direction
     */
    private int[] getValidLocation(int[] loc, int[] direction) {
        int row = loc[0] + direction[0];
        int col = loc[1] + direction[1];

        row = Math.min(row, tMap.length - 1);
        row = Math.max(row, 0);

        col = Math.min(col, tMap[0].length - 1);
        col = Math.max(col, 0);

        return new int[] {row, col};
    }

    /**
     * Checks whether the players can reach every non-wall square
     */
    private boolean isValidMap() {
        int[][] visited = new int[tMap.length][tMap[0].length];

        visited[tMapWalkers[0][0]][tMapWalkers[0][1]] = 1;
        int squaresVisited = 1;

        Stack<int[]> stack = new Stack<int[]>();
        stack.add(new int[] {tMapWalkers[0][0], tMapWalkers[0][1]});

        while (!stack.empty()) {
            int[] tmpLoc = stack.pop();

            for (int i = 0; i < DIRECTIONS.length; i++) {
                int[] newLoc = getValidLocation(tmpLoc, DIRECTIONS[i]);

                if (visited[newLoc[0]][newLoc[1]] == 0 && tMap[newLoc[0]][newLoc[1]] == 1) {
                    visited[newLoc[0]][newLoc[1]] = 1;
                    stack.add(newLoc);
                    squaresVisited++;
                }
            }
        }

        if (squaresVisited == tLandSquares) {
            return true;
        }

        return false;
    }
}
