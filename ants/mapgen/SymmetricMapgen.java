import java.util.Random;
import java.util.Stack;


public class SymmetricMapgen {

    public static void main(String[] args) {
        SymmetricMapgen map = new SymmetricMapgen();
        map.randomWalkMap();
        System.out.print(map.printMap());
    }

    private Random rnd = new Random();

    private final static char[] PRINT_OUTS = {'%', '.', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'};
    private final static int WATER = 0;
    private final static int LAND = 1;
    private final static int A_ANT = 2;

    private final static int[][] DIRECTIONS = {{-1, 0}, {1, 0}, {0, 1}, {0, -1}};

    //Game parameters
    private final static int MIN_PLAYERS = 4;
    private final static int MAX_PLAYERS = 10;

    private final static int MIN_DIM = 70;
    private final static int MAX_DIM = 120;
    private final static int MIN_START_DISTANCE = 30;

    private final static double MIN_LAND_PROPORTION = 0.5;
    private final static double MAX_LAND_PROPORTION = 0.8;
    private final static int NO_EXTRA_WALKS = 30;

    //Map parameters
    private int noPlayers = 0;
    private int rows = 0;
    private int cols = 0;
    private int rowT = 0;
    private int colT = 0;
    private int waterSquares = 0;
    private int landSquares = 0;

    private int[][] mapData;
    private int[] timesVisited;
    private int[] aLoc;
    private int[][] cLocs;

    //Makes a map by performing a bunch of random walks carving out water
    public void randomWalkMap() {
        pickDimensions();
        mapData = new int[rows][cols];
        addAnts();
        startWalks();
        addWalkLand();
    }

    public String printMap() {
        StringBuilder sb = new StringBuilder();
        sb.append("rows " + rows + "\n");
        sb.append("cols " + cols + "\n");
        sb.append("players " + noPlayers + "\n");

        for (int r = 0; r < rows; r++) {
            sb.append("m ");
            for (int c = 0; c < cols; c++) {
                sb.append(PRINT_OUTS[mapData[r][c]]);
            }
            sb.append("\n");
        }

        return sb.toString();
    }

    //Picks the dimensions of the map
    private void pickDimensions() {
        while (true) {
            while (true) {
                rows = rnd.nextInt(MAX_DIM - MIN_DIM) + MIN_DIM;
                cols = rnd.nextInt(MAX_DIM - MIN_DIM) + MIN_DIM;

                rowT = rnd.nextInt(rows - 8) + 4;
                colT = rnd.nextInt(cols - 8) + 4;

                if (rows / gdc(rowT, rows) == cols / gdc(colT, cols)) {
                    break;
                }
            }

            noPlayers = lcm(rows / gdc(rowT, rows), cols / gdc(colT, cols));

            //Forces a valid number of players all starting at a valid distance
            if (noPlayers >= MIN_PLAYERS && noPlayers <= MAX_PLAYERS && isValidStart()) {
                break;
            }
        }
    }

    //Checks whether the players start far enough apart
    private boolean isValidStart() {
        int[] loc = {0, 0};
        int[] nLoc = {0, 0};

        for (int i = 0; i < noPlayers - 1; i++) {
            nLoc = getTranslateLoc(nLoc);
            if (distance(loc, nLoc) < MIN_START_DISTANCE) {
                return false;
            }
        }

        return true;
    }

    //Returns the new location after translating it by (rtranslate, ctranslate)
    private int[] getTranslateLoc(int[] loc) {
        return new int[] {(loc[0] + rowT) % rows, (loc[1] + colT) % cols};
    }

    //Returns the distance between two squares
    private double distance(int[] loc1, int[] loc2) {
        int d1 = Math.abs(loc1[0] - loc2[0]);
        int d2 = Math.abs(loc1[1] - loc2[1]);
        int dr = Math.min(d1, rows - d1);
        int dc = Math.min(d2, cols - d2);

        return Math.sqrt(dr * dr + dc * dc);
    }

    //Adds ants to the map
    private void addAnts() {
        landSquares = noPlayers;
        aLoc = pickSquare();
        fillSquares(aLoc, A_ANT);
    }

    //Randomly picks a location inside the map
    private int[] pickSquare() {
        return new int[] {rnd.nextInt(rows - 1), rnd.nextInt(cols - 1)};
    }

    //Starts two random walks from the players starting ants
    private void startWalks() {
        cLocs = new int[NO_EXTRA_WALKS][2];
        cLocs[0] = aLoc;

        for (int i = 1; i < NO_EXTRA_WALKS; i++) {
            cLocs[i] = pickSquare();
        }
    }

    //Adds land to a map of water
    private void addWalkLand() {
        int min = (int)(MIN_LAND_PROPORTION * (double)rows * (double)cols);
        int max = (int)(MAX_LAND_PROPORTION * (double)rows * (double)cols);

        int noLandSquares = rnd.nextInt(max - min) + min;

        while(landSquares < noLandSquares || isValid()) {
            walkLocations();

            for (int i = 0; i < cLocs.length; i++) {
                if (mapData[cLocs[i][0]][cLocs[i][1]] == WATER) {
                    landSquares += noPlayers;
                    fillSquares(cLocs[i], LAND);
                }
            }
        }
    }

    //Checks whether the players can reach every non-wall square
    private boolean isValid() {
        int[] startLoc = aLoc;
        int[][] visited = new int[rows][cols];

        visited[startLoc[0]][startLoc[1]] = 1;
        int squaresVisited = 1;

        Stack<int[]> stack = new Stack<int[]>();
        stack.add(startLoc);

        while (!stack.empty()) {
            int[] tmpLoc = stack.pop();

            for (int i = 0; i < DIRECTIONS.length; i++) {
                int[] newLoc = getLoc(tmpLoc, DIRECTIONS[i]);

                if (visited[newLoc[0]][newLoc[1]] == 0) {
                    visited[newLoc[0]][newLoc[1]] = 1;
                    stack.add(newLoc);
                    squaresVisited++;
                }
            }
        }

        if (squaresVisited == landSquares) {
            return true;
        }

        return false;
    }

    //Walks the random walk locations
    private void walkLocations() {
        for (int i = 0; i < cLocs.length; i++) {
            int[] direction = DIRECTIONS[rnd.nextInt(DIRECTIONS.length)];
            cLocs[i] = getLoc(cLocs[i], direction);
        }
    }

    //Fills in all symmetrically equivalent squares with the given type
    private void fillSquares(int[] loc, int type) {
        int value = type;

        for (int i = 0; i < noPlayers; i++) {
            mapData[loc[0]][loc[1]] = value;

            if (type == A_ANT) {
                value++;
            }

            loc = getTranslateLoc(loc);
        }
    }

    //Returns the new location after moving in a particular direction
    private int[] getLoc(int[] loc, int[] direction) {
        int row = (loc[0] + direction[0]) % rows;
        int col = (loc[1] + direction[1]) % cols;

        if (row == -1) {
            row = rows - 1;
        }
        if (col == -1) {
            col = cols -1;
        }

        return new int[] {row, col};
    }

    private static int gdc(int a, int b) {
        if (b==0)
            return a;
        else
            return gdc(b, a % b);
    }

    private static int lcm(int a, int b) {
        return (a / gdc(a, b)) * b;
    }
}
