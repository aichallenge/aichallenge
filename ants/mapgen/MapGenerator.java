import java.util.Random;


public class MapGenerator {

    Random rnd = new Random();

    /** Constants */
    //Values used on the map
    public final static int WATER_TILE = 0;
    public final static int LAND_TILE = 1;
    public final static int INVALID_TILE = -1;
    private final static int ANT_START_POS_TILE = 2;

    //Map size
    private final static int MAX_MAP_SIZE = 120;
    private final static int MIN_MAP_SIZE = 80;

    //For printout
    private final static String ROW_PREFIX = "m ";
    private final static String TILE_PREFIX = "";

    //Minimum start distance from the end of the map part. Distance between ants will be double this
    private int MIN_START_DIST = 3;

    /** Map settings */
    //Only 2, 4 or 8 players are currently valid
    private int nbrOfPlayers = 8;

    //If the map should be divided diagonal. 8 players require diagMode. diagMode require width == height
    private boolean diagMode = false;

    //If the map should have a border or allow the ants to wrap to the other side
    private boolean border = true;

    //If the map should be squared or not. diagMode forces the map to be square
    private boolean forceSquare = false;

    /**
     * Generates a full map based on the map settings
     */
    public int[][] generateMap() {
        //Generate the map part that will be mirrored to create the full map
        int[][] mapPart = createMapPart();

        //Fetch the starting position
        int[] startPos = fetchStartPos(mapPart);

        //Fill the current map part with land
        MapWalker mapWalker = new MapWalker(20 / nbrOfPlayers);
        mapPart = mapWalker.fillMap(mapPart, startPos);

        mapPart[startPos[0]][startPos[1]] = ANT_START_POS_TILE;

        //Build a full map from the map part
        int[][] fullMap = buildFullMap(mapPart);

        return fullMap;
    }

    /**
     * Translate a map into a string
     */
    public String map2String(int[][] map) {
        int ant = 0;
        
        // This is totaly the wrong place to put this, but it's a quick ahck
        char[][] antss = {
        		// 8 player
        		{ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' },
        		{ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' },
        		{ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' },
        		// 4 player
        		{ 'a', 'a', 'b', 'c', 'b', 'c', 'd', 'd' },
        		{ 'a', 'b', 'c', 'c', 'd', 'd', 'a', 'b' },
        		{ 'a', 'b', 'a', 'b', 'c', 'd', 'c', 'd' },
        		// 2 player
        		{ 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b' },
        		{ 'a', 'b', 'a', 'b', 'b', 'a', 'b', 'a' },
        		{ 'a', 'b', 'b', 'a', 'a', 'b', 'b', 'a' }
        };
        int player_count = 0;
        int sym = rnd.nextInt(9);
        char[] ants = antss[sym];
        switch (sym) {
        // 8 players
        case 0:
        case 1:
        case 2:
        	player_count = 8;
        	break;
        // 4 players
        case 3:
        case 4:
        case 5:
        	player_count = 4;
        	break;
        // 2 players
        case 6:
        case 7:
        case 8:
        	player_count = 2;
        	break;        	
        }
        StringBuilder sb = new StringBuilder();
        sb.append("players " + Integer.toString(player_count) + "\n");
        sb.append("rows " + Integer.toString(map.length) + "\n");
        sb.append("cols " + Integer.toString(map.length) + "\n");
        for (int r = 0; r < map.length; r++) {
            sb.append(ROW_PREFIX);
            for (int c = 0; c < map[0].length; c++) {
                switch (map[r][c]) {
                    case WATER_TILE:
                        sb.append(TILE_PREFIX + "%");
                        break;
                    case LAND_TILE:
                        sb.append(TILE_PREFIX + ".");
                        break;
                    case ANT_START_POS_TILE:
                        sb.append(TILE_PREFIX + ants[ant++]);
                        break;
                    case INVALID_TILE:
                        sb.append(TILE_PREFIX + " ");
                        break;
                }
            }
            sb.append("\n");
        }

        return sb.toString();
    }

    /**
     * Prints a map
     */
    public void printMap(int[][] map) {
        System.out.println(map2String(map));
    }

    /**
     * Create a map with the right size based on the current settings
     * The map will consist of WATER_TILE's and INVALID_TILE's
     */
    private int[][] createMapPart() {
        int rows = rnd.nextInt(MAX_MAP_SIZE - MIN_MAP_SIZE) + MIN_MAP_SIZE;
        int cols = rnd.nextInt(MAX_MAP_SIZE - MIN_MAP_SIZE) + MIN_MAP_SIZE;

        int[][] mapPart;

        if (forceSquare || diagMode || nbrOfPlayers == 8) {
            cols = rows;
        }

        if (border) {
            rows = rows - 2;
            cols = cols -2;
        }

        if (!diagMode || nbrOfPlayers == 8) {
            rows = rows / 2;

            if (nbrOfPlayers > 2) {
                cols = cols / 2;
            }
        }

        mapPart = new int[rows][cols];

        //Remove one part of the map in diagmode
        if (diagMode || nbrOfPlayers == 8) {
            if (nbrOfPlayers == 4) {
                mapPart = clearLowerLeftDiag(clearLowerRightDiag(mapPart));
            }
            else {
                mapPart = clearLowerLeftDiag(mapPart);
            }
        }

        return mapPart;
    }

    /**
     * Randomize a starting position
     */
    private int[] fetchStartPos(int[][] map) {
        int startRow = 0;
        int startCol = 0;

        while (true) {
            startRow = rnd.nextInt(map.length - MIN_START_DIST * 2) + MIN_START_DIST;
            startCol = rnd.nextInt(map.length - MIN_START_DIST * 2) + MIN_START_DIST;

            //Make sure we don't start in a place that will be removed
            if ((diagMode && nbrOfPlayers == 2) || nbrOfPlayers == 8) {
                if (startCol > startRow + MIN_START_DIST) {
                    break;
                }
            }
            else if (diagMode && nbrOfPlayers == 4) {
                if (startCol > startRow + MIN_START_DIST &&
                        (map[0].length - startCol) > startRow + MIN_START_DIST) {
                    break;
                }
            }
            else {
                break;
            }
        }

        return new int[] {startRow, startCol};
    }

    /**
     * Take a generated map part and build a full map according to map settings
     */
    private int[][] buildFullMap(int[][] mapPart) {
        int[][] fullMap = null;

        if (nbrOfPlayers == 2) {
            if (diagMode) {
                fullMap = overlayMaps(fullMap, mirrorDiagonal(mapPart));
            }
            else {
                fullMap = new int[mapPart.length * 2][mapPart[0].length];
                addMapPart(fullMap, mapPart, 0, 0);
                addMapPart(fullMap, mirrorTopBottom(mapPart), mapPart.length, 0);
            }
        }
        else if (nbrOfPlayers == 4) {
            if (diagMode) {
                mapPart = mirrorDiagonal(mapPart);
                fullMap = overlayMaps(fullMap, mapPart);
                fullMap = overlayMaps(fullMap, mirrorDiagonal2(fullMap));
            }
            else {
                fullMap = new int[mapPart.length * 2][mapPart[0].length * 2];
                addMapPart(fullMap, mapPart, 0, 0);
                addMapPart(fullMap, mirrorTopBottom(mapPart), mapPart.length, 0);
                addMapPart(fullMap, mirrorLeftRight(mapPart), 0, mapPart[0].length);
                addMapPart(fullMap, mirrorBoth(mapPart), mapPart.length, mapPart[0].length);
            }
        }
        else if (nbrOfPlayers == 8) {
            mapPart = overlayMaps(mapPart, mirrorDiagonal(mapPart)); 

            fullMap = new int[mapPart.length * 2][mapPart[0].length * 2];
            addMapPart(fullMap, mapPart, 0, 0);
            addMapPart(fullMap, mirrorTopBottom(mapPart), mapPart.length, 0);
            addMapPart(fullMap, mirrorLeftRight(mapPart), 0, mapPart[0].length);
            addMapPart(fullMap, mirrorBoth(mapPart), mapPart.length, mapPart[0].length);
        }

        if (border) {
            fullMap = addBorder(fullMap);
        }

        return fullMap;
    }

    /**
     * Add a smaller map (mapPart) to to a larger map at the chosen selection
     */
    private void addMapPart(int[][] map, int[][] mapPart, int startRow, int startCol) {
        for (int r = startRow; r < map.length && r < mapPart.length + startRow; r++) {
            for (int c = startCol; c < map[0].length && c < mapPart[0].length + startCol; c++) {
                map[r][c] = mapPart[r - startRow][c - startCol];
            }
        }
    }

    /**
     * Mirror the map according to:
     * 123    789
     * 456 => 456
     * 789    123
     */
    private int[][] mirrorTopBottom(int[][] map) {
        int[][] res = new int[map.length][map[0].length];

        for (int r = 0; r < map.length; r++) {
            for (int c = 0; c < map[0].length; c++) {
                res[r][c] = map[map.length - r - 1][c];
            }
        }

        return res;
    }

    /**
     * Mirror the map according to:
     * 123    321
     * 456 => 654
     * 789    987
     */
    private int[][] mirrorLeftRight(int[][] map) {
        int[][] res = new int[map.length][map[0].length];

        for (int r = 0; r < map.length; r++) {
            for (int c = 0; c < map[0].length; c++) {
                res[r][c] = map[r][map[0].length - c - 1];
            }
        }

        return res;
    }

    /**
     * Mirror the map according to:
     * 123    987
     * 456 => 654
     * 789    321
     */
    private int[][] mirrorBoth(int[][] map) {
        return mirrorTopBottom(mirrorLeftRight(map));
    }

    /**
     * Mirror the map according to:
     * 123    147
     * 456 => 258
     * 789    987
     */
    private int[][] mirrorDiagonal(int[][] map) {
        int[][] res = new int[map.length][map[0].length];

        for (int r = 0; r < map.length; r++) {
            for (int c = 0; c < map[0].length; c++) {
                res[r][c] = map[c][r];
            }
        }

        return res;
    }

    /**
     *  Mirror the map according to:
     * 123    963
     * 456 => 852
     * 789    741
     */
    private int[][] mirrorDiagonal2(int[][] map) {
        int[][] res = new int[map.length][map[0].length];

        for (int r = 0; r < map.length; r++) {
            for (int c = 0; c < map[0].length; c++) {
                res[r][c] = map[map[0].length - c - 1][map.length - r - 1];
            }
        }

        return res;
    }

    /**
     * Overlays 2 maps (replaces -1's in the first map with numbers from the other)
     */
    private int[][] overlayMaps(int[][] mapA, int[][] mapB) {
        int[][] res = new int[mapA.length][mapA[0].length];

        for (int r = 0; r < mapA.length; r++) {
            for (int c = 0; c < mapA[0].length; c++) {
                if (mapA[r][c] == -1) {
                    res[r][c] = mapB[r][c];
                }
                else {
                    res[r][c] = mapA[r][c];
                }
            }
        }

        return res;
    }

    /**
     * Removes part of the map according to:
     * xxxx    xxxx
     * xxxx =>  xxx
     * xxxx      xx
     * xxxx       x
     */
    private int[][] clearLowerLeftDiag(int[][] map) {
        int[][] res = map.clone();

        for (int r = 0; r < res.length; r++) {
            for (int c = 0; c < res[0].length; c++) {
                if (c < r) {
                    res[r][c] = -1;
                }
            }
        }

        return res;
    }

    /**
     * Removes part of the map according to:
     * xxxx    xxxx
     * xxxx => xxx
     * xxxx    xx
     * xxxx    x
     */
    private int[][] clearLowerRightDiag(int[][] map) {
        int[][] res = map.clone();

        for (int r = 0; r < res.length; r++) {
            for (int c = 0; c < res[0].length; c++) {
                if ((res[0].length - c - 1) < r) {
                    res[r][c] = -1;
                }
            }
        }

        return res;
    }

    /**
     * Extend the map by 2 and set as border
     */
    private int[][] addBorder(int[][] map) {
        int[][] res = new int[map.length + 2][map[0].length + 2];
        addMapPart(res, map, 1, 1);

        for (int r = 0; r < res.length; r++) {
            for (int c = 0; c < res.length; c++) {
                if (r == 0 || r == res.length - 1 ||
                        c == 0 || c == res[0].length - 1) {
                    res[r][c] = 0;
                }
            }
        }

        return res;
    }
}
