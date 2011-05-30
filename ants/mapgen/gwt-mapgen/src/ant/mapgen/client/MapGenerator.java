package ant.mapgen.client;

import java.util.Random;





public class MapGenerator {

    Random rnd = new Random();

    /** Constants */
    //Values used on the map
    public final static int WATER_TILE = 0;
    public final static int LAND_TILE = 1;
    public final static int INVALID_TILE = -1;
    public final static int ANT_START_POS_TILE = 2;

    //Map size
    private int maxMapSize = 120;
    private int minMapSize = 80;

    //For printout
    private final static String ROW_PREFIX = "m ";
    private final static String TILE_PREFIX = "";

    //Minimum start distance from the end of the map part. Distance between ants will be double this
    private int minStartDist = 3;

    /** Map settings */
    //Only 2, 4 or 8 players are currently valid
    private int nbrOfPlayers = 8;
    
    //Amount of map walkers (will be divided by amount of players)
    private int nbrOfWalkers = 20;

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
        MapWalker mapWalker = new MapWalker(nbrOfWalkers / nbrOfPlayers);
        mapPart = mapWalker.fillMap(mapPart, startPos);

        mapPart[startPos[0]][startPos[1]] = ANT_START_POS_TILE;

        //Build a full map from the map part
        int[][] fullMap = buildFullMap(mapPart);

        return fullMap;
    }

    public int getNbrOfWalkers() {
		return nbrOfWalkers;
	}

	public void setNbrOfWalkers(int nbrOfWalkers) {
		this.nbrOfWalkers = nbrOfWalkers;
	}

	/**
     * Translate a map into a string
     */
    public String map2String(int[][] map) {
        char ant = 'a';
        StringBuilder sb = new StringBuilder();

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
                        sb.append(TILE_PREFIX + ant++);
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
    	
        int rows =  maxMapSize;
        int cols = maxMapSize;
        
        if (maxMapSize > minMapSize) {
        	rows = rnd.nextInt(maxMapSize - minMapSize) + minMapSize;
        	cols = rnd.nextInt(maxMapSize - minMapSize) + minMapSize;
        }

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
            startRow = rnd.nextInt(map.length - minStartDist * 2) + minStartDist;
            startCol = rnd.nextInt(map.length - minStartDist * 2) + minStartDist;

            //Make sure we don't start in a place that will be removed
            if ((diagMode && nbrOfPlayers == 2) || nbrOfPlayers == 8) {
                if (startCol > startRow + minStartDist) {
                    break;
                }
            }
            else if (diagMode && nbrOfPlayers == 4) {
                if (startCol > startRow + minStartDist &&
                        (map[0].length - startCol) > startRow + minStartDist) {
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
                fullMap = overlayMaps(mapPart, mirrorDiagonal(mapPart));
            }
            else {
                fullMap = new int[mapPart.length * 2][mapPart[0].length];
                addMapPart(fullMap, mapPart, 0, 0);
                addMapPart(fullMap, mirrorTopBottom(mapPart), mapPart.length, 0);
            }
        }
        else if (nbrOfPlayers == 4) {
            if (diagMode) {
                fullMap = overlayMaps(mapPart, mirrorDiagonal(mapPart));
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
        int[][] res = clone(map);

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
        int[][] res = clone(map);

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
            for (int c = 0; c < res[0].length; c++) {
                if (r == 0 || r == res.length - 1 ||
                        c == 0 || c == res[0].length - 1) {
                    res[r][c] = 0;
                }
            }
        }

        return res;
    }
    
    /**
     * Clone a int array
     */
    private static int[][] clone(int[][] map) {
    	int[][] cloned = new int[map.length][map[0].length];

        for (int r = 0; r < map.length; r++) {
            for (int c = 0; c < map.length; c++) {
                cloned[r][c] = map[r][c];
            }
        }
    	
        return cloned;
    }

	public int getMaxMapSize() {
		return maxMapSize;
	}

	public void setMaxMapSize(int maxMapSize) {
		this.maxMapSize = maxMapSize;
	}

	public int getMinMapSize() {
		return minMapSize;
	}

	public void setMinMapSize(int minMapSize) {
		this.minMapSize = minMapSize;
	}

	public int getMinStartDist() {
		return minStartDist;
	}

	public void setMinStartDist(int minStartDist) {
		this.minStartDist = minStartDist;
	}

	public int getNbrOfPlayers() {
		return nbrOfPlayers;
	}

	public void setNbrOfPlayers(int nbrOfPlayers) {
		if (nbrOfPlayers != 2 && nbrOfPlayers != 4 && nbrOfPlayers != 8) {
			return; //Not valid
		}
		this.nbrOfPlayers = nbrOfPlayers;
	}

	public boolean isDiagMode() {
		return diagMode;
	}

	public void setDiagMode(boolean diagMode) {
		this.diagMode = diagMode;
	}

	public boolean isBorder() {
		return border;
	}

	public void setBorder(boolean border) {
		this.border = border;
	}

	public boolean isForceSquare() {
		return forceSquare;
	}

	public void setForceSquare(boolean forceSquare) {
		this.forceSquare = forceSquare;
	}
}
