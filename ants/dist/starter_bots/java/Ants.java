import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Holds all game data and current game state.
 */
public class Ants {
    /** Maximum map size. */
    public static final int MAX_MAP_SIZE = 256;
    
    private final int loadTime;
    
    private final int turnTime;
    
    private final int rows;
    
    private final int cols;
    
    private final int turns;
    
    private final int viewRadius2;
    
    private final int attackRadius2;
    
    private final int spawnRadius2;
    
    private long turnStartTime;
    
    private final Ilk map[][];
    
    private final Set<Tile> myAnts = new HashSet<Tile>();
    
    private final Set<Tile> enemyAnts = new HashSet<Tile>();
    
    private final Set<Tile> foodTiles = new HashSet<Tile>();
    
    private final Set<Order> orders = new HashSet<Order>();
    
    /**
     * Creates new {@link Ants} object.
     * 
     * @param loadTime timeout for initializing and setting up the bot on turn 0
     * @param turnTime timeout for a single game turn, starting with turn 1
     * @param rows game map height
     * @param cols game map width
     * @param turns maximum number of turns the game will be played
     * @param viewRadius2 squared view radius of each ant
     * @param attackRadius2 squared attack radius of each ant
     * @param spawnRadius2 squared spawn radius of each ant
     */
    public Ants(int loadTime, int turnTime, int rows, int cols, int turns, int viewRadius2,
            int attackRadius2, int spawnRadius2) {
        this.loadTime = loadTime;
        this.turnTime = turnTime;
        this.rows = rows;
        this.cols = cols;
        this.turns = turns;
        this.viewRadius2 = viewRadius2;
        this.attackRadius2 = attackRadius2;
        this.spawnRadius2 = spawnRadius2;
        map = new Ilk[rows][cols];
        for (Ilk[] row : map) {
            Arrays.fill(row, Ilk.LAND);
        }
    }
    
    /**
     * Returns timeout for initializing and setting up the bot on turn 0.
     * 
     * @return timeout for initializing and setting up the bot on turn 0
     */
    public int getLoadTime() {
        return loadTime;
    }
    
    /**
     * Returns timeout for a single game turn, starting with turn 1.
     * 
     * @return timeout for a single game turn, starting with turn 1
     */
    public int getTurnTime() {
        return turnTime;
    }
    
    /**
     * Returns game map height.
     * 
     * @return game map height
     */
    public int getRows() {
        return rows;
    }
    
    /**
     * Returns game map width.
     * 
     * @return game map width
     */
    public int getCols() {
        return cols;
    }
    
    /**
     * Returns maximum number of turns the game will be played.
     * 
     * @return maximum number of turns the game will be played
     */
    public int getTurns() {
        return turns;
    }
    
    /**
     * Returns squared view radius of each ant.
     * 
     * @return squared view radius of each ant
     */
    public int getViewRadius2() {
        return viewRadius2;
    }
    
    /**
     * Returns squared attack radius of each ant.
     * 
     * @return squared attack radius of each ant
     */
    public int getAttackRadius2() {
        return attackRadius2;
    }
    
    /**
     * Returns squared spawn radius of each ant.
     * 
     * @return squared spawn radius of each ant
     */
    public int getSpawnRadius2() {
        return spawnRadius2;
    }
    
    /**
     * Sets turn start time.
     * 
     * @param turnStartTime turn start time
     */
    public void setTurnStartTime(long turnStartTime) {
        this.turnStartTime = turnStartTime;
    }
    
    /**
     * Returns how much time the bot has still has to take its turn before timing out.
     * 
     * @return how much time the bot has still has to take its turn before timing out
     */
    public int getTimeRemaining() {
        return turnTime - (int)(System.currentTimeMillis() - turnStartTime);
    }
    
    /**
     * Returns ilk at the specified location.
     * 
     * @param tile location on the game map
     * 
     * @return ilk at the <cod>tile</code>
     */
    public Ilk getIlk(Tile tile) {
        return map[tile.getRow()][tile.getCol()];
    }
    
    /**
     * Sets ilk at the specified location.
     * 
     * @param tile location on the game map
     * @param ilk ilk to be set at <code>tile</code>
     */
    public void setIlk(Tile tile, Ilk ilk) {
        map[tile.getRow()][tile.getCol()] = ilk;
    }
    
    /**
     * Returns ilk at the location in the specified direction from the specified location.
     * 
     * @param tile location on the game map
     * @param direction direction to look up
     * 
     * @return ilk at the location in <code>direction</code> from <cod>tile</code>
     */
    public Ilk getIlk(Tile tile, Aim direction) {
        Tile newTile = getTile(tile, direction);
        return map[newTile.getRow()][newTile.getCol()];
    }
    
    /**
     * Returns location in the specified direction from the specified location.
     * 
     * @param tile location on the game map
     * @param direction direction to look up
     * 
     * @return location in <code>direction</code> from <cod>tile</code>
     */
    public Tile getTile(Tile tile, Aim direction) {
        int row = (tile.getRow() + direction.getRowDelta()) % rows;
        if (row < 0) {
            row += rows;
        }
        int col = (tile.getCol() + direction.getColDelta()) % cols;
        if (col < 0) {
            col += cols;
        }
        return new Tile(row, col);
    }
    
    /**
     * Returns a set containing all my ants locations.
     * 
     * @return a set containing all my ants locations
     */
    public Set<Tile> getMyAnts() {
        return myAnts;
    }
    
    /**
     * Returns a set containing all enemy ants locations.
     * 
     * @return a set containing all enemy ants locations
     */
    public Set<Tile> getEnemyAnts() {
        return enemyAnts;
    }
    
    /**
     * Returns a set containing all food locations.
     * 
     * @return a set containing all food locations
     */
    public Set<Tile> getFoodTiles() {
        return foodTiles;
    }
    
    /**
     * Returns all orders sent so far.
     * 
     * @return all orders sent so far
     */
    public Set<Order> getOrders() {
        return orders;
    }
    
    /**
     * Calculates distance between two locations on the game map.
     * 
     * @param t1 one location on the game map
     * @param t2 another location on the game map
     * 
     * @return distance between <code>t1</code> and <code>t2</code>
     */
    public int getDistance(Tile t1, Tile t2) {
        int rowDelta = Math.abs(t1.getRow() - t2.getRow());
        int colDelta = Math.abs(t1.getCol() - t2.getCol());
        rowDelta = Math.min(rowDelta, rows - rowDelta);
        colDelta = Math.min(colDelta, cols - colDelta);
        return rowDelta * rowDelta + colDelta * colDelta;
    }
    
    /**
     * Returns one or two orthogonal directions from one location to the another.
     * 
     * @param t1 one location on the game map
     * @param t2 another location on the game map
     * 
     * @return orthogonal directions from <code>t1</code> to <code>t2</code>
     */
    public List<Aim> getDirections(Tile t1, Tile t2) {
        List<Aim> directions = new ArrayList<Aim>();
        if (t1.getRow() < t2.getRow()) {
            if (t2.getRow() - t1.getRow() >= rows / 2) {
                directions.add(Aim.NORTH);
            } else {
                directions.add(Aim.SOUTH);
            }
        } else if (t1.getRow() > t2.getRow()) {
            if (t1.getRow() - t2.getRow() >= rows / 2) {
                directions.add(Aim.SOUTH);
            } else {
                directions.add(Aim.NORTH);
            }
        }
        if (t1.getCol() < t2.getCol()) {
            if (t2.getCol() - t1.getCol() >= cols / 2) {
                directions.add(Aim.WEST);
            } else {
                directions.add(Aim.EAST);
            }
        } else if (t1.getCol() > t2.getCol()) {
            if (t1.getCol() - t2.getCol() >= cols / 2) {
                directions.add(Aim.EAST);
            } else {
                directions.add(Aim.WEST);
            }
        }
        return directions;
    }
    
    /**
     * Clears game state information about my ants locations.
     */
    public void clearMyAnts() {
        for (Tile myAnt : myAnts) {
            map[myAnt.getRow()][myAnt.getCol()] = Ilk.LAND;
        }
        myAnts.clear();
    }
    
    /**
     * Clears game state information about enemy ants locations.
     */
    public void clearEnemyAnts() {
        for (Tile enemyAnt : enemyAnts) {
            map[enemyAnt.getRow()][enemyAnt.getCol()] = Ilk.LAND;
        }
        enemyAnts.clear();
    }
    
    /**
     * Updates game state information about new ants and food locations.
     * 
     * @param ilk ilk to be updated
     * @param tile location on the game map to be updated
     */
    public void update(Ilk ilk, Tile tile) {
        map[tile.getRow()][tile.getCol()] = ilk;
        switch (ilk) {
            case FOOD:
                foodTiles.add(tile);
            break;
            case MY_ANT:
                myAnts.add(tile);
            break;
            case ENEMY_ANT:
                enemyAnts.add(tile);
            break;
        }
    }
    
    /**
     * Issues an order by sending it to the system output.
     * 
     * @param myAnt map tile with my ant
     * @param direction direction in which to move my ant
     */
    public void issueOrder(Tile myAnt, Aim direction) {
        Order order = new Order(myAnt, direction);
        orders.add(order);
        System.out.println(order);
        System.out.flush();
    }
}
