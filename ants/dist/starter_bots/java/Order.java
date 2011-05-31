/**
 * Represents an order to be issued.
 */
public class Order {
    private final int row;
    
    private final int col;
    
    private final char direction;
    
    /**
     * Creates new {@link Order} object.
     * 
     * @param tile map tile with my ant
     * @param direction direction in which to move my ant
     */
    public Order(Tile tile, Aim direction) {
        row = tile.getRow();
        col = tile.getCol();
        this.direction = direction.getSymbol();
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return "o " + row + " " + col + " " + direction;
    }
}
