case class Order(tile: Tile, point: CardinalPoint) {

  def inServerSpeak = "o %d %d %s".format(tile.column, tile.row, point.symbol)

}