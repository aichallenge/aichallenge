case class Tile(column: Int, row: Int) {
  def directionTo(other: Tile) = {
    val ns: Set[CardinalPoint] = if (row < other.row) {
      if (other.row - row >= parameters.rows / 2) Set(North) else Set(South)
    } else if (one.row > other.row) {
      if (row - other.row >= parameters.rows / 2) Set(South) else Set(North)
    } else Set()

    val ew: Set[CardinalPoint] = if (one.column < other.column) {
      if (other.column - column >= parameters.columns / 2) Set(West) else Set(East)
    } else if (one.column > other.column) {
      if (column - other.column >= parameters.columns / 2) Set(East) else Set(West)
    } else Set()

    ns ++ ew
  }
}
