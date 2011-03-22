
sealed trait Positionable {
  val tile: Tile
}
sealed trait Owned {
  val mine: Boolean
}
case class Ant(tile: Tile, mine: Boolean) extends Owned with Positionable
case class Corpse(tile: Tile, mine: Boolean) extends Owned with Positionable
case class Food(tile: Tile) extends Positionable
case class Water(tile: Tile) extends Positionable
