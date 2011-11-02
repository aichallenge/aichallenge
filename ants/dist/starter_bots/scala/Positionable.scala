sealed trait Positionable {
  val tile: Tile
}

case class MyAnt(tile: Tile) extends Positionable
case class EnemyAnt(tile: Tile) extends Positionable
case class Corpse(tile: Tile) extends Positionable
case class Food(tile: Tile) extends Positionable
case class Water(tile: Tile) extends Positionable
case class MyHill(tile: Tile) extends Positionable
case class EnemyHill(tile: Tile) extends Positionable
