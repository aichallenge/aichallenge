
case class Board(elements: Map[Tile, Positionable] = Map.empty[Tile, Positionable]) {

  lazy val ants = elements.values.filter(_.isInstanceOf[Ant]).asInstanceOf[Seq[Ant]].toList
  lazy val food = elements.values.filter(_.isInstanceOf[Food]).asInstanceOf[Seq[Food]].toList
  lazy val (myAnts, enemyAnts) = ants.partition(_.mine)

  def add(p: Positionable) = this.copy(elements = elements.updated(p.tile, p))


}