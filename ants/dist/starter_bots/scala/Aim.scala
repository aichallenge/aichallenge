sealed trait CardinalPoint {
  val symbol: Char
}

case object North extends CardinalPoint {
  val symbol = 'n'
}
case object East extends CardinalPoint {
  val symbol = 'e'
}
case object South extends CardinalPoint {
  val symbol = 's'
}
case object West extends CardinalPoint {
  val symbol = 'w'
}

object CardinalPoint {
  def oppositeOf[P <: CardinalPoint](p: P) = {
    p match {
      case North => South
      case South => North
      case East => West
      case West => East
    }
  }
  def lateralTo[P <: CardinalPoint](p: P) = {
    p match {
      case North | South => Set(East, West)
      case East | West => Set(North, South)
    }
  }
}
