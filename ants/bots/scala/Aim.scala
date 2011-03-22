
sealed trait CardinalPoint {
  val symbol: Char
  val lateral: Set[CardinalPoint]
  val opposite: CardinalPoint
}
case object North extends CardinalPoint {
  val symbol = 'n'
  val lateral: Set[CardinalPoint] = Set(East, West)
  val opposite: CardinalPoint = South
}
case object East extends CardinalPoint {
  val symbol = 'e'
  val opposite: CardinalPoint = West
  val lateral: Set[CardinalPoint] = Set(North, South)
}
case object South extends CardinalPoint {
  val symbol = 's'
  val opposite: CardinalPoint = North
  val lateral: Set[CardinalPoint] = Set(East, West)
}
case object West extends CardinalPoint {
  val symbol = 'w'
  val opposite: CardinalPoint = East
  val lateral: Set[CardinalPoint] = Set(North, South)
}
