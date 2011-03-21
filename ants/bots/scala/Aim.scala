
sealed trait CardinalPoint {
  val symbol: Char
}
object North extends CardinalPoint {
  val symbol = 'n'
}
object East extends CardinalPoint {
  val symbol = 'e'
}
object South extends CardinalPoint {
  val symbol = 's'
}
object West extends CardinalPoint {
  val symbol = 'w'
}
