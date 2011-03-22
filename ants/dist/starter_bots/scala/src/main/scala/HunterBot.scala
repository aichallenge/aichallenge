object HunterBot extends Application {
  new AntsGame().run(new HunterBot)
}

class HunterBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {
    val targets = game.board.food ::: game.board.enemyAnts

    val antAndDirections = game.board.myAnts.map{ant =>
      val distance: (Positionable) => Double = (p) => game.distanceFrom(ant.tile).to(p.tile)
      val maybeTarget: Option[Positionable] = targets.sortWith(distance(_) < distance(_)).headOption
      val maybeDirection: Option[CardinalPoint] = maybeTarget.flatMap(target => game.directionsFrom(ant.tile).to(target.tile).headOption)
      (ant, maybeDirection)
    }.filter(_._2.isDefined).map{case(ant,maybeDirection) => (ant, maybeDirection.get)}

    val validOrders = antAndDirections.flatMap{case(ant,direction) =>
      val targetTile = game.tile(direction).of(ant.tile)
      game.board.elements.get(targetTile) match {
        case None | Some(Corpse(_, _)) => Some(Order(ant.tile, direction))
        case _ => None
      }
    }

    validOrders.toSet
  }

}