object HunterBot extends Application {
  new AntsGame().run(new HunterBot)
}

class HunterBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {

    // Your logic goes here.
    // for example ...

    val targets = (game.board.food ++ game.board.enemyAnts).values.toList
    val blockedTiles = game.board.myAnts.keySet ++ game.board.water.keySet

    val orders = game.board.myAnts.values.flatMap{ant =>
      val target = {
        val distance = (p: Positionable) => game.distanceFrom(ant.tile).to(p.tile)
        targets.sortWith(distance(_) < distance(_)).headOption
      }
      val aim = target.map(t => game.directionFrom(ant.tile).to(t.tile))
      val direction = aim.flatMap{a =>
        val directionPrecedence = a.toList match {
          case List(first, second) => first :: second :: first.opposite :: second.opposite :: Nil
          case List(first) => first :: first.lateral.toList ::: first.opposite :: Nil
        }
        directionPrecedence.find(d => !blockedTiles.contains(game.tile(d).of(ant.tile)))
      }
      direction.map(d => Order(ant.tile, d))
    }

    orders.toSet
  }
}
