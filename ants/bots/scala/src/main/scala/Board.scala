case class Board(myAnts: Map[Tile, MyAnt] = Map(),
                 enemyAnts: Map[Tile, EnemyAnt] = Map(),
                 water: Map[Tile, Water] = Map(),
                 food: Map[Tile, Food] = Map(),
                 corpses: Map[Tile, Corpse] = Map()) {

  lazy val elements = myAnts ++ enemyAnts ++ water ++ food ++ corpses

  def including[P <: Positionable](positionable: P) = positionable match {
      case friend: MyAnt => this.copy(myAnts = this.myAnts.updated(friend.tile, friend))
      case enemy: EnemyAnt => this.copy(enemyAnts = this.enemyAnts.updated(enemy.tile, enemy))
      case puddle: Water => this.copy(water = this.water.updated(puddle.tile, puddle))
      case crumb: Food => this.copy(food = this.food.updated(crumb.tile, crumb))
      case corpse: Corpse => this.copy(corpses = this.corpses.updated(corpse.tile, corpse))
    }
}
