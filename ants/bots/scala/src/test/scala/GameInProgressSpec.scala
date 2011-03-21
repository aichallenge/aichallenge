import org.specs2.mutable.Specification

class GameInProgressSpec extends Specification {

  "A game" should {
    val subject = GameInProgress(parameters = GameParameters(rows = 50, columns = 40))

    "find the tile north of another tile" in {
      val tile = subject.tile(North).of(Tile(row = 10, column = 20))
      tile must_== Tile(row = 9, column = 20)
    }
    "find the tile south of another tile" in {
      val tile = subject.tile(South).of(Tile(row = 10, column = 20))
      tile must_== Tile(row = 11, column = 20)
    }
    "find the tile east of another tile" in {
      val tile = subject.tile(East).of(Tile(row = 10, column = 20))
      tile must_== Tile(row = 10, column = 21)
    }
    "find the tile west of another tile" in {
      val tile = subject.tile(West).of(Tile(row = 10, column = 20))
      tile must_== Tile(row = 10, column = 19)
    }

    "find the tile north of another tile at the edge" in {
      val tile = subject.tile(North).of(Tile(row = 0, column = 20))
      tile must_== Tile(row = 49, column = 20)
    }
    "find the tile south of another tile at the edge" in {
      val tile = subject.tile(South).of(Tile(row = 49, column = 20))
      tile must_== Tile(row = 0, column = 20)
    }
    "find the tile east of another tile at the edge" in {
      val tile = subject.tile(East).of(Tile(row = 10, column = 39))
      tile must_== Tile(row = 10, column = 0)
    }
    "find the tile west of another tile at the edge" in {
      val tile = subject.tile(West).of(Tile(row = 10, column = 0))
      tile must_== Tile(row = 10, column = 39)
    }
  }

}
