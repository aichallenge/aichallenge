import io.Source
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class ParserSpec extends Specification with ScalaCheck {
  val gameParameterGenerator = for {
    loadTime <- Gen.oneOf(1 to 50000)
    turnTime <- Gen.oneOf(1 to 50000)
    rows <- Gen.oneOf(1 to 500)
    columns <- Gen.oneOf(1 to 500)
    turns <- Gen.oneOf(1 to 5000)
    viewRadius <- Gen.oneOf(1 to 200)
    attackRadius <- Gen.oneOf(1 to 200)
    spawnRadius <- Gen.oneOf(1 to 200)
  } yield(GameParameters(loadTime, turnTime, rows, columns, turns, viewRadius, attackRadius, spawnRadius))

  "The parse function" should {
    "parse valid turn zero data" ! checkProp {
      implicit def arb = Arbitrary{gameParameterGenerator}
      (params: GameParameters) =>
        val input = """turn 0
          loadtime %d
          turntime %d
          rows %d
          cols %d
          turns %d
          viewradius2 %d
          attackradius2 %d
          spawnradius2 %d
          ready""".format(params.loadTime, params.turnTime, params.rows, params.columns, params.turns,
          params.viewRadius, params.attackRadius, params.spawnRadius)
        val actualGame = Parser.parse(Source.fromString(input))
        actualGame must_== GameInProgress(parameters = params)
    }

    "parse valid turn +n data" in {
      val input = """
        turn 1
        f 6 5
        w 7 6
        a 7 9 1
        a 10 8 0
        a 10 9 0
        go"""
      val actualGame = Parser.parse(Source.fromString(input))
      actualGame must_== GameInProgress(turn = 1, board = Board(Map(
        Tile(6,5) -> Food(Tile(6,5)),
        Tile(7,6) -> Water(Tile(7,6)),
        Tile(7,9) -> Ant(Tile(7,9), false),
        Tile(10,8) -> Ant(Tile(10,8), true),
        Tile(10,9) -> Ant(Tile(10,9), true)
      )))
    }
  }

  "The parser" should {
    "assign default parameters if none are provided" in {
      val game = Parser.parse(Source.fromString("go"))
      game.parameters must_== GameParameters()
    }

    "use the provided parameters" in {
      val expectedParams = GameParameters(1,2,3,4,5,6,7,8)
      val game = Parser.parse(Source.fromString("go"), expectedParams)
      game.parameters must_== expectedParams
    }
  }
}