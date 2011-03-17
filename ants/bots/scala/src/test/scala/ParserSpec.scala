import io.Source
import java.io.ByteArrayInputStream
import org.scalacheck.{Arbitrary, Gen, Prop}
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
        val actualGame = Parser.parse(Source.fromInputStream(new ByteArrayInputStream(input.getBytes)))
        actualGame must_== Game(parameters = params)
    }
  }
}