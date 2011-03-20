import java.io.{FileInputStream, ByteArrayInputStream}
import org.specs2.mock.Mockito
import org.specs2.mutable._

class AntsGameSpec extends Specification with Mockito {

  "An ants game" should {
    "iterate once per turn until the end" in {
      val bot = mock[Bot]
      bot.ordersFrom(any[Game]) returns Set.empty[Order]
      val subject = new AntsGame(new ByteArrayInputStream("go\ngo\ngo\nend".getBytes))
      subject.run(bot)
      there were three(bot).ordersFrom(any[Game])
    }

    "propagate the parameters from one round to the next" in {
      var gameParameters: List[GameParameters] = Nil
      val bot = new Bot {
        def ordersFrom(gameState: Game) = {
          gameParameters = gameState.parameters :: gameParameters
          Set()
        }
      }
      val subject = new AntsGame(new FileInputStream("src/test/resources/sample.input"))
      subject.run(bot)
      val expectedParams = GameParameters(1000,1000,30,20,550,83,5,7)
      gameParameters must_== List(expectedParams, expectedParams)
    }
  }
}
