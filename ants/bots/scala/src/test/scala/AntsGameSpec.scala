import java.io.ByteArrayInputStream
import org.specs2.mutable._

class AntsGameSpec extends Specification {

/*
  val bot = new Bot {
    def ordersFrom(gameState: Game) = {
      println(gameState)
      Set.empty[Order]
    }
  }
*/

  "An ants game" should {
    "Parse valid turn zero data" in {
/*
      val subject = new AntsGame(new ByteArrayInputStream(input.getBytes))
      subject.run(bot)
*/
      true must beTrue
    }
  }
}
