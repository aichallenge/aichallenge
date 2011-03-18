import java.io.ByteArrayInputStream
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
  }
}
