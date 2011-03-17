import io.Source
import java.io.InputStream

class AntsGame(in: InputStream) {

  val source = Source.fromInputStream(in)

  def run(bot: Bot) = {
    try {
      while (true) {
        val gameState = Parser.parse(source)
        val orders = bot.ordersFrom(gameState)
        orders.map(_.inServerSpeak).foreach(println)
        println("go")
      }
    } catch {
      case t => t.printStackTrace
    }
  }
  
}