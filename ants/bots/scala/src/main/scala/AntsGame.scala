import annotation.tailrec
import io.Source
import java.io.InputStream

class AntsGame(in: InputStream) {

  val source = Source.fromInputStream(in)

  def run(bot: Bot) = {
    try {

      @tailrec
      def playNextTurn(game: Game): Unit = {
        val newGameState = Parser.parse(source)
        if (newGameState.gameOver) Unit
        else {
          val orders = bot.ordersFrom(newGameState)
          orders.map(_.inServerSpeak).foreach(println)
          println("go")
          playNextTurn(newGameState)
        }
      }
      playNextTurn(GameInProgress())

    } catch {
      case t => t.printStackTrace
    }
  }
  
}