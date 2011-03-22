import annotation.tailrec
import io.Source
import java.io.{BufferedWriter, OutputStreamWriter, OutputStream, InputStream}

class AntsGame(in: InputStream = System.in, out: OutputStream = System.out) {

  val source = Source.fromInputStream(in)
  val writer = new BufferedWriter(new OutputStreamWriter(out))

  def run(bot: Bot) = {
    try {

      @tailrec
      def playNextTurn(game: Game): Unit = {
        val newGameState = Parser.parse(source, game.parameters)
        if (newGameState.gameOver) Unit
        else {
          val orders = bot.ordersFrom(newGameState)
          orders.map(_.inServerSpeak).foreach(writer.write)
          writer.write("go\n")
          writer.flush
          playNextTurn(newGameState)
        }
      }
      playNextTurn(GameInProgress())

    } catch {
      case t => t.printStackTrace
    }
  }
  
}
