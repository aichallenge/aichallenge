class AntsGame {

  def run(bot: Bot) = {
    try {
      while (true) {
        val gameState = Parser.parse(System.in)
        val orders = bot.ordersFrom(gameState)
        orders.map(_.inServerSpeak).foreach(println)
      }
    } catch {
      case t => t.printStackTrace
    }
  }
  
}