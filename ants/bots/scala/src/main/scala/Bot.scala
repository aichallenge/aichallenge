trait Bot extends Application {

  def ordersFrom(gameState: Game): Set[Order]

  new AntsGame().run(this)

}