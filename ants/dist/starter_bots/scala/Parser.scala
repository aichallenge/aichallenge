import annotation.tailrec
import io.Source
import util.matching.Regex

object Parser {

  def parse(source: Source, params: GameParameters = GameParameters(), knownWater: Map[Tile, Water] = Map.empty) = {
    val lines = source.getLines

    def parseInternal(state: GameInProgress): Game = {
      val line = lines.next.trim
      line match {
        case "" => parseInternal(state)
        case "go" | "ready" => state
        case "end" => GameOver(turn = state.turn, parameters = state.parameters, board = state.board)
        case _ => {
          regularExpressions.find{case(regex, _) => line.matches(regex.toString)}.map{case(regex, f) =>
            val regex(value) = line
            val values = value.split(" ").map(_.toInt)
            parseInternal(f(state, values))
          }.getOrElse(parseInternal(state))
        }
      }
    }

    parseInternal(GameInProgress(parameters = params, board = Board(water = knownWater)))
  }

  // The sequence of these is important. The parser will invoke the first that matches.
  private val regularExpressions: List[(Regex, (GameInProgress, Seq[Int]) => GameInProgress)] =
    ("a (\\d+ \\d+) 0".r, (game: GameInProgress, values: Seq[Int]) => game including MyAnt(tileFrom(values))) ::
    ("a (\\d+ \\d+ \\d+)".r, (game: GameInProgress, values: Seq[Int]) => game including EnemyAnt(tileFrom(values))) ::
    ("w (\\d+ \\d+)".r, (game: GameInProgress, values: Seq[Int]) => game including Water(tileFrom(values))) ::
    ("f (\\d+ \\d+)".r, (game: GameInProgress, values: Seq[Int]) => game including Food(tileFrom(values))) ::
    ("d (\\d+ \\d+ \\d+)".r, (game: GameInProgress, values: Seq[Int]) => game including Corpse(tileFrom(values))) ::
    ("h (\\d+ \\d+) 0".r, (game: GameInProgress, values: Seq[Int]) => game including MyHill(tileFrom(values))) ::
    ("h (\\d+ \\d+ \\d+)".r, (game: GameInProgress, values: Seq[Int]) => game including EnemyHill(tileFrom(values))) ::
    ("turn (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(turn = values(0))) ::
    ("loadtime (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(parameters = game.parameters.copy(loadTime = values(0)))) ::
    ("turntime (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(parameters = game.parameters.copy(turnTime = values(0)))) ::
    ("rows (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(parameters = game.parameters.copy(rows = values(0)))) ::
    ("cols (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(parameters = game.parameters.copy(columns = values(0)))) ::
    ("turns (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(parameters = game.parameters.copy(turns = values(0)))) ::
    ("seed (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(parameters = game.parameters.copy(seed = values(0)))) ::
    ("viewradius2 (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(parameters = game.parameters.copy(viewRadius = values(0)))) ::
    ("attackradius2 (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(parameters = game.parameters.copy(attackRadius = values(0)))) ::
    ("spawnradius2 (\\d+)".r, (game: GameInProgress, values: Seq[Int]) => game.copy(parameters = game.parameters.copy(spawnRadius = values(0)))) :: Nil

  private def tileFrom(values: Seq[Int]) = Tile(row = values(0), column = values(1))
}
