import annotation.tailrec
import io.Source
import util.matching.Regex

object Parser {

  private val regularExpressions: Map[Regex, (GameInProgress, Seq[Int]) => GameInProgress] = Map(
    "a (\\d+ \\d+ \\d+)".r -> ((state, values: Seq[Int]) => state.copy(board = state.board.add(Ant(Tile(values(0), values(1)), values(2) == 0)))),
    "w (\\d+ \\d+)".r -> ((state, values: Seq[Int]) => state.copy(board = state.board.add(Water(Tile(values(0), values(1)))))),
    "f (\\d+ \\d+)".r -> ((state, values: Seq[Int]) => state.copy(board = state.board.add(Food(Tile(values(0), values(1)))))),
    "d (\\d+ \\d+ \\d+)".r -> ((state, values: Seq[Int]) => state.copy(board = state.board.add(Corpse(Tile(values(0), values(1)), values(2) == 0)))),
    "turn (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(turn = values(0))),
    "loadtime (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(parameters = state.parameters.copy(loadTime = values(0)))),
    "turntime (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(parameters = state.parameters.copy(turnTime = values(0)))),
    "rows (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(parameters = state.parameters.copy(rows = values(0)))),
    "cols (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(parameters = state.parameters.copy(columns = values(0)))),
    "turns (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(parameters = state.parameters.copy(turns = values(0)))),
    "viewradius2 (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(parameters = state.parameters.copy(viewRadius = values(0)))),
    "attackradius2 (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(parameters = state.parameters.copy(attackRadius = values(0)))),
    "spawnradius2 (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(parameters = state.parameters.copy(spawnRadius = values(0))))
  )

  def parse(source: Source, params: GameParameters = GameParameters()) = {
    val lines = source.getLines

    @tailrec
    def parseInternal(state: GameInProgress): Game = {
      val line = lines.next.trim
      line match {
        case "" => parseInternal(state)
        case "go" | "ready" => state
        case "end" => GameOver(turn = state.turn, parameters = state.parameters, board = state.board)
        case _ => {
          regularExpressions.find{case(regex, _) =>
            line.matches(regex.toString)
          }.map{case(regex, f) =>
            val regex(value) = line
            val values = value.split(" ").map(_.toInt)
            parseInternal(f(state, values))
          }.getOrElse(state)
        }
      }
    }

    parseInternal(GameInProgress(parameters = params))
  }
}