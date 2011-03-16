import annotation.tailrec
import io.Source
import java.io.InputStream
import util.matching.Regex

object Parser {

  private val regularExpressions: Map[Regex, (Game, Seq[Int]) => Game] = Map(
    "a (\\d+) (\\d+) (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(board = state.board.add(Ant(Tile(values(0), values(1)), values(2) == 0)))),
    "w (\\d+) (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(board = state.board.add(Water(Tile(values(0), values(1)))))),
    "f (\\d+) (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(board = state.board.add(Food(Tile(values(0), values(1)))))),
    "d (\\d+) (\\d+) (\\d+)".r -> ((state, values: Seq[Int]) => state.copy(board = state.board.add(Corpse(Tile(values(0), values(1)), values(2) == 0)))),
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

  def parse(in: InputStream) = {
    val lines = Source.fromInputStream(in).getLines

    @tailrec
    def parse(state: Game): Game = {
      val line = lines.next.trim
      if ("".equals(line)) parse(state)
      else if ("ready".equals(line)) state
      else if ("go".equals(line)) state
      else regularExpressions.find{case(regex, _) => line.matches(regex.toString)}.map{case(regex, f) =>
        val regex(value) = line
        val values = value.split(" ").map(_.toInt)
        parse(f(state, values))
      }.getOrElse(state)
    }

    parse(Game())
  }
}