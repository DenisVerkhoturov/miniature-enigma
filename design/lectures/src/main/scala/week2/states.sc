class Pouring(capacity: Vector[Int]) {
  type Glass = Int
  // State
  type State = Vector[Glass]
  val initialState = capacity map (_ => 0)

  // Move
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Glass) extends Move {
    override def change(state: State) = state updated (glass, 0)
  }

  case class Fill(glass: Glass) extends Move {
    override def change(state: State) = state updated (glass, capacity(glass))
  }

  case class Pour(from: Glass, to: Glass) extends Move {
    override def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString = (history.reverse mkString " ") + "-->" + endState
  }

  val glasses = capacity.indices
  val moves = (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))
  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}

val problem = new Pouring(Vector(4, 9))
problem.moves
problem.pathSets.take(3).toList
problem.solutions(6)
