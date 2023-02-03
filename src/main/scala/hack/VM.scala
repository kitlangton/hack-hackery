package hack

object VM {
  //
}

sealed trait VmExpr extends Product with Serializable

object VmExpr {
  final case class Push(segment: Segment, index: Int) extends VmExpr
  final case class Pop(segment: Segment, index: Int)  extends VmExpr

  sealed trait Command extends VmExpr
  object Command {
    case object Push extends Command
    case object Pop  extends Command
    case object Add  extends Command
    case object Sub  extends Command
    case object Neg  extends Command
    case object Eq   extends Command
    case object Gt   extends Command
    case object Lt   extends Command
    case object And  extends Command
    case object Or   extends Command
    case object Not  extends Command
  }
}

sealed trait Segment extends Product with Serializable

object Segment {
  case object Argument extends Segment
  case object Local    extends Segment
  case object Static   extends Segment
  case object Constant extends Segment
  case object This     extends Segment
  case object That     extends Segment
  case object Pointer  extends Segment
  case object Temp     extends Segment
}
