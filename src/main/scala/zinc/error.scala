package zinc

private[zinc] enum CompilerError:
  case BadToken(pos: Int)
  case UnexpectedDelim(pos: Int)
  case MismatchedDelim(openPos: Int, closePos: Int)
  case UnclosedDelim(pos: Int)

