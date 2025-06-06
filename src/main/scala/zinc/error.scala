package zinc

private[zinc] enum CompilerError:
  case NotCompilingADirectory, IOError
  case BadToken(pos: Int)
  case UnexpectedDelim(pos: Int)
  case MismatchedDelim(openPos: Int, closePos: Int)
  case UnclosedDelim(pos: Int)
  case UnexpectedToken(expected: Seq[TokenType], got: Token)
  case ErrorCompilingNumber(number: Token)

