package zinc

import ParserOps.*
import TokenType.*

private[zinc] enum Expr:
  case Literal(token: Token)
  case Binary(left: Expr, right: Expr, operator: Token)
  case Range(left: Option[Expr], right: Option[Expr], operator: Token)
  case Call(callee: Expr, args: List[Expr])
  case Index(callee: Expr, args: List[Expr])
  case Tuple(fields: List[Expr])
  case Group(expr: Expr)
  case Unary(expr: Expr, operator: Token)
  case Block(statements: List[Stmt])

private[zinc] enum Stmt:
  case Expression(expr: Expr, semicolon: Option[Token])
  case Let(pattern: Pattern, ty: Option[Type], initializer: Option[Expr])

private[zinc] enum Pattern:
  case Identifier(token: Token)
  case MutIdentifier(token: Token)

private[zinc] enum Type:
  case Path(path: ComplexPath)

private[zinc] enum ComplexPath:
  case TypePath(body: List[ComplexSegment])

private[zinc] case class ComplexSegment(
  identifier: Token,
  generics: Option[GenericArgs]
)

private[zinc] case class GenericArgs(
  args: List[GenericArg]
)

private[zinc] enum GenericArg:
  case Simple(ty: Type)
  case Binding(identifier: Token, ty: Type)


private[zinc] object AST:
  def binary(_left: => Parser[Expr], _right: => Parser[Expr], symbols: TokenType*) =
    lazy val left = _left
    lazy val right = _right

    for
      leftExpr <- left
      infix <- (for
        op <- token(symbols *)
        r <- right.!
      yield (op, r)).?
    yield infix match
      case Some((op, rightExpr)) => Expr.Binary(leftExpr, rightExpr, op)
      case None => leftExpr

  lazy val expr: Parser[Expr] = assignment
  lazy val assignment: Parser[Expr] = binary(or, assignment, Equal)
  lazy val or: Parser[Expr] = binary(and, or, PipePipe)
  lazy val and: Parser[Expr] = binary(comp, and, AmpAmp)
  lazy val comp: Parser[Expr] = binary(bitOr, comp, EqualEqual, BangEqual, Less, LessEqual, Greater, GreaterEqual)
  lazy val bitOr: Parser[Expr] = binary(xor, bitOr, Pipe)
  lazy val xor: Parser[Expr] = binary(bitAnd, xor, Caret)
  lazy val bitAnd: Parser[Expr] = binary(shift, bitAnd, Amp)
  lazy val shift: Parser[Expr] = binary(range, shift, LessLess, GreaterGreater) // TODO: lex >>>

  lazy val range: Parser[Expr] =
    val infixParser = for
      op <- token(DotDot, DotDotEqual)
      r <- range.?
    yield (op, r)

    (for
      leftExpr <- term
      (op, rightExpr) <- infixParser
    yield Expr.Range(Some(leftExpr), rightExpr, op)) |
      infixParser.map((op, rightExpr) => Expr.Range(None, rightExpr, op))

  lazy val term: Parser[Expr] = binary(factor, term, Plus, Minus)
  lazy val factor: Parser[Expr] = binary(cast, factor, Star, Slash, Percent)

  lazy val cast: Parser[Expr] = binary(unary, cast, As)
  lazy val unary: Parser[Expr] = (for
    op <- token(Bang, Minus, Tilda)
    expr <- unary.!
  yield Expr.Unary(expr, op)) | call

  lazy val call: Parser[Expr] =
    enum CallType:
      case Call(left: Token, right: Token, args: List[Expr])
      case Index(left: Token, right: Token, args: List[Expr])
    lazy val callPrime =
      val callArgs = commaGroup(LeftParen, RightParen, expr).map { (left, right, args) =>
        CallType.Call(left, right, args.fold(left => List(left), right => right))
      }
      val indexArgs = commaGroup(LeftBracket, RightBracket, expr).map { (left, right, args) =>
        CallType.Index(left, right, args.fold(left => List(left), right => right))
      }
      callArgs | indexArgs
    for
      expr <- primary
      args <- callPrime.?
    yield args match
      case Some(CallType.Call(left, right, args)) => Expr.Call(expr, args)
      case Some(CallType.Index(left, rigth, args)) => Expr.Index(expr, args)
      case None => expr

  lazy val group: Parser[Expr] = for
    (left, right, group) <- commaGroup(LeftParen, RightParen, expr)
  yield group match
    case Left(expr) => Expr.Group(expr)
    case Right(fields) => Expr.Tuple(fields)

  lazy val number: Parser[Expr] = for
    number <- token(Integer)
  yield Expr.Literal(number)

  lazy val primary: Parser[Expr] = group | number





