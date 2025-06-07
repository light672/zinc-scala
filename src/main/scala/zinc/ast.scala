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
  case Fn(pub: Option[Token], name: Token, params: List[FunctionParam], returnTy: Option[Type], block: Either[Token, Expr.Block])
  case UnitStruct(pub: Option[Token], name: Token, semicolon: Token)


private[zinc] enum FunctionParam:
  case PatternParam(pattern: Pattern, ty: Type)
  case SelfParam(self: Token, ty: Option[Type])

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
  lazy val stmt: Parser[Stmt] =
    declaration | let | exprStmt

  lazy val exprStmt: Parser[Stmt] = for
    expr <- expr
    semicolon <- (token(Semicolon).map(t => Some(t)) | peek(RightBrace).map(_ => None)).!
  yield Stmt.Expression(expr, semicolon)

  lazy val declaration: Parser[Stmt] =
    fn |
      struct
  //impl |
  //`trait` |
  //typealias |
  //const |
  //static

  lazy val let: Parser[Stmt] = for
    let <- token(Let)
    pattern <- pattern.!
    ty <- (for
      _ <- token(Colon)
      ty <- ty.!
    yield ty).?
    init <- (for
      _ <- token(Equal)
      expr <- expr.!
    yield expr).?
    semicolon <- token(Semicolon).!
  yield Stmt.Let(pattern, ty, init)

  lazy val fn: Parser[Stmt] = for
    pub <- token(Pub).?
    fn <- token(Fn)
    name <- token(Identifier).!
    params <- commaGroup(LeftParen, RightParen, fnParam).map {
      case (_, _, Left(single)) => List(single)
      case (_, _, Right(list)) => list
    }.!
    returnTy <- (for
      arrow <- token(MinusArrow)
      ty <- ty.!
    yield ty).?
    block <- (token(Semicolon) <|> block).!
  yield Stmt.Fn(pub, name, params, returnTy, block)


  lazy val fnParam: Parser[FunctionParam] = for
    pattern <- pattern
    colon <- token(Colon).!
    ty <- ty.!
  yield FunctionParam.PatternParam(pattern, ty)

  lazy val struct: Parser[Stmt] = for
    pub <- token(Pub).?
    struct <- token(Struct)
    name <- token(Identifier).!
    semicolon <- token(Semicolon).!
  yield Stmt.UnitStruct(pub, name, semicolon)


  lazy val impl: Parser[Stmt] =
    ???

  lazy val `trait`: Parser[Stmt] =
    ???

  lazy val typealias: Parser[Stmt] =
    ???

  lazy val const: Parser[Stmt] =
    ???

  lazy val static: Parser[Stmt] =
    ???


  lazy val ty: Parser[Type] = for
    path <- typePath
  yield Type.Path(path)

  lazy val typePath: Parser[ComplexPath.TypePath] = for
    initial <- typePathSeg
    rest <- (for
      _ <- token(ColonColon)
      seg <- typePathSeg
    yield seg).*
  yield ComplexPath.TypePath(initial :: rest)
  lazy val typePathSeg: Parser[ComplexSegment] = token(Identifier).map(t => ComplexSegment(t, None))

  lazy val pattern: Parser[Pattern] = token(Identifier).map(t => Pattern.Identifier(t))

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
  lazy val shift: Parser[Expr] = binary(range, shift, LessLess, GreaterGreater, Greater3)

  lazy val range: Parser[Expr] =
    val infixParser = for
      op <- token(DotDot, DotDotEqual)
      r <- range.?
    yield (op, r)

    (for
      leftExpr <- term
      infix <- infixParser.?
    yield infix match
      case Some((op, rightExpr)) => Expr.Range(Some(leftExpr), rightExpr, op)
      case None => leftExpr
      ) | infixParser.map((op, rightExpr) => Expr.Range(None, rightExpr, op))

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

  lazy val block: Parser[Expr.Block] = for
    left <- token(LeftBrace)
    stmts <- stmt.*
    right <- token(RightBrace).!
  yield Expr.Block(stmts)


  lazy val number: Parser[Expr] = for
    number <- token(Integer)
  yield Expr.Literal(number)

  lazy val primary: Parser[Expr] = group | number





