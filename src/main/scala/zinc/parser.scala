package zinc

import TokenType.Comma

import scala.annotation.{tailrec, targetName}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}


private[zinc] enum ParseError:
  case NoMatch(error: CompilerError)
  case Error(error: CompilerError)

private[zinc] case class ParseSuccess[+T](
  data: T,
  pos: Int,
  delimStack: List[Token]
)


private[zinc] type ParseResult[+T] = Either[ParseError, ParseSuccess[T]]
private[zinc] type Parser[+T] = (Int, String, List[Token]) => ParseResult[T]


extension [A](self: Parser[A])
  @targetName("or")
  def |[B](other: Parser[B]): Parser[A | B] = (pos, source, delimStack) =>
    self(pos, source, delimStack) match
      case Left(ParseError.NoMatch(_)) => other(pos, source, delimStack)
      case other => other
      
  @targetName("either")
  def <|>[B](other: Parser[B]): Parser[Either[A, B]] = (pos, source, delimStack) =>
    self(pos, source, delimStack) match
      case Left(ParseError.NoMatch(_)) => other.map(b => Right(b))(pos, source, delimStack)
      case Left(error@ParseError.Error(_)) => Left(error)
      case Right(ParseSuccess(data, pos, delimStack)) => Right(ParseSuccess(Left(data), pos, delimStack))

  @targetName("expect")
  def ! : Parser[A] = (pos, source, delimStack) =>
    self(pos, source, delimStack) match
      case Left(ParseError.NoMatch(error)) => Left(ParseError.Error(error))
      case Left(ParseError.Error(error)) => Left(ParseError.Error(error))
      case Right(ParseSuccess(data, pos, delimStack)) => Right(ParseSuccess(data, pos, delimStack))

  @targetName("optional")
  def ? : Parser[Option[A]] = (pos, source, delimStack) =>
    self(pos, source, delimStack) match
      case Left(ParseError.NoMatch(error)) => Right(ParseSuccess(None, pos, delimStack))
      case Left(error@ParseError.Error(_)) => Left(error)
      case Right(ParseSuccess(data, pos, delimStack)) => Right(ParseSuccess(Some(data), pos, delimStack))
      
  @targetName("optionalWithError")
  def ?# : Parser[Either[CompilerError, A]] = (pos, source, delimStack) =>
    self(pos, source, delimStack) match
      case Left(ParseError.NoMatch(error)) => Right(ParseSuccess(Left(error), pos, delimStack))
      case Left(error@ParseError.Error(_)) => Left(error)
      case Right(ParseSuccess(data, pos, delimStack)) => Right(ParseSuccess(Right(data), pos, delimStack))

  @targetName("loop")
  def * : Parser[List[A]] = (pos, source, delimStack) =>
    @tailrec
    def loop(
      parser: Parser[A],
      acc: List[A],
      pos: Int,
      source: String,
      delimStack: List[Token]
    ): ParseResult[List[A]] =
      parser(pos, source, delimStack) match
        case Left(ParseError.NoMatch(_)) => Right(ParseSuccess(acc.reverse, pos, delimStack))
        case Left(error@ParseError.Error(_)) => Left(error)
        case Right(ParseSuccess(data, pos, delimStack)) => loop(parser, data :: acc, pos, source, delimStack)
        
    loop(self, List.empty, pos, source, delimStack)




  def flatMap[B](function: A => Parser[B]): Parser[B] = (pos, source, delimStack) =>
    self(pos, source, delimStack) match
      case Left(error) => Left(error)
      case Right(value) => function(value.data)(value.pos, source, value.delimStack)

  def map[B](function: A => B): Parser[B] = (pos, source, delimStack) =>
    self(pos, source, delimStack).map {
      case ParseSuccess(data, pos, delimStack) => ParseSuccess(function(data), pos, delimStack)
    }

  def withFilter(pf: A => Boolean): Parser[A] = (pos, source, delimStack) =>
    self(pos, source, delimStack) match
      case right@Right(ParseSuccess(data, _, _)) if pf(data) => right
      case other => other


private[zinc] object ParserOps:

  def loopIf[A, B](parser: Parser[A], mapCondition: A => Option[B]): Parser[List[B]] =
    @tailrec
    def loop(
      parser: Parser[A],
      condition: A => Option[B],
      acc: List[B],
      pos: Int,
      source: String,
      delimStack: List[Token]
    ): ParseResult[List[B]] =
      parser(pos, source, delimStack) match
        case Left(ParseError.NoMatch(_)) => Right(ParseSuccess(acc.reverse, pos, delimStack))
        case Left(error@ParseError.Error(_)) => Left(error)
        case Right(ParseSuccess(option, pos, delimStack)) =>
          condition(option) match
            case Some(value) => loop(parser, condition, value :: acc, pos, source, delimStack)
            case None => Right(ParseSuccess(acc.reverse, pos, delimStack))

    (pos, source, delimStack) => loop(parser, mapCondition, List.empty, pos, source, delimStack)

  def peek(ty: TokenType): Parser[Token] = (pos, source, delimStack) =>
    Lexer.lexToken(pos, source, delimStack) match
      case Left(error) => Left(ParseError.Error(error))
      case Right((token, _, _)) => token.ty match
        case matched if matched == ty => Right(ParseSuccess(token, pos, delimStack))
        case _ => Left(ParseError.NoMatch(CompilerError.UnexpectedToken(Seq(ty), token)))

  def token(ty: TokenType): Parser[Token] = (pos, source, delimStack) =>
    Lexer.lexToken(pos, source, delimStack) match
      case Left(error) => Left(ParseError.Error(error))
      case Right((token, newPos, newDelimStack)) => token.ty match
        case matched if matched == ty => Right(ParseSuccess(token, newPos, newDelimStack))
        case _ => Left(ParseError.NoMatch(CompilerError.UnexpectedToken(Seq(ty), token)))


  def token(tys: TokenType*): Parser[Token] = (pos, source, delimStack) =>
    Lexer.lexToken(pos, source, delimStack) match
      case Left(error) => Left(ParseError.NoMatch(error))
      case Right((token, newPos, newDelimStack)) => token.ty match
        case matched if tys contains matched => Right(ParseSuccess(token, newPos, newDelimStack))
        case _ => Left(ParseError.NoMatch(CompilerError.UnexpectedToken(tys, token)))


  def commaGroup[T](left: TokenType, right: TokenType, _parser: => Parser[T]): Parser[(Token, Token, Either[T, List[T]])] =
    lazy val parser = _parser
    def fields(parser: Parser[T]) =
      val commaPortion = for
        comma <- token(Comma)
        p <- parser.?
      yield (comma, p)
      for
        initial <- parser
        list <- loopIf(commaPortion, (t, option) => option).?
      yield list match
        case Some(list) => Right(initial :: list)
        case None => Left(initial)

    for
      l <- token(left)
      p <- fields(parser).?.map {
        case Some(Left(single)) => Left(single)
        case Some(Right(list)) => Right(list)
        case None => Right(List.empty)
      }
      r <- token(right)
    yield (l, r, p)

  def parse[T](parser: Parser[T], source: String): ParseResult[T] =
    parser(0, source, List.empty)

  def parseFile(file: Int, source: String): (List[Stmt], List[CompilerError]) =
    parse(AST.expr, source) match
      case Left(ParseError.Error(err)) => (List.empty, List(err))
      case Left(ParseError.NoMatch(err)) => (List.empty, List(err))
      case Right(ParseSuccess(data, _, _)) => (List(Stmt.Expression(data, None)), List.empty)

