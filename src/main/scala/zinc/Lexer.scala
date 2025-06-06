package zinc

import TokenType.*

import scala.annotation.tailrec
import scala.util.Try


private[zinc] object Lexer:
  def lex(source: String): Either[CompilerError, List[Token]] =
    lexBackwards(0, source, List(), List()).map(reversedTokens => reversedTokens.reverse)

  def lexToken(pos: Int, source: String, delimStack: List[Token]): Either[CompilerError, (Token, Int, List[Token])] =
    val start = skipWhiteSpace(pos, source)
    if start >= source.length then
      delimStack.headOption match
        case None => Right((Token(start, start, EOF), start, delimStack))
        case Some(token) => Left(CompilerError.UnclosedDelim(token.start))
    else
      source(start) match
        case '@' => Right((Token(start, start + 1, At), start + 1, delimStack))
        case '~' => Right((Token(start, start + 1, Tilda), start + 1, delimStack))
        case ';' => Right((Token(start, start + 1, Semicolon), start + 1, delimStack))
        case '.' => Right((Token(start, start + 1, Dot), start + 1, delimStack))
        case ',' => Right((Token(start, start + 1, Comma), start + 1, delimStack))
        case '$' => Right((Token(start, start + 1, Dollar), start + 1, delimStack))
        case '#' => Right((Token(start, start + 1, Pound), start + 1, delimStack))
        case '(' =>
          val token = Token(start, start + 1, LeftParen)
          Right((token, start + 1, token :: delimStack))
        case ')' =>
          delimStack.headOption match
            case Some(Token(_, _, LeftParen)) =>
              Right((Token(start, start + 1, RightParen), start + 1, delimStack drop 1))
            case Some(other) => Left(CompilerError.MismatchedDelim(other.start, start))
            case None => Left(CompilerError.UnexpectedDelim(start))

        case '{' =>
          val token = Token(start, start + 1, LeftParen)
          Right((token, start + 1, token :: delimStack))
        case '}' =>
          delimStack.headOption match
            case Some(Token(_, _, LeftBrace)) =>
              Right((Token(start, start + 1, RightBrace), start + 1, delimStack drop 1))
            case Some(other) => Left(CompilerError.MismatchedDelim(other.start, start))
            case None => Left(CompilerError.UnexpectedDelim(start))

        case '[' =>
          val token = Token(start, start + 1, LeftBrace)
          Right((token, start + 1, token :: delimStack))
        case ']' =>
          delimStack.headOption match
            case Some(Token(_, _, LeftBracket)) =>
              Right((Token(start, start + 1, RightBracket), start + 1, delimStack drop 1))
            case Some(other) => Left(CompilerError.MismatchedDelim(other.start, start))
            case None => Left(CompilerError.UnexpectedDelim(start))

        case '!' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, BangEqual), start + 2, delimStack))
        case '!' => Right((Token(start, start + 1, Bang), start + 1, delimStack))
        case '+' if onNext(start, char => char == '+', source) =>
          Right((Token(start, start + 2, PlusPlus), start + 2, delimStack))
        case '+' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, PlusEqual), start + 2, delimStack))
        case '+' => Right((Token(start, start + 1, Plus), start + 1, delimStack))
        case '-' if onNext(start, char => char == '-', source) =>
          Right((Token(start, start + 2, MinusMinus), start + 2, delimStack))
        case '-' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, MinusEqual), start + 2, delimStack))
        case '-' => Right((Token(start, start + 1, Minus), start + 1, delimStack))
        case '*' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, StarEqual), start + 2, delimStack))
        case '*' => Right((Token(start, start + 1, Star), start + 1, delimStack))
        case '/' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, SlashEqual), start + 2, delimStack))
        case '/' => Right((Token(start, start + 1, Slash), start + 1, delimStack))
        case '<' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, LessEqual), start + 2, delimStack))
        case '<' => Right((Token(start, start + 1, Less), start + 1, delimStack))
        case '>' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, GreaterEqual), start + 2, delimStack))
        case '>' => Right((Token(start, start + 1, Greater), start + 1, delimStack))
        case '%' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, PercentEqual), start + 2, delimStack))
        case '%' => Right((Token(start, start + 1, Percent), start + 1, delimStack))
        case '^' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, CaretEqual), start + 2, delimStack))
        case '^' => Right((Token(start, start + 1, Caret), start + 1, delimStack))
        case '=' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, EqualEqual), start + 2, delimStack))
        case '=' => Right((Token(start, start + 1, Equal), start + 1, delimStack))
        case '|' if onNext(start, char => char == '|', source) =>
          Right((Token(start, start + 2, PipePipe), start + 2, delimStack))
        case '|' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, PipeEqual), start + 2, delimStack))
        case '|' => Right((Token(start, start + 1, Pipe), start + 1, delimStack))
        case '&' if onNext(start, char => char == '&', source) =>
          Right((Token(start, start + 2, AmpAmp), start + 2, delimStack))
        case '&' if onNext(start, char => char == '=', source) =>
          Right((Token(start, start + 2, AmpEqual), start + 2, delimStack))
        case '&' => Right((Token(start, start + 1, Amp), start + 1, delimStack))
        case '_' if onNext(start, char => !isAlphaNumeric(char), source) =>
          Right((Token(start, start + 1, Underscore), start + 1, delimStack))
        case digit if isNumeric(digit) =>
          val current = number(start, source)
          Right((Token(start, current, Integer), current, delimStack))
        case letter if isAlpha(letter) =>
          val current = identifier(start, source)
          Right((keyword(start, current, source), current, delimStack))
        case _ => Left(CompilerError.BadToken(start))

  @tailrec
  private def skipWhiteSpace(pos: Int, source: String): Int =
    if pos >= source.length then
      pos
    else
      source(pos) match
        case ' ' | '\n' | '\r' | '\t' => skipWhiteSpace(pos + 1, source)
        case _ => pos

  private def isAlpha(char: Char) = ('a' to 'z' contains char) || ('A' to 'Z' contains char) || char == '_'

  private def isNumeric(char: Char) = '0' to '9' contains char

  private def isAlphaNumeric(char: Char) = isAlpha(char) || isNumeric(char)

  private inline def onNext(pos: Int, function: Char => Boolean, source: String) =
    if pos + 1 < source.length then
      function(source(pos + 1))
    else
      false

  @tailrec
  private def identifier(pos: Int, source: String): Int =
    if pos >= source.length then
      pos
    else if isAlphaNumeric(source(pos)) then
      identifier(pos + 1, source)
    else
      pos

  private def keyword(start: Int, end: Int, source: String): Token =
    source.subSequence(start, end) match
      case "let" => Token(start, end, Let)
      case _ => Token(start, end, Identifier)

  @tailrec
  private def number(pos: Int, source: String): Int =
    if pos >= source.length then
      pos
    else if isNumeric(source(pos)) then
      number(pos + 1, source)
    else
      pos


  @tailrec
  private def lexBackwards(pos: Int, source: String, acc: List[Token], delimStack: List[Token]): Either[CompilerError, List[Token]] =
    lexToken(pos, source, delimStack) match
      case Left(error) => Left(error)
      case Right((token, current, newDelimStack)) => token.ty match
        case EOF => Right(token :: acc)
        case _ => lexBackwards(current, source, token :: acc, newDelimStack)

