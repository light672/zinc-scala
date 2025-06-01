package zinc

import TokenType.{At, Bang, BangEqual, Dollar, EOF, Equal, EqualEqual, Greater, GreaterEqual, Identifier, Integer, LeftBrace, LeftBracket, LeftParen, Less, LessEqual, Let, Minus, MinusEqual, MinusMinus, Percent, PercentEqual, Plus, PlusEqual, PlusPlus, Pound, RightBrace, RightBracket, RightParen, Semicolon, Slash, SlashEqual, Star, StarEqual, Underscore}

import scala.annotation.tailrec
import scala.util.Try

private[zinc] def lex(source: String): Either[CompilerError, List[Token]] =
  @tailrec
  def skipWhiteSpace(pos: Int, source: String): Int =
    if pos >= source.length then
      pos
    else
      source(pos) match
        case ' ' | '\n' | '\r' | '\t' => skipWhiteSpace(pos + 1, source)
        case _ => pos

  def isAlpha(char: Char) = ('a' to 'z' contains char) || ('A' to 'Z' contains char) || char == '_'
  def isNumeric(char: Char) = '0' to '9' contains char
  def isAlphaNumeric(char: Char) = isAlpha(char) || isNumeric(char)
  inline def onNext(pos: Int, function: Char => Boolean, source: String) =
    if pos + 1 < source.length then
      function(source(pos + 1))
    else
      false

  @tailrec
  def identifier(pos: Int, source: String): Int =
    if pos >= source.length then
      pos
    else
      if isAlphaNumeric(source(pos)) then
        identifier(pos + 1, source)
      else
        pos

  def keyword(start: Int, end: Int, source: String): Token =
    source.subSequence(start, end) match
      case "let" => Token(start, end, Let)
      case identifier => Token(start, end, Identifier(identifier))

  @tailrec
  def number(pos: Int, source: String): Int =
    if pos >= source.length then
      pos
    else if isNumeric(source(pos)) then
      number(pos + 1, source)
    else
      pos


  @tailrec
  def lexBackwards(pos: Int, source: String, acc: List[Token], delimStack: List[Token]): Either[CompilerError, List[Token]] =
    val start = skipWhiteSpace(pos, source)
    if start >= source.length then
      delimStack.headOption match
        case None => Right(Token(start, start, EOF) :: acc)
        case Some(token) => Left(CompilerError.UnclosedDelim(token.start))
    else
      source(start) match
        case ';' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Semicolon) :: acc, delimStack)
        case '$' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Dollar) :: acc, delimStack)
        case '@' =>
          lexBackwards(start + 1, source, Token(start, start + 1, At) :: acc, delimStack)
        case '#' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Pound) :: acc, delimStack)
        case '(' =>
          val token = Token(start, start + 1, LeftParen)
          lexBackwards(start + 1, source, token :: acc, token :: delimStack)
        case ')' =>
          val token = Token(start, start + 1, RightParen)
          delimStack.headOption match
            case Some(Token(_, _, LeftParen)) =>
              lexBackwards(start + 1, source, token :: acc, delimStack drop 1)
            case Some(other) => Left(CompilerError.MismatchedDelim(other.start, start))
            case None => Left(CompilerError.UnexpectedDelim(start))
        case '{' =>
          val token = Token(start, start + 1, LeftBrace)
          lexBackwards(start + 1, source, token :: acc, token :: delimStack)
        case '}' =>
          val token = Token(start, start + 1, RightBrace)
          delimStack.headOption match
            case Some(Token(_, _, LeftBrace)) =>
              lexBackwards(start + 1, source, token :: acc, delimStack drop 1)
            case Some(other) => Left(CompilerError.MismatchedDelim(other.start, start))
            case None => Left(CompilerError.UnexpectedDelim(start))
        case '[' =>
          val token = Token(start, start + 1, LeftBracket)
          lexBackwards(start + 1, source, token :: acc, token :: delimStack)
        case ']' =>
          val token = Token(start, start + 1, RightBracket)
          delimStack.headOption match
            case Some(Token(_, _, LeftBracket)) =>
              lexBackwards(start + 1, source, token :: acc, delimStack drop 1)
            case Some(other) => Left(CompilerError.MismatchedDelim(other.start, start))
            case None => Left(CompilerError.UnexpectedDelim(start))

        case '!' if onNext(start, char => char == '=', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, BangEqual) :: acc, delimStack)
        case '!' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Bang) :: acc, delimStack)
        case '+' if onNext(start, char => char == '+', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, PlusPlus) :: acc, delimStack)
        case '+' if onNext(start, char => char == '=', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, PlusEqual) :: acc, delimStack)
        case '+' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Plus) :: acc, delimStack)
        case '-' if onNext(start, char => char == '-', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, MinusMinus) :: acc, delimStack)
        case '-' if onNext(start, char => char == '=', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, MinusEqual) :: acc, delimStack)
        case '-' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Minus) :: acc, delimStack)
        case '*' if onNext(start, char => char == '=', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, StarEqual) :: acc, delimStack)
        case '*' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Star) :: acc, delimStack)
        case '/' if onNext(start, char => char == '=', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, SlashEqual) :: acc, delimStack)
        case '/' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Slash) :: acc, delimStack)
        case '<' if onNext(start, char => char == '=', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, LessEqual) :: acc, delimStack)
        case '<' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Less) :: acc, delimStack)
        case '>' if onNext(start, char => char == '=', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, GreaterEqual) :: acc, delimStack)
        case '>' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Greater) :: acc, delimStack)
        case '%' if onNext(start, char => char == '=', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, PercentEqual) :: acc, delimStack)
        case '%' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Percent) :: acc, delimStack)
        case '=' if onNext(start, char => char == '=', source) =>
          lexBackwards(start + 2, source, Token(start, start + 2, EqualEqual) :: acc, delimStack)
        case '=' =>
          lexBackwards(start + 1, source, Token(start, start + 1, Equal) :: acc, delimStack)
        case '_' if onNext(start, char => !isAlphaNumeric(char), source) =>
          lexBackwards(start + 1, source, Token(start, start + 1, Underscore) :: acc, delimStack)

        case digit if isNumeric(digit) =>
          val current = number(start, source)
          val parsedNumber = Try(source.substring(start, current).toInt) getOrElse -1
          val newAcc = Token(start, current, Integer(parsedNumber)) :: acc
          lexBackwards(current, source, newAcc, delimStack)

        case letter if isAlpha(letter) =>
          val current = identifier(start, source)
          val token = keyword(start, current, source)
          val newAcc = token :: acc
          lexBackwards(current, source, newAcc, delimStack)

        case _ => Left(CompilerError.BadToken(start))

  lexBackwards(0, source, List(), List()) map (reversedTokens => reversedTokens.reverse)