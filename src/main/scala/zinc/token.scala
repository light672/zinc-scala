package zinc

import TokenType.*

case class Token
(
  val start: Int,
  val end: Int,
  val ty: TokenType
):
  override def toString: String = ty match
    case Semicolon => ";"
    case Plus => "+"
    case Minus => "-"
    case Star => "*"
    case Slash => "/"
    case Equal => "="
    case MinusEqual => "-="
    case StarEqual => "*="
    case SlashEqual => "/="
    case Let => "let"
    case PlusEqual => "+="
    case Integer(number) => number.toString
    case Identifier(lexeme) => lexeme.toString
    case EOF => "EOF"
    case NA => "NA"
    case Pound => "#"
    case LeftParen => "("
    case RightParen => ")"
    case LeftBrace => "{"
    case RightBrace => "}"
    case LeftBracket => "["
    case RightBracket => "]"
    case At => "@"
    case Percent => "%"
    case PercentEqual => "%="
    case Bang => "!"
    case BangEqual => "!="
    case Less => "<"
    case LessEqual => "<="
    case Greater => ">"
    case GreaterEqual => ">="
    case Underscore => "_"
    case PlusPlus => "++"
    case MinusMinus => "--"
    case EqualEqual => "=="
    case Dollar => "$"

object Token {
  def empty: Token = Token(0, 0, TokenType.NA)
}

enum TokenType:
  case
  Semicolon,
  Plus, PlusEqual, PlusPlus,
  Minus, MinusEqual, MinusMinus,
  Star, StarEqual,
  Slash, SlashEqual,
  Percent, PercentEqual,
  Bang, BangEqual,
  Less, LessEqual,
  Greater, GreaterEqual,
  Underscore,
  Equal, EqualEqual,
  Pound,
  At,
  Dollar,
  LeftParen, RightParen,
  LeftBrace, RightBrace,
  LeftBracket, RightBracket,

  Let,
  EOF, NA
  case Integer(number: Int)
  case Identifier(lexeme: CharSequence)