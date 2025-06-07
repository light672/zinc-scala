package zinc

import TokenType.*

private[zinc] case class Token
(
  start: Int,
  end: Int,
  ty: TokenType
):
  def toString(source: String): String = ty match
    case Semicolon => ";"
    case Dot => "."
    case Comma => ","
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
    case Integer | Identifier => source.substring(start, end)
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
    case Pipe => "|"
    case PipePipe => "||"
    case PipeEqual => "|="
    case Amp => "&"
    case AmpAmp => "&&"
    case AmpEqual => "&="
    case Tilda => "~"
    case Caret => "^"
    case CaretEqual => "^="

private[zinc] object Token {
  def empty: Token = Token(0, 0, TokenType.NA)
}

private[zinc] enum TokenType:
  case
  Semicolon, Dot, Comma, Tilda,
  Plus, PlusEqual, PlusPlus,
  Minus, MinusEqual, MinusMinus,
  Star, StarEqual,
  Slash, SlashEqual,
  Percent, PercentEqual,
  Caret, CaretEqual,
  Bang, BangEqual,
  Less, LessEqual,
  Greater, GreaterEqual,
  Underscore,
  Equal, EqualEqual,
  Pipe, PipePipe, PipeEqual,
  Amp, AmpAmp, AmpEqual,
  Pound,
  At,
  Dollar,
  LeftParen, RightParen,
  LeftBrace, RightBrace,
  LeftBracket, RightBracket,

  Let,
  EOF, NA,
  Integer, Identifier