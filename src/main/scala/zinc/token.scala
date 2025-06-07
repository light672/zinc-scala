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
    case Colon => ":"
    case Dot => "."
    case DotDot => ".."
    case DotDotEqual => "..="
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
    case MinusArrow => "->"
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
    case LessLess => "<<"
    case GreaterGreater => ">>"
    case Greater3 => ">>>"
    case As => "as"
    case Pub => "pub"
    case Fn => "fn"
    case Struct => "struct"
    case Impl => "impl"
    case Trait => "trait"
    case Type => "type"
    case Const => "const"
    case Static => "static"
    case Mut => "mut"
    case True => "true"
    case False => "false"

private[zinc] object Token {
  def empty: Token = Token(0, 0, TokenType.NA)
}

private[zinc] enum TokenType:
  case
  Semicolon, Colon, Dot, DotDot, DotDotEqual, Comma, Tilda,
  Plus, PlusEqual, PlusPlus,
  Minus, MinusEqual, MinusMinus, MinusArrow,
  Star, StarEqual,
  Slash, SlashEqual,
  Percent, PercentEqual,
  Caret, CaretEqual,
  Bang, BangEqual,
  Less, LessEqual, LessLess,
  Greater, GreaterEqual, GreaterGreater, Greater3,
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

  Let, As, Pub, Fn, Struct, Impl, Trait, Type, Const, Static, Mut,
  EOF, NA,
  Integer, Identifier, True, False