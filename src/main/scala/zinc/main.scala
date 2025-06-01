package zinc






extension (source: StringContext)
  def zinc(args: Any*): Either[List[CompilerError], List[Token]] = for
    tokens <- lex(source.s(args*)).left map (error => List(error))
  yield tokens

@main
def main(): Unit =
  val result = zinc"let a = 3; ("
  println(result)