package zinc


def zinc(source: String) = for
  tokens <- Lexer.lex(source).left map (error => List(error))
yield tokens


@main
def main(): Unit =
  def prettyPrint(value: Any, indent: String = ""): Unit = value match {
    case prod: Product =>
      println(indent + prod.productPrefix + "(")
      val fields = prod.productIterator.toList
      fields.foreach(f => prettyPrint(f, indent + "  "))
      println(indent + ")")
    case seq: Seq[_] =>
      println(indent + "[")
      seq.foreach(e => prettyPrint(e, indent + "  "))
      println(indent + "]")
    case other =>
      println(indent + other.toString)
  }

  // tree structure for files
  // each file is a pair of its source map and its tokens
  // files and directories will be arranged in a binary tree so spans can easily find them, vs using an array
  val path = "src/main/testproject"
  prettyPrint(create(path))
// println(create(path))