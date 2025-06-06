package zinc

import java.io.File
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Using}


case class ParsedFile(
  path: String,
  ast: List[Stmt],
  sourceMap: SourceMap,
  source: String,
)


private def getAllFiles(dir: File): Array[File] =
  val (directories, files) = dir.listFiles.partition(_.isDirectory)
  files ++ directories.flatMap(getAllFiles)

private type FileSystemParseResult = Either[CompilerError, (Vector[ParsedFile], List[CompilerError])]

def create(rootDirPath: String): FileSystemParseResult =

  val directory = new File(rootDirPath)
  if !directory.isDirectory then
    Left(CompilerError.NotCompilingADirectory)
  else
    getAllFiles(directory)
      .zipWithIndex
      .foldLeft(Right((Vector.empty, List.empty)): FileSystemParseResult) { case (result, (file, index)) =>
        result match
          case left@Left(err) => left
          case Right((fileAcc, errorAcc)) =>
            Using(Source.fromFile(file))(_.mkString) match
              case Failure(exception) => Left(CompilerError.IOError)
              case Success(source) =>
                val sourceMap = SourceMap.lineOffsets(source)
                val (stmts, errors) = ParserOps.parseFile(index, source)
                val newFileAcc = fileAcc :+ ParsedFile(file.getPath, stmts, sourceMap, source)
                val newErrorAcc = errors ++ errorAcc
                Right(newFileAcc, newErrorAcc)
      }