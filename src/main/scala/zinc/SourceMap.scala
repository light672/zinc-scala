package zinc

import scala.annotation.tailrec

private[zinc] opaque type SourceMap = Vector[Int]

private[zinc] object SourceMap:
  def lineOffsets(source: String): SourceMap = source.zipWithIndex
    .collect { case ('\n', index) => index + 1 }
    .scanLeft(0)(_ max _)
    .toVector

  extension (self: SourceMap)
    def findLineIndex(sourceMap: SourceMap, pos: Int): Int =
      @tailrec
      def loop(left: Int, right: Int, best: Int): Int =
        if left > right then best
        else
          val mid = left + (right - left) / 2
          if sourceMap(mid) <= pos then
            loop(mid + 1, right, mid)
          else
            loop(left, mid - 1, best)

      loop(0, sourceMap.length - 1, -1)


