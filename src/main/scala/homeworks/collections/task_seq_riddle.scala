package homeworks.collections

import scala.annotation.tailrec

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {
    @tailrec
    def innerRec(acc: List[Int], currentLine: List[Int]): List[Int] = currentLine match {
      case Nil => acc
      case currLine@(head :: _) =>
        val currList = currLine.takeWhile(_ == head)
        val currResList = List(currList.size, head)
        innerRec(acc ::: currResList, currLine.dropWhile(_ == head))
    }

    innerRec(Nil, currentLine)
  }


  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  lazy val funSeq: LazyList[List[Int]] = List(1) #:: funSeq.map(nextLine)

}