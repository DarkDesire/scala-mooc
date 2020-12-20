package homeworks.futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  implicit val ec = ExecutionContext.global
  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]]): Future[(List[A], List[Throwable])] = {
    // замапили значения фьюч в Either
    val ss: List[Future[Either[Throwable, A]]] = futures.map(_.map(Right(_)).recover{case e => Left(e)})
    // сделали из списка фьюч - фьючу cо списком
    val transformed: Future[List[Either[Throwable, A]]] = Future.sequence(ss)
    // замапили выходной тип
    transformed
      .map { list: List[Either[Throwable, A]] =>
        val (items, errors) = list.foldRight[(List[A], List[Throwable])](Nil, Nil) {
          case (Left(error), (e, i)) => (e, error :: i)
          case (Right(result), (e, i)) => (result :: e, i)
        }
        (items, errors)
      }
  }
}
