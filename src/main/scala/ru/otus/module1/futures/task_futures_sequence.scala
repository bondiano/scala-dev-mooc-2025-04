package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличие от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правовой результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])(implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val initialResult = Future.successful((List.empty[A], List.empty[Throwable]))

    futures.foldLeft(initialResult) { (acc, future) =>
      acc.flatMap { case (successes, failures) =>
        future.map(a => (a :: successes, failures)).recover { case e => (successes, e :: failures) }
      }
    }
    .map{case (successes, failures) => (successes.reverse, failures.reverse)}
  }

}
