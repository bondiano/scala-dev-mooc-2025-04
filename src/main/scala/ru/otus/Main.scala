package ru.otus

import scala.collection.View
import ru.otus.module1.collections.ListLike
import ru.otus.module1.{hof, pattern_matching, type_system, list, opt}


object App {
  def main(args: Array[String]): Unit = {

    println(s"Hello from ${Thread.currentThread().getName}")
    val t0 = new Thread{
      override def run(): Unit = {
        Thread.sleep(1000)
        println(s"Hello from ${Thread.currentThread().getName}")
      }
    }
//    val t1 = new concurrency.Thread1
//    t0.start()
//    t0.join()
//    t1.start()

//    def rates = {
//      getRatesLocation1.onComplete{
//        case Success(r1) =>
//          getRatesLocation2.onComplete{
//            case Success(r2) => println(r1 + r2)
//            case Failure(exception) => println(exception.getMessage)
//        }
//        case Failure(exception) => println(exception.getMessage)
//      }
//    }
//
//    getRatesLocation1.onComplete{ case tr1 =>
//      getRatesLocation2.onComplete { case tr2 =>
//            val r = for{
//              i1 <- tr1
//              i2 <- tr2
//            } yield (i1 + i2)
//            println(r)
//      }
//    }
//
//
//    printRunningTime(rates)

//    import scala.concurrent.ExecutionContext.Implicits.global
//    future.printRunningTime(future.rates)
//      .foreach(_ =>  executors.pool1.shutdown())
//    println("Hello here")

    ListLike(1, 2, 3).foreach(println)
    ListLike("a", "b").headOption.foreach(println)
  }
}