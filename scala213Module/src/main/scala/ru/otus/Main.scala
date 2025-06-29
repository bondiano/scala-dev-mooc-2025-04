package ru.otus

import ru.otus.module1.{concurrency, executors, future}
import ru.otus.module1.concurrency.{getRatesLocation1, getRatesLocation2, printRunningTime}
import ru.otus.module2.{catsTypeClasses, functional, implicits, type_classes, validation}
import ru.otus.module3.functional_effects.functionalProgram.{declarativeEncoding, executableEncoding}

import scala.util.{Failure, Success}
import ru.otus.module2.catsHomework._
import cats.implicits._


object Main {


  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(1), Leaf(2))
    println(tree.map(x => x * 2))

    // declarativeEncoding.run(declarativeEncoding.greet2)
  }
}