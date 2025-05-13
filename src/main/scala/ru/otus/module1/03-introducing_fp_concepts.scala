package ru.otus.module1

import ru.otus.module1.opt.{Animal, Cat, Dog}

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */




 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if(n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int, accum: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if(n <= 0) accum
      else loop(n - 1, n * accum)

    loop(n, 1)
  }





  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */


}



object hof{

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
      val start = System.currentTimeMillis()
      val result = f(a)
      val end = System.currentTimeMillis()
      println(s"Running time: ${end - start}")
      result
  }



  // изменение поведения ф-ции

  def isOdd(i: Int): Boolean = i % 2 > 0
  def not[A](f: A => Boolean): A => Boolean = a => !f(a)
  val isEven: Int => Boolean = not(isOdd)

  isOdd(5) // true
  isEven(5) // false


  // изменение самой функции

  def sum(x: Int, y: Int): Int = x + y

  def partial[A, B, C](a: A)(f: (A, B) => C): B => C =
    f.curried(a)


















}






/**
 *  Реализуем тип Option
 */



 object opt {



  trait Animal
  case class Cat() extends Animal
  case class Dog() extends Animal

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутствие результата
   */

  // Invariance
  // + Covariance Если А является подтипом В, то Option[A] является подтипом Option[B]
  // - Contravariance Если А является подтипом В, то Option[A] является супер типом Option[B]

    // Function1[-R, +T]
  val f1: String => Unit = ???
  val f2: Any => Unit = ???

  def foo(f: String => Unit) = f("Hello")

  foo(f2)
  foo(f1)


  sealed trait Option[+T] {
    def isEmpty: Boolean = if(this.isInstanceOf[None.type]) true else false

//    def get: T =  if(this.isInstanceOf[None.type]) throw new Exception("None get")
//      else{
//        val r = this.asInstanceOf[Some[T]]
//        r.v
//      }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = ???
  }

  case class Some[T](v: T) extends Option[T]

  case object None extends Option[Nothing]

  object Option {
    def apply[T](v: T): Option[T] =
      if(v == null) None else Some(v)
  }

  val opt1 : Option[Int] = ???

  val opt2: Option[Option[Int]] = opt1.map(i => Option(i + 1))
  val opt3: Option[Int] = opt1.flatMap(i => Option(i + 1))




  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */
  def printIfAny(opt: Option[String]): Unit =
    opt match {
      case Some(v) => println(v)
      case None => ()
    }

  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */
  def zip[A, B](opt1: Option[A], opt2: Option[B]): Option[(A, B)] =
    (opt1, opt2) match {
      case (Some(v1), Some(v2)) => Some((v1, v2))
      case _ => None
    }

  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */
  def filter[A](opt: Option[A])(p: A => Boolean): Option[A] = opt match {
    case Some(v) if p(v) => Some(v)
    case _ => None
  }
 }

 object list {
   /**
    *
    * Реализовать одно связанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */

   def treat(a: Option[Animal]) = ???




    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      *
      * Например, вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      *
      */
    sealed trait List[+T] {

     // prepend
    def ::[TT >: T](elem: TT): List[TT] =
      new ::(elem, this)
    }
    case class ::[T](elem: T, tail: List[T]) extends List[T]
    case object Nil extends List[Nothing]

   object List {
     def apply[A](v: A*): List[A] =
       if(v.isEmpty) Nil
       else new ::(v.head, apply(v.tail :_*))
   }

   val l1 = List(1, 2, 3)

   val l2: List[Cat] = List(Cat())

    /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */
    def reverse[A](list: List[A]): List[A] = {
      @tailrec
      def loop(list: List[A], acc: List[A]): List[A] = list match {
        case Nil => acc
        case ::(head, tail) => loop(tail, head :: acc)
      }

      loop(list, Nil)
    }


    def fold[A, B](list: List[A])(f: (List[B], A) => List[B]): List[B] = {
      @tailrec
      def loop(list: List[A], acc: List[B]): List[B] = list match {
        case Nil => reverse(acc)
        case ::(head, tail) => loop(tail, f(acc, head))
      }

      loop(list, Nil)
    }

    /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */
    def map[A, B](list: List[A])(f: A => B): List[B] =
      fold[A, B](list){(acc, elem) => f(elem) :: acc}

    /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */
    def filter[A](list: List[A])(p: A => Boolean): List[A] =
      fold[A, A](list){(acc, elem) => if (p(elem)) elem :: acc else acc}


    /**
      *
      * Написать функцию incList которая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */
    def incList(list: List[Int]): List[Int] =
      map(list)(_ + 1)

    /**
      *
      * Написать функцию shoutString которая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */
    def shoutString(list: List[String]): List[String] =
      map(list)(s => s"${s}!")

    def test = {
      val list1 = List(1, 2, 3)
      val list2 = List("a", "b", "c")
      val list3 = List(Cat(), Dog())

      println(incList(list1))
      println(shoutString(list2))
      println(filter(list3)(_.isInstanceOf[Cat]))
    }
 }