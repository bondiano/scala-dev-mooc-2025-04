package ru.otus

import ru.otus.module1.{hof, type_system, list, opt}


object App {
  def main(args: Array[String]): Unit = {
    trait A extends Serializable
    object A

    list.test
  }
}