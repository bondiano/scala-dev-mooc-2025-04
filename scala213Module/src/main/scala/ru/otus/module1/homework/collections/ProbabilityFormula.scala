package ru.otus.module1.homework.collections

import scala.util.Random

object ProbabilityFormula {
  /**
    * В урне 3 белых и 3 черных шара. Из урны дважды вынимают по одному шару, не возвращая их обратно. Найти вероятность появления белого шара

    * Как будем делать:
    * создать класс с моделированием эксперимента, в нем должна быть коллекция (List) моделирующая урну с шариками (1 - белый шарик, 0 - черный шарик) и функция случайного выбора 2х шариков без возвращения (scala.util.Random), возвращать эта функция должна true (если был выбран белый шар) и false (в противном случае)
    * создать коллекцию объектов этих классов, скажем 10000 элементов, и провести этот эксперимент (функция map)
    * посчитать количество элементов массива из пункта 2 где функция вернула true, это количество поделенное на общее количество элементов массива
    * PS: чем больше будет количество опытов в пункте 2, тем ближе будет результат моделирования к аналитическому решению
    */

  case class Experiment(urn: List[Int], random: Random) {
    def drawTwoBalls: List[Int] = {
      val shuffled = random.shuffle(urn)
      shuffled.take(2)
    }
  }

  def experiment(experiments: List[Experiment]): Double = {
    experiments
      .map(_.drawTwoBalls)
      .count(balls => balls.contains(1))
      .toDouble / experiments.size
  }

  def run() {
    val random = new Random()
    val experiments = List.fill(10000)(Experiment(List(1, 1, 1, 0, 0, 0), random))

    println(s"experiment(experiments) = ${experiment(experiments)}")

    val white = 3
    val black = 3
    val total = white + black
    val blackWays = (black * (black - 1)) / 2
    val totalWays = (total * (total - 1)) / 2
    val mathResult = 1.0 - blackWays.toDouble / totalWays
    println(s"mathResult = $mathResult")
  }

}