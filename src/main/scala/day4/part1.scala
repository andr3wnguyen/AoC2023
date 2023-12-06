package day4

import scala.io.Source

object part1 extends App {

  val input = Source.fromFile("src/main/scala/day4/input.csv").getLines.toSeq

 print(input.map(x => calculateGameScore(x)).sum)

  def calculateGameScore(gamecard:String): Int = {
    val numbers = getNumbers(gamecard)
    val winningNumbers = getWinningNumbers(gamecard)
    calculateScore(countMatches(winningNumbers,numbers))
  }

  //helpers
  def getNumbers(string:String): Seq[Int] = {
    val rightSide = string.split("\\|")(1)
    rightSide.split("\\s").map(_.trim).filter(x=>x.nonEmpty).map(_.toInt)
  }

  def getWinningNumbers(string:String): Seq[Int] = {
    val leftSide = string.split("\\|")(0).split(":")(1)
    leftSide.split("\\s").map(_.trim).filter(x=>x.nonEmpty).map(_.toInt)
  }

  def countMatches(winningNumbers:Seq[Int], numbers: Seq[Int]): Int = {
    winningNumbers.count(winningNumber => numbers.contains(winningNumber))
  }

  def calculateScore(matches: Int): Int = {
    matches match {
      case 0 => 0
      case 1 => 1
      case _ => 1 * math.pow(2,matches-1).toInt
    }
  }
  //separate the values into 2 seq.

}
