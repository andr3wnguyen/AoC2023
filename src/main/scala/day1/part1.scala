package day1

import scala.io.Source


object part1 extends App {

  //save the input to a list
  val input = Source.fromFile("src/main/scala/day1/input.csv").getLines.flatMap(_.split(",")).toSeq


  //loop and sum
  val answer = (input.map(weight => getWeightsFromString(weight))).sum
  //show me the money
  println(answer)


  //method to extract the numbers from the string and gets the first and last
  def getWeightsFromString(string:String): Int = {
    val numberString = getNumberString(string)
    getFirstAndLast(numberString)
  }

  //helpermethods//
  //method to check - takes a string and returns only the ints
  def getNumberString(string: String): String = {
    val numbersOnly = string.filter(character => character.isDigit)
    numbersOnly
  }

  def getFirstAndLast(numberString: String): Int = {
    s"${numberString.head}${numberString.last}".toInt
  }

}
