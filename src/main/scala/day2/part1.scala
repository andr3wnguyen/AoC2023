package day2

import scala.io.Source

object part1 extends App {

  val inputSplitByGame = Source.fromFile("src/main/scala/day2/input.csv").getLines.flatMap(_.split("Game ")).toSeq.filter(_.nonEmpty)

  //  println(convertToMap(inputSplitByGame))
  println(calculateScores(inputSplitByGame))

  //  val mapOfGames = convertToMap(inputSplitByGame)
  //convert input to a map of gameId -> value


  def calculateScores(input: Seq[String]): Int = {
    var score = 0
    val mapOfInputs = convertToMap(input)
    for ((gameId, result) <- mapOfInputs) {
      if (parseString(result)) {
        score = score + gameId
      }
    }
    println(score)
    score
  }


  def convertToMap(input: Seq[String]): Map[Int, String] = {
    input.map {
      game =>
        game.split(":") match {
          case Array(game, score) => game.toInt -> score
        }
    }.toMap
  }

  //map of maps of number->colour

  //** maybe -> in the string, find 'x' then go back 2 indexes and look up the number.
  def parseString(string: String): Boolean = {
    //parse string and compare to allowed values
    val pattern = """(\d+)\s+(\w+)""".r
    val matches = pattern.findAllMatchIn(string)
    for {items <- matches} {
      val value = items.group(1).toInt
      val colour = items.group(2).toLowerCase

      // checks
      if ((colour == "red" && value > 12) ||
        (colour == "green" && value > 13) ||
        (colour == "blue" && value > 14)) {
        return false
      }
    }
    true
  }


}
