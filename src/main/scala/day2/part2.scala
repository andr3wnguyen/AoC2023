package day2

import scala.io.Source

object part2 extends App {
  val inputSplitByGame = Source.fromFile("src/main/scala/day2/input.csv").getLines.flatMap(_.split("Game ")).toSeq.filter(_.nonEmpty)

  //  println(convertToMap(inputSplitByGame))
  //  println(inputSplitByGame.map(game=>calculateCubes(game)))
  //  println(convertToMap(inputSplitByGame))

  //  for {mapItem <- convertToMap(inputSplitByGame)} {
  //    println(mapItem._2)
  //  }

  val productOfCubesPerGame = for {mapItem <- convertToMap(inputSplitByGame)} yield {
    (calculateCubes(mapItem._2))
  }
  println(productOfCubesPerGame.sum)


  //  val mapOfGames = convertToMap(inputSplitByGame)
  //convert input to a map of gameId -> value


  def calculateCubes(input: String): Int = {
    //replace the semicolons
    val cleanString = input.replace(";", ",").trim
    //split one commas, then create pairing of count -> colour
    val pairs = cleanString.split(", ").map { pair =>
      val Array(count, colour) = pair.split("\\s+")
      (count.toInt, colour)
    }
    //group by colours
    val groupedPairs = pairs.groupBy(_._2)

    //get the max - forEach group, get the max value
    val maxValues = groupedPairs.map { case (colour, pairs) =>
      colour -> pairs.maxBy(_._1)._1
    }
    //    println(maxValues)
    //multiply the  maxes
    val score = maxValues.values.product

    //return the score
    score
  }
  //for each game, find the minimum number of cubes of each colour and multiple them togehter.
  //find all green, get the max of that, find all red, get max of that, find all blue, get max of that.

  def convertToMap(input: Seq[String]): Map[Int, String] = {
    input.map {
      game =>
        game.split(":") match {
          case Array(game, score) => game.toInt -> score
        }
    }.toMap
  }
}
