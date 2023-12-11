package day3

import scala.io.Source

object part1 extends App {

  val input = Source.fromFile("src/main/scala/day3/input.csv").getLines.toSeq
  val splitInput = createSlidingWindows(input)

  println(check(splitInput).sum)
  println(input.map(x => getPartNumbersToIndexMap(x)))


  //get the

  def getPartNumbers(string: String): Seq[String] = {
    //parse through and return a list of part numbers in a string
    val digitRegex = """\d+""".r
    (digitRegex findAllIn string).toSeq
  }

  def getPartNumbersToIndexMap(string: String): Map[Int, Seq[Int]] = {
    //gets a list of the indexes (indices?) of where the part numbers are within the string, then map it to the actual number.
    // returns: Map(partNumber->ListOfIndex) for that line

    //using the getPartNumbers to get the initial index of the part numbers (e.g. if (121xxx13) would give List(0,1,2,6,7) -
    val partNumbersInString = getPartNumbers(string)
    //get indexes of numbers in the string, mapped to the actual numbers
    partNumbersInString.flatMap(partNumber => Map(partNumber.toInt -> (string.indexOf(partNumber) to (string.indexOf(partNumber) + (partNumber.length - 1))).toList)).toMap
  }

  def createSlidingWindows(input: Seq[String]): Seq[Seq[String]] = {
    //split the input up into sliding windows
    input.indices.flatMap { i =>
      if (i + 2 < input.length) Some(input.slice(i, i + 3))
      else if (i + 1 < input.length) Some(input.slice(i, i + 2))
      else None
    }
  }


  def getIndexOfSymbols(string: String): Seq[Int] = {
    //not a dot or a digit
    val symbols = List("*", "#", "+", "$", "=", "@", "/", "-", "%")
    //return an index of any symbols
    symbols.flatMap { symbol =>
      if (string.contains(symbol)) Some(string.indexOf(symbol))
      else None
    }
  }

  def getWindowForIndexOfSymbols(string: String): Seq[Int] = {
    val indexOfSymbols = getIndexOfSymbols(string)
    indexOfSymbols.flatMap(value => Seq(value - 1, value, value + 1)).distinct
  }

  //given a part number and a sequence of symbols from adjacent lines, if it is a valid part number, add it to result.
  def getValidPartNumbers(potentialParts: Map[Int, Seq[Int]], seqOfSymbolIndexes: Seq[Int]): Seq[Int] = {
    //get adjacent lines and add create a sequence - pass into this method alonside potentialparts
    var listOfPartNumbers = Seq[Int]()
    //if it is, return the Seq[Int] of valid parts.
    potentialParts.foreach { case (partNumber, listOfIndexes) => if (listOfIndexes.exists(index => seqOfSymbolIndexes.contains(index))) {
      listOfPartNumbers = listOfPartNumbers :+ partNumber
    }
    }
    listOfPartNumbers
  }

  //create a sliding window using the createSlidingWindows method
  // for each index in the Seq, get the line's adjacent lines and create a sequence containing the indexes + 1 (using getWindowForIndexOfSymbols). This is the Seq[Int]
  //use getValidPartNumbers to get valid part numbers from getPartNumbersToIndexMap and the index above.
  def check(input: Seq[Seq[String]]): Seq[Int] = {
    val inputWithIndex = input.zipWithIndex
    var result = Seq[Int]()
    val inputLength = input.length - 1


    inputWithIndex.foreach { case (strings, index) =>
      val validPartNumbers = index match {
        case 0 =>
          val partNumbers = getPartNumbersToIndexMap(strings(0)) //line to check
          val symbolIndexWindowStringBelow = getWindowForIndexOfSymbols(strings(1)) //line to check symbols against
          val symbolIndexesWindowInCurrentString = getWindowForIndexOfSymbols(strings(0))
          val indexesToQualifyValidPartNumber = (symbolIndexWindowStringBelow ++ symbolIndexesWindowInCurrentString).distinct
          getValidPartNumbers(partNumbers, indexesToQualifyValidPartNumber)
        // first input has one adjacent line below
        case i if i == inputLength =>
          val partNumbers = getPartNumbersToIndexMap(strings(1)) //line to check
          val symbolIndexWindowStringAbove = getWindowForIndexOfSymbols(strings(0)) //line to check symbols against
          val symbolIndexesWindowInCurrentString = getWindowForIndexOfSymbols(strings(1))
          val indexesToQualifyValidPartNumber = (symbolIndexWindowStringAbove ++ symbolIndexesWindowInCurrentString).distinct

          getValidPartNumbers(partNumbers, indexesToQualifyValidPartNumber)
        // last input has one adjacent line above
        case _ =>
          //make a seq with the surrounding lines
          val symbolIndexWindowStringAbove = getWindowForIndexOfSymbols(strings(0))
          val symbolIndexWindowCurrentString = getWindowForIndexOfSymbols(strings(1))
          val symbolIndexWindowStringBelow = getWindowForIndexOfSymbols(strings(2))
          val indexesToQualifyValidPartNumber = (symbolIndexWindowStringAbove ++ symbolIndexWindowCurrentString ++ symbolIndexWindowStringBelow).distinct

          val partNumbers = getPartNumbersToIndexMap(strings(1)) //line to check
          getValidPartNumbers(partNumbers, indexesToQualifyValidPartNumber)
        // every other input has 2 adjacent lines
      }
      result = result :++ validPartNumbers
    }
    result
  }
}
