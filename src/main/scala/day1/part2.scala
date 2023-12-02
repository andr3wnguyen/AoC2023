package day1

import scala.io.Source

object part2 extends App {

  val input = Source.fromFile("src/main/scala/day1/input.csv").getLines.flatMap(_.split(",")).toSeq

// test it can decode the words
//  println(input.map(weight => decodeToInt(weight)))

  //loop and sum
    val answer = (input.map(weight => decodeToInt(weight))).sum
    //show me the money
    println(answer)

  //


  //helpers

  //look at the string, create another string , and decode it, left to right, if it contains a decoded value add the case to the new string.

  //extract the numbers and substrings into separate list of chars.

  //extractor methods

  //get all the numbers out that exist in this string as a string.
  def extractDigits(string: String): Seq[String] = {
    //if a digit is present, extract it into a seq
    string.collect {
      case character if character.isDigit => character
    }.split("")
  }

  def extractNumberWords(string: String): Seq[String] = {
    //go through the string and note every instance of the string
    val numberWords = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "zero")
    numberWords.filter(number => string.contains(number))
    //create a map that adds a value each time a word appears in the string, and add the index.
  }

  //******** TODO this method needs to consider duplicates... e.g. five five five
  //stringseq used as a reference of what the substrings to look for - bug with empty int lists being added so filtered at the end
  def createStringSeq(string: String): Seq[String] = {
    //make seq of digits into separate values..
    val seqOfDigits = extractDigits(string)
    val seqOfNumberWords = extractNumberWords(string)

    (seqOfDigits ++ seqOfNumberWords).filter(_.nonEmpty)
  }

  def createOrderedStringSeq(string: String): Seq[String] = {
    //extra values to get a Seq of numbers.
    val unorderedList = createStringSeq(string)

    //get the indexes of the values from a list. get a list of map values.
    val unorderedMapFirstIndexes = unorderedList.collect {
      case a if string.contains(a) => a -> string.indexOf(a)
    }.filter { case (_, position) => position >= 0 }

    val unorderedMapLastIndexes = unorderedList.collect {
      case a if string.contains(a) => a -> string.lastIndexOf(a)
    }.filter {
      {
        case (_, position) => position >= 0
      }
    }

    //get the last index as well. . . LOL .
    val unorderedMap = unorderedMapFirstIndexes ++ unorderedMapLastIndexes

    //sort by value and return a list of keys - returns a list of strings
    unorderedMap.sortBy(_._2).map { case (key, _) => key }
  }

  def getFirstAndLast(string: String): Seq[String] = {
    val orderedList = createOrderedStringSeq(string)
    Seq(orderedList.head, orderedList.last)
  }

  def decodeToInt(string: String): Int = {
    val firstAndLast = getFirstAndLast(string)
    val twoInts = firstAndLast.map {
      case s if s.matches("\\d+") => s.toInt
      case "one" => 1
      case "two" => 2
      case "three" => 3
      case "four" => 4
      case "five" => 5
      case "six" => 6
      case "seven" => 7
      case "eight" => 8
      case "nine" => 9
      case "zero" => 0
      case _ => -1 // Default value for non-matching cases
    }

    twoInts.mkString("").toInt
  }
}


//ISSUE IS THAT THERE CAN BE DUPLICATE WORDS. e.g. 64, five, five . . .and also single words (this is saved by head and last)