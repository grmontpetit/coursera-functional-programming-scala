package week6

import java.io.InputStream

import scala.io.Source

object Lecture65 extends App {

  //val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
  val stream : InputStream = getClass.getResourceAsStream("/linuxwords.txt")
  val in = Source.fromInputStream(stream)

  val words = in.getLines().toList.filter(w => w.forall(c => c.isLetter))

  println(words.head)

  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] = mnem.flatMap(x => x._2.map(y => y -> x._1))

  def wordCode(word: String): String = word.toUpperCase.map(charCode)

  //println(wordCode("Java")) // expected: 5282

  val wordsForNum: Map[String, Seq[String]] =
    words.groupBy(wordCode).withDefaultValue(Seq())

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet
  }

  def translate(number: String): Set[String] =
    encode(number).map(_.mkString(" "))

  println(translate("7225247386"))
}