package gks

import scala.io.Source

object Util {
  val files: Map[String, String] =
    Map(
      "kopelets" -> "kopelets.txt",
      "nika" -> "nika.txt",
      "myself" -> "input.txt"
    ).mapValues("input/" + _)

  val OperationList: List[List[String]] =
    Source
      .fromFile(files("kopelets"))
      .getLines
      .filter(_ != "")
      .toList
      .map(_
        .toString
        .split(" ")
        .toList
      )

  implicit class StringExtender(s: String) {
    val task: () => Unit = () => println(s"\n***\n[${s.toUpperCase}]\n***\n")

    val graphDir = s"graph/lab3-graph${s}.dot"
  }

}
