package chazarachart

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import chazarachart.hebrew.{FormatDate, FormatNumber}
import jewishdate.{JewishDate, JewishMonth}

case class Amud(daf: Int, amudBeis: Boolean) {
  def previous: Amud = this match {
    case Amud(n, true)  => Amud(n, amudBeis = false)
    case Amud(n, false) => Amud(n - 1, amudBeis = true)
  }
  def next: Amud     = this match {
    case Amud(n, false) => Amud(n, amudBeis = true)
    case Amud(n, true)  => Amud(n + 1, amudBeis = false)
  }

  override def toString =
    FormatNumber(daf, plain = true) + (if (amudBeis) ":" else ".")
}

enum Inclusion:
  case Rashi, Tosafos
  override def toString = this match
    case Rashi   => """עם רש"י"""
    case Tosafos => """עם תוספות"""

case class Limud(amud: Amud, including: Option[Inclusion])
trait ChazaraProgram {
  def limudim(newAmud: Amud): Seq[Limud]
}
object DefaultProgram extends ChazaraProgram {
  override def limudim(newAmud: Amud): Seq[Limud] =
    Seq(
      Limud(
        newAmud.previous.previous.previous.previous.previous.previous.previous.previous,
        None
      ),
      Limud(newAmud.previous, Some(Inclusion.Tosafos)),
      Limud(newAmud, Some(Inclusion.Rashi)),
      Limud(newAmud, None),
      Limud(newAmud.previous, None),
      Limud(newAmud, None)
    )
}

case class Day(date: JewishDate, amud: Amud) {
  def previous =
    Day(
      date = date.prev,
      amud = amud.previous
    )
  def next     =
    Day(
      date = date.next,
      amud = amud.next
    )
}
object Main                                  {
  private val day1                        =
    Day(
      JewishDate(5785, JewishMonth.Shvat, 15),
      Amud(50, false)
    ).previous.previous.previous.previous.previous.previous.previous.previous.previous.previous.previous.previous.previous
  private def generateTableRows(): String =
    Iterator
      .iterate(day1)(_.next)
      .map { case Day(jewishDate, amud) =>
        val limudim     = DefaultProgram.limudim(amud)
        val limudimCols = limudim.map { case Limud(amud, including) =>
          // language=html
          s"""<td>${amud.toString} ${including.mkString}</td>"""
        }
        // language=html
        s"""<tr class="day-${jewishDate.toLocalDate.getDayOfWeek}">
           |  <td>${FormatDate(jewishDate)}</td>
           |  ${limudimCols.mkString}
           |</tr>""".stripMargin
      }
      .take(50)
      .mkString("\n        ")

  def main(args: Array[String]): Unit = {
    val html =
      // language=html
      s"""<!DOCTYPE html>
         |<html lang="he">
         |<head>
         |    <title>Hebrew Counters Demo</title>
         |    <meta charset="UTF-8">
         |    <style>
         |      body {
         |        direction: rtl;
         |        background-color: white;
         |      }
         |      table {
         |        border-collapse: collapse;
         |        width: 100%;
         |      }
         |      tr.day-SATURDAY {
         |        background-color: #f2f2f2;
         |      }
         |      td {
         |        border: 1px solid #ddd;
         |        padding: 8px;
         |        text-align: right;
         |      }
         |      th {
         |        border: 1px solid #ddd;
         |        padding: 8px;
         |        background-color: #f2f2f2;
         |        text-align: right;
         |      }
         |    </style>
         |</head>
         |<body>
         |    <table>
         |      <thead>
         |        <tr dir="rtl">
         |          <th>תאריך</th>
         |        </tr>
         |      </thead>
         |      <tbody>
         |        ${generateTableRows()}
         |      </tbody>
         |    </table>
         |</body>
         |</html>""".stripMargin
    val outputPath = Paths.get("output.html")
    Files.write(outputPath, html.getBytes(StandardCharsets.UTF_8))
    println(s"HTML file saved to ${outputPath.toAbsolutePath}")
    println("You can open this file in your browser to see the Hebrew numerals")
  }
}
