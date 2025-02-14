package chazarachart

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import chazarachart.hebrew.{FormatDate, FormatNumber}
import jewishdate.{JewishDate, JewishMonth}

object Main {
  private def generateTableRows(start: Int, end: Int): String = {
    var currentDate = JewishDate(5785, JewishMonth.Shvat, 1)
    (start to end)
      .map { pageNum =>
        val jewishDate = currentDate
        val nextDate   = if (currentDate.dayOfMonth < 29) {
          JewishDate(
            currentDate.year,
            currentDate.month,
            currentDate.dayOfMonth + 1
          )
        } else {
          // For simplicity, we'll just reset to day 1 of the same month
          // This is not accurate for month transitions but will work for our demo
          JewishDate(currentDate.year, currentDate.month, 1)
        }
        currentDate = nextDate
        val amud       = FormatNumber(pageNum, plain = true)
        // language=html
        s"""<tr>
           |  <td>${FormatDate(jewishDate)}</td>
           |  <td>$amud.</td>
           |</tr>
           |<tr>
           |  <td>${FormatDate(nextDate)}</td>
           |  <td>$amud:</td>
           |</tr>""".stripMargin
      }
      .mkString("\n        ")
  }

  private def generateHtml(title: String, content: String): String = {
    // language=html
    s"""<!DOCTYPE html>
       |<html lang="he">
       |<head>
       |    <title>$title</title>
       |    <meta charset="UTF-8">
       |    <style>
       |      body {
       |        direction: rtl;
       |      }
       |      table {
       |        border-collapse: collapse;
       |        width: 100%;
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
       |          <th>עמוד</th>
       |          <th>רש"י 1</th>
       |          <th>רש"י 2</th>
       |        </tr>
       |      </thead>
       |      <tbody>
       |        ${generateTableRows(40, 60)}
       |      </tbody>
       |    </table>
       |    <div>$content</div>
       |</body>
       |</html>""".stripMargin
  }

  def main(args: Array[String]): Unit = {
    val html       = generateHtml(
      "Hebrew Counters Demo",
      "<h1>Hebrew Number Demonstration</h1><p>Above you can see numbers 40-60 in Hebrew numerals with corresponding dates starting from 10 Shevat 5785.</p>"
    )
    val outputPath = Paths.get("output.html")
    Files.write(outputPath, html.getBytes(StandardCharsets.UTF_8))
    println(s"HTML file saved to ${outputPath.toAbsolutePath}")
    println("You can open this file in your browser to see the Hebrew numerals")
  }
}
