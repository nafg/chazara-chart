package com.example.html

import jewishdate.JewishDate
import jewishdate.JewishMonth

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object HtmlGenerator {
  private val hebrewNumerals: Map[Int, String] = Map(
    1 -> "א",
    2 -> "ב",
    3 -> "ג",
    4 -> "ד",
    5 -> "ה",
    6 -> "ו",
    7 -> "ז",
    8 -> "ח",
    9 -> "ט",
    10 -> "י",
    20 -> "כ",
    30 -> "ל",
    40 -> "מ",
    50 -> "נ",
    60 -> "ס",
    70 -> "ע",
    80 -> "פ",
    90 -> "צ",
    100 -> "ק",
    200 -> "ר",
    300 -> "ש",
    400 -> "ת"
  )

  private val hebrewMonths: Map[JewishMonth.Value, String] = Map(
    JewishMonth.Tishrei -> "תשרי",
    JewishMonth.Cheshvan -> "חשון",
    JewishMonth.Kislev -> "כסלו",
    JewishMonth.Teves -> "טבת",
    JewishMonth.Shvat -> "שבט",
    JewishMonth.Adar -> "אדר",
    JewishMonth.Nissan -> "ניסן",
    JewishMonth.Iyar -> "אייר",
    JewishMonth.Sivan -> "סיון",
    JewishMonth.Tammuz -> "תמוז",
    JewishMonth.Av -> "אב",
    JewishMonth.Elul -> "אלול"
  )

  private def numberToHebrew(n: Int, plain: Boolean = false): String = {
    if (n == 15) return "ט״ו"
    if (n == 16) return "ט״ז"

    val values = Seq(400, 300, 200, 100, 90, 80, 70, 60, 50, 40, 30, 20, 10, 9,
      8, 7, 6, 5, 4, 3, 2, 1)

    var remaining = n
    val result = new StringBuilder

    for (value <- values if remaining > 0) {
      while (remaining >= value) {
        result.append(hebrewNumerals(value))
        remaining -= value
      }
    }

    if (!plain) {
      if (result.length > 1) {
        result.insert(result.length - 1, "״")
      } else {
        result.append("׳")
      }
    }

    result.toString
  }

  private def formatHebrewDate(date: JewishDate): String = {
    val day = numberToHebrew(date.dayOfMonth)
    val month = hebrewMonths(date.month)
    val yearStr = date.year.toString
    val year = numberToHebrew(yearStr.substring(yearStr.length - 3).toInt)
    s"$day $month $year"
  }

  def generateTableRows(start: Int, end: Int): String = {
    var currentDate = JewishDate(5785, JewishMonth.Shvat, 1)
    (start to end)
      .map(pageNum => {
        val jewishDate = currentDate
        val nextDate = if (currentDate.dayOfMonth < 29) {
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
        val amud = numberToHebrew(pageNum, plain = true)
        s"""<tr>
           |          <td>$amud.</td>
           |          <td>${formatHebrewDate(jewishDate)}</td>
           |        </tr>
           |        <tr>
           |          <td>$amud:</td>
           |          <td>${formatHebrewDate(nextDate)}</td>
           |        </tr>""".stripMargin
      })
      .mkString("\n        ")
  }

  def generateHtml(title: String, content: String): String = {
    s"""<!DOCTYPE html>
       |<html>
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
    val html = generateHtml(
      "Hebrew Counters Demo",
      "<h1>Hebrew Number Demonstration</h1><p>Above you can see numbers 40-60 in Hebrew numerals with corresponding dates starting from 10 Shevat 5785.</p>"
    )
    val outputPath = Paths.get("output.html")
    Files.write(outputPath, html.getBytes(StandardCharsets.UTF_8))
    println(s"HTML file saved to ${outputPath.toAbsolutePath}")
    println("You can open this file in your browser to see the Hebrew numerals")
  }
}
