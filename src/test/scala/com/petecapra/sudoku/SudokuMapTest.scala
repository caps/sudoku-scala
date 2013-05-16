package com.petecapra.sudoku

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite


@RunWith(classOf[JUnitRunner])
class SudokuMapTest extends FunSuite {

  test("string to map") {

    val numbers =
      """123456789
        |123456789
        |123456789
        |123456789
        |223451789
        |123456789
        |123 56789
        |123456789
        |123456789""".stripMargin

    val sudokuMap = SudokuMap(numbers)

    assert(sudokuMap.numbers ===
      Vector(
        Vector(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Vector(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Vector(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Vector(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Vector(2, 2, 3, 4, 5, 1, 7, 8, 9),
        Vector(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Vector(1, 2, 3, 0, 5, 6, 7, 8, 9),
        Vector(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Vector(1, 2, 3, 4, 5, 6, 7, 8, 9)))

  }

  test("map toString") {

    val numbers =
      """123456789
        |123456789
        |123456789
        |123456789
        |223451789
        |123456789
        |123 56789
        |123456789
        |123456789""".stripMargin

    val toString = SudokuMap(numbers).toString

    assert(toString ===
      """-------------------------
        || 1 2 3 | 4 5 6 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        |-------------------------
        || 1 2 3 | 4 5 6 | 7 8 9 |
        || 2 2 3 | 4   6 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        |-------------------------
        || 1 2 3 | 4 5 6 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        |-------------------------""".stripMargin)

  }

}
