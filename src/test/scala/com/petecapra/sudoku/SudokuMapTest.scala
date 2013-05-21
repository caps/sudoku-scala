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
        Cell(0, Set(1)), Cell(1, Set(2)), Cell(2, Set(3)), Cell(3, Set(4)), Cell(4, Set(5)), Cell(5, Set(6)), 
        Cell(6, Set(7)), Cell(7, Set(8)), Cell(8, Set(9)), Cell(9, Set(1)), Cell(10, Set(2)), Cell(11, Set(3)),
        Cell(12, Set(4)), Cell(13, Set(5)), Cell(14, Set(6)), Cell(15, Set(7)), Cell(16, Set(8)),
        Cell(17, Set(9)), Cell(18, Set(1)), Cell(19, Set(2)), Cell(20, Set(3)), Cell(21, Set(4)), 
        Cell(22, Set(5)), Cell(23, Set(6)), Cell(24, Set(7)), Cell(25, Set(8)), Cell(26, Set(9)),
        Cell(27, Set(1)), Cell(28, Set(2)), Cell(29, Set(3)), Cell(30, Set(4)), Cell(31, Set(5)), 
        Cell(32, Set(6)), Cell(33, Set(7)), Cell(34, Set(8)), Cell(35, Set(9)), Cell(36, Set(2)),
        Cell(37, Set(2)), Cell(38, Set(3)), Cell(39, Set(4)), Cell(40, Set(5)), Cell(41, Set(1)),
        Cell(42, Set(7)), Cell(43, Set(8)), Cell(44, Set(9)), Cell(45, Set(1)), Cell(46, Set(2)),
        Cell(47, Set(3)), Cell(48, Set(4)), Cell(49, Set(5)), Cell(50, Set(6)), Cell(51, Set(7)),
        Cell(52, Set(8)), Cell(53, Set(9)), Cell(54, Set(1)), Cell(55, Set(2)), Cell(56, Set(3)), 
        Cell(57, Set(1,2,3,4,5,6,7,8,9)), Cell(58, Set(5)), Cell(59, Set(6)), Cell(60, Set(7)),
        Cell(61, Set(8)), Cell(62, Set(9)), Cell(63, Set(1)), Cell(64, Set(2)), Cell(65, Set(3)), 
        Cell(66, Set(4)), Cell(67, Set(5)), Cell(68, Set(6)), Cell(69, Set(7)), Cell(70, Set(8)), 
        Cell(71, Set(9)), Cell(72, Set(1)), Cell(73, Set(2)), Cell(74, Set(3)), Cell(75, Set(4)), 
        Cell(76, Set(5)), Cell(77, Set(6)), Cell(78, Set(7)), Cell(79, Set(8)), Cell(80, Set(9))))

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

    println(toString)

    assert(toString ===
     """|-------------------------
        || 1 2 3 | 4 5 6 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        |-------------------------
        || 1 2 3 | 4 5 6 | 7 8 9 |
        || 2 2 3 | 4 5 1 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        |-------------------------
        || 1 2 3 |   5 6 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        || 1 2 3 | 4 5 6 | 7 8 9 |
        |-------------------------""".stripMargin)

  }

  test("map solved") {

    val numbers = 
      """145327698
        |839654127
        |672918543
        |496185372
        |218473956
        |753296481
        |367542819
        |984761235
        |521839764""".stripMargin

    assert( SudokuMap(numbers).solved === true )

  }

}
