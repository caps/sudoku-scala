package com.petecapra.sudoku


case class SudokuMap(numbers: Vector[Vector[Int]]) {

  override def toString:String = {
    val horizontalDivider = "-------------------------\n"
    horizontalDivider
  }

}

object SudokuMap {

  def apply(s: String): SudokuMap = {
    SudokuMap(s.split("\n").map { row =>
      row.map { cell => cell match {
        case ' ' => 0
        case _ => cell.getNumericValue
      }
      }.toVector
    }.toVector)
  }

}


