package com.petecapra.sudoku

case class Cell(index: Int, candidates: Set[Int]) {

  def solved: Boolean = candidates.size == 1

  def number: Int = candidates.head

  override def toString = if (solved) number.toString else " "

}

case class SudokuMap(numbers: Vector[Cell]) {

  def removeCandidate(cell: Cell, candidate: Cell): SudokuMap = {
    if (cell.solved || !cell.candidates.contains(candidate.number))
      this
    else {
      val newCell: Cell = Cell(cell.index, cell.candidates - candidate.number)
      if (cell.candidates.size == 2)
        SudokuMap(numbers.updated(cell.index, newCell)).removeFromNeighbours(newCell)
      else
        SudokuMap(numbers.updated(cell.index, newCell))
    }
  }


  def removeFromNeighbours(cell: Cell): SudokuMap = {
    val neighbours: Set[Cell] =
      numbersByCol(numbersByCol.indexWhere(_.contains(cell))).filter(c => c.index != cell.index && !c.solved).toSet ++
        numbersByBox(numbersByBox.indexWhere(_.contains(cell))).filter(c => c.index != cell.index && !c.solved) ++
        numbersByBox(numbersByBox.indexWhere(_.contains(cell))).filter(c => c.index != cell.index && !c.solved)
    println("**** UNSOLVED NEIGHBOURS FOR CELL: " + cell.index + " ****")
    neighbours foreach ( neighbour => print(neighbour.index + ", "))
    println("****************************************************")
    neighbours.foldLeft(this)((current: SudokuMap, neighbour: Cell) => current.removeCandidate(neighbour, cell))
  }

  /**
   * Helper functions to provide appropriate vectors containing rows, columns and boxes
   */
  private def numbersByCol: Vector[Vector[Cell]] = numbers.groupBy(_.index % 9).values.toVector

  private def numbersByBox: Vector[Vector[Cell]] =
    numbers.groupBy(cell => ((cell.index / 27) * 3) + (cell.index % 9 / 3)).values.toVector

  private def numbersByRow: Vector[Vector[Cell]] = numbers.grouped(9).toVector

  def solved: Boolean = {
    def containsAllNumbers: (Vector[Cell] => Boolean) = {
      numbers => 1 to 9 forall (i => numbers.exists(cell => cell.solved && cell.number == 1))
    }
    numbersByRow.forall(containsAllNumbers) &&
      numbersByCol.forall(containsAllNumbers) &&
      numbersByBox.forall(containsAllNumbers)
  }

  def solveByElimination: SudokuMap = {
    val solvedCells = numbers.filter(_.solved)
//    println("**** SOLVED CELLS ****")
//    solvedCells foreach print
//    println("**********************")
    solvedCells.foldLeft(this)((current: SudokuMap, solvedCell: Cell) => current.removeFromNeighbours(solvedCell))
  }

  /**
   * print out the numbers in the table in a nice, neat format that's easy to read
   **/
  override def toString: String = {
    (for {
      index <- 0 to 80
    } yield {
      index match {
        case i if (i % 27 == 0) => "-------------------------\n| " + numbers(index)
        case 80 => " " + numbers(index) + " |\n-------------------------"
        case i if (i % 9 == 8) => " " + numbers(index) + " |\n"
        case i if (i % 3 == 2) => " " + numbers(index) + " |"
        case i if (i % 9 == 0) => "| " + numbers(index)
        case _ => " " + numbers(index)
      }
    }).mkString
  }

}

object SudokuMap {

  def apply(s: String): SudokuMap = {
    val numberString = s.replaceAll("\n", "").replaceAll("-", " ")
    val numbers = (for {
      i <- 0 to 80
    } yield {
      val number = numberString(i)
      if (number == ' ') Cell(i, Set(1, 2, 3, 4, 5, 6, 7, 8, 9)) else Cell(i, Set(number.getNumericValue))
    }).toVector
    SudokuMap(numbers)
  }

}


