package com.petecapra.sudoku

case class Cell(index: Int, candidates: Set[Int]) {

  def solved: Boolean = candidates.size == 1

  def number: Int = candidates.head

  override def toString = if (solved) number.toString else " "

}

case class SudokuMap(numbers: Vector[Cell]) {

  private def solveCell(cell: Cell, value: Int): SudokuMap = {
    val newCell: Cell = Cell(cell.index, Set(value))
    SudokuMap(numbers.updated(cell.index, newCell)).removeCandidateFromNeighbours(newCell, 0)
  }

  /**
   * Helper functions to provide appropriate vectors containing rows, columns and boxes
   */
  private def numbersByCol: Vector[Vector[Cell]] = numbers.groupBy(_.index % 9).values.toVector
  private def numbersByBox: Vector[Vector[Cell]] =
    numbers.groupBy(cell => ((cell.index / 27) * 3) + (cell.index % 9 / 3)).values.toVector
  private def numbersByRow: Vector[Vector[Cell]] = numbers.grouped(9).toVector

  private def removeCandidateFromCell(cellIndex: Int, candidate: Cell, depth: Int): SudokuMap = {
    val newCell: Cell = Cell(cellIndex, numbers(cellIndex).candidates - candidate.number)
    val updatedMap: SudokuMap = SudokuMap(numbers.updated(cellIndex, newCell))
    if (newCell.solved)
      // TODO: use solveCell method here
      updatedMap.removeCandidateFromNeighbours(newCell, depth + 1)
    else
      updatedMap
  }

  private def removeCandidateFromNeighbours(cell: Cell, depth: Int): SudokuMap = {
    getNeighbours(cell).filter { c =>
      c.index != cell.index && !c.solved && c.candidates.contains(cell.number)
    }.foldLeft(this)((current: SudokuMap, neighbour: Cell) => {
      current.removeCandidateFromCell(neighbour.index, cell, depth)
    })
  }

  def solved: Boolean = {
    def containsAllNumbers: (Vector[Cell] => Boolean) = {
      numbers => 1 to 9 forall (i => numbers.exists(cell => cell.solved && cell.number == i))
    }
    numbersByRow.forall(containsAllNumbers) &&
      numbersByCol.forall(containsAllNumbers) &&
      numbersByBox.forall(containsAllNumbers)
  }

  def solveByCellElimination: SudokuMap = {
    numbers.filter(_.solved).foldLeft(this) {
      (current: SudokuMap, solvedCell: Cell) => current.removeCandidateFromNeighbours(solvedCell, 0)
    }
  }

  def getNeighbours(cell: Cell): Set[Cell] = {
    numbersByCol(numbersByCol.indexWhere(_.contains(cell))).toSet ++
      numbersByRow(numbersByRow.indexWhere(_.contains(cell))) ++
      numbersByBox(numbersByBox.indexWhere(_.contains(cell)))
  }

  def solveByGroupElimination: SudokuMap = {

    // for each group
    val groups: Vector[Vector[Cell]] = numbersByCol ++ numbersByRow ++ numbersByBox

    // filter out solved cells
    val unsolvedGroups: Vector[Vector[Cell]] = for {
      group <- groups
    } yield group.filterNot(_.solved)
    
    // group by candidate number
    val candidateGroups: Vector[(Int, Set[Cell])] = (for {
      number <- 1 to 9
      unsolvedGroup <- unsolvedGroups
    } yield (number, unsolvedGroup.filter(_.candidates.contains(number)).toSet)).toVector  

    // and filter to numbers with only one cell
    val candidateGroupsFilter: Vector[(Int, Set[Cell])] = candidateGroups.filter(_._2.size == 1)

    // solve those cells
    candidateGroupsFilter.foldLeft(this)((current, cellPair) => current.solveCell(cellPair._2.head, cellPair._1))

  }

  private def unsolvedCells: Vector[Cell] = numbers.filterNot(_.solved)

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


