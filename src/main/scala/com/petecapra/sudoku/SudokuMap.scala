package com.petecapra.sudoku

case class Cell(index: Int, candidates: Set[Int]) {

  def solved: Boolean = candidates.size == 1

  def number: Int = candidates.head

  override def toString = if (solved) number.toString else " "

}

case class SudokuMap(numbers: Vector[Cell]) {

  def solveCell(cell: Cell, value: Int): SudokuMap = {
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
    val cell: Cell = numbers(cellIndex)
    val newCell: Cell = Cell(cellIndex, cell.candidates - candidate.number)
    val updatedMap: SudokuMap = SudokuMap(numbers.updated(cell.index, newCell))
    println(depthSpacing(depth) + "Removed candidate: " + candidate.number + " from cell with index: " + cell.index + " remaining candidates: " + newCell.candidates)
    if (newCell.solved) {
      println(depthSpacing(depth) + "Just solved cell with index: " + newCell.index + " and value: " + newCell.number)
      updatedMap.removeCandidateFromNeighbours(newCell, depth + 1)
    } else
      updatedMap
  }

  private def removeCandidateFromNeighbours(cell: Cell, depth: Int): SudokuMap = {
    val neighbours: Set[Cell] = getNeighbours(cell).filter { c =>
      c.index != cell.index && !c.solved && c.candidates.contains(cell.number)
    }
    print(depthSpacing(depth) + "**** UNSOLVED NEIGHBOURS FOR CELL: " + cell.index + " => ")
    neighbours foreach (neighbour => print(neighbour.index + ", "))
    print("****")
    println("")
    neighbours.foldLeft(this)((current: SudokuMap, neighbour: Cell) => {
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

    // for each unsolved cell get all unsolved neighbours
    val unsolvedNeighbours: Set[Cell] = for {
      unsolvedCell:Cell <- unsolvedCells.toSet
      unsolvedNeighbour <- getNeighbours(unsolvedCell).filterNot(_.solved)
    } yield unsolvedNeighbour

    // TODO: UGGGGLLYYY! AND WRONG!
    // for cell and all neighbours group by candidates filter to groups where only one cell for candidate
    val candidateGroups:Set[(Int, Cell)] = (for {
      number <- 1 to 9
      unsolvedNeighbour <- unsolvedNeighbours
      if (unsolvedNeighbour.candidates.contains(number))
    } yield (number, unsolvedNeighbour)).groupBy(_._1).filter(_._1 == 1).head._2.toSet

    // solve those cells
    candidateGroups.foldLeft(this)((current, cellPair) => current.solveCell(cellPair._2, cellPair._1))

  }

  def depthSpacing(depth: Int): String = depth + "." + (for {i <- 0 to depth} yield "  ").mkString

  def unsolvedCells: Vector[Cell] = numbers.filterNot(_.solved)

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


