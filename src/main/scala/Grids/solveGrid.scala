package Grids

import Display.display
import Model.Grid
import Model.Square

object solveGrid {

  def solve(grid : Grid): Grid = {

    val smallestSolutionsSquare = getSmallestSolutionTable(grid)
    println(smallestSolutionsSquare)
    if(smallestSolutionsSquare == Square(0,0,0,0,List(1,2,3,4,5,6,7,8,9))) grid
    else {
      Grid(grid.updateSquare(Square(smallestSolutionsSquare.x, smallestSolutionsSquare.y, 0,0,List()), smallestSolutionsSquare.possibleValues.head), grid.isValid)
    }

    //check all possible value tables to find the square with the smallest answer set

  }

  def getSmallestSolutionTable(grid : Grid): Square = {
    var minusSolutionsSquare = Square(0,0,0,0,List(1,2,3,4,5,6,7,8,9))
    grid.squares.foldLeft(List[Square]())((acc, square) => {
      if(square.possibleValues.length < minusSolutionsSquare.possibleValues.length && !square.possibleValues.isEmpty  && square.status < 3)
        {
          minusSolutionsSquare = square
        }
      acc
    })
    return minusSolutionsSquare
  }
}
