import Grids.initGrid
import Model.Grid
import Model.Square
import Display.display
import Grids.solveGrid

object Main extends App {
  val grid = initGrid.init
  display.renderStartScreen

  play(grid)

  def play(grid: Grid): Grid =  {
    display.renderGrid(grid)

    if(grid.isCompleted()) {
      //If grid is completed
      display.endGame()
      val newGameGrid = initGrid.init
      play(grid)

    } else {
      //else ask square changes
      //val newGrid = display.askInputs(grid)
      val newGrid = solveGrid.solve(grid)

      //Process changes
      val checkedGrid = newGrid.checkValues()

      //Update possible values for each squares
      val processedGrid = checkedGrid.filPossibleValueSquares()


      //loop new turn
      play(processedGrid)
    }



  }

}
