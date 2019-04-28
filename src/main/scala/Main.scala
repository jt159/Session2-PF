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
    //If grid finish
    //TODO display end
    scala.io.StdIn.readLine("Follow ? (enter) ")

    //else ask square changes
    //val newGrid = display.askInputs(grid)
    val newGrid = solveGrid.solve(grid)

    display.renderGrid(newGrid)
    //Process changes
    val checkedGrid = newGrid.checkValues()
    //Update possible values for each squares
    val processedGrid = checkedGrid.filPossibleValueSquares()


    //loop new turn
    play(processedGrid)

  }

}
