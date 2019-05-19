import Grids.initGrid
import Model.Grid
import Model.Square
import Display.display
import Grids.solveGrid

object Main extends App {
  display.renderStartScreen
  newGame()



  def newGame() = {
    val difficultyValue = display.renderMenu()
    val grid = initGrid.init(difficultyValue)

    play(grid,0)
  }


  def play(grid: Grid,oldTypeTurn:Int): Grid =  {
    if(grid.isCompleted()) {
      //If grid is completed
      display.renderGrid(grid)
      display.endGame()
      grid
    }else {
      if(oldTypeTurn<3) display.renderGrid(grid)

      var typeTurnValue = 0
      if(oldTypeTurn<3){
        typeTurnValue = display.renderPlayingMenu()
      } else {
        typeTurnValue = oldTypeTurn
      }

      //else ask square changes
      var newGrid: Grid = null;
      if(typeTurnValue > 1) {
        newGrid = solveGrid.solve(grid)
      } else {
        newGrid = display.askInputs(grid)
      }

      //Process changes
      val checkedGrid = newGrid.checkValues()

      //Update possible values for each squares
      val processedGrid = checkedGrid.filPossibleValueSquares()

      //loop new turn
      play(processedGrid, typeTurnValue)
    }



  }

}

