import Grids.{initGrid, solveGrid}
import Model.{Grid, Square}
import org.scalatest.WordSpec

class SolverTests extends WordSpec {

  "A Game" when {
    "when AI only" should {
      "Should return solved Grid" in {
        val grid = initGrid.init(1)
        val solvedGrid = Main.play(grid, 3)

        assert(solvedGrid.isCompleted())
      }

    }
  }

  "A Game" when {
    "when process to possible value check" should {
      "Should return a square with minimal possible solution " in {
        val grid = initGrid.init(1)
        val processedGrid = grid.filPossibleValueSquares()

        assert(solveGrid.getSmallestSolutionTable(processedGrid) != null )
      }

    }
  }

}
