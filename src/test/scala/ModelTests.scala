import Grids.initGrid
import Model.{Grid, Square}
import org.scalatest.WordSpec


class ModelTests extends WordSpec {

  "A Grid" when {
    "when created" should {
      "Should return a non empty square List" in {
        val grid = initGrid.init
        assert(grid.squares.nonEmpty)
      }
      "Should have a 81 length square list" in {
        val grid = initGrid.init
        assert(grid.squares.length == 81)
      }
      "Should be unvalid" in {
        val grid = initGrid.init
        assert(!grid.isValid)
      }
    }

    "when searching a square" should {
      "Should return a unique square when the right x and y value" in {
        val grid = initGrid.init
        val square = grid.getSquare(1,2)
        assert(square.x == 1 && square.y == 2)
      }
    }

    "when updating a square" should {
      "Should return a unique square when the right x and y and the new value" in {
        val grid = initGrid.init
        val square = grid.getSquare(1,2)
        val oldValue = square.value
        val updatedGrid = Grid(grid.updateSquare(square, oldValue-1),grid.isValid)
        assert(updatedGrid.getSquare(1,2).value == oldValue-1 )
      }
    }
  }

  "A Square" when {
    "when created" should {
      "Should return a x value" in {
        val square = Square(1,1,0,-1,List() )
        assert(square.x == 1)
      }
      "Should return a y value" in {
        val square = Square(1,2,0,-1,List())
        assert(square.y == 2)
      }
      "Should return a value" in {
        val square = Square(1,2,8,-1,List())
        assert(square.value == 8)
      }
      "Should return a status" in {
        val square = Square(1,2,8,1,List())
        assert(square.status == 1)
      }
      "Should return a empty list of possibilities" in {
        val square = Square(1,2,0,-1,List())
        assert(square.possibleValues.isEmpty)
      }

    }

    "when 2 squares has same x and y values " should {
      "should be equals" in {
        val s1 = Square(1, 1, 0, 2, List())
        val s2 = Square(1, 1, 8, 1, List())
        assert(s1.equals(s2))
      }
    }
  }
}
