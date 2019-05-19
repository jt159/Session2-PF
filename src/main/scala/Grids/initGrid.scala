package Grids

import Model.Grid
import Model.Square

object initGrid {

  /**
    *
    * @return a new grid from the model
    */
  def init(difficultyValue: Int): Grid = {
    //Select a random grid in the array of diffculty selected value catalog of grids
    val selectedSchema = difficultyValue match {
      case 1 => {
        easyGridSchema{scala.util.Random.nextInt(easyGridSchema.length-1)}
      }
      case 2 => {
        mediumGridSchema{scala.util.Random.nextInt(mediumGridSchema.length-1)}
      }
      case 3 => {
        hardGridSchema{scala.util.Random.nextInt(hardGridSchema.length-1)}
      }
    }

    //Create a new square List from the selected schema
    val squares = selectedSchema.foldLeft(List[Square]())((acc, line) => {
          var squareStatus = if (line != 0) 4 else 2

          acc :+ new Square(acc.length % 9, Math.round(acc.length/9),line,squareStatus,List() )

    })

    new Grid(squares,false)
  }

  val easyGridSchema = Array(
    Array(
      0,9,1,0,2,4,0,0,6,
      0,0,4,0,0,3,0,0,7,
      0,0,0,0,5,7,2,0,0,
      9,1,0,0,7,0,6,0,0,
      5,0,8,1,0,6,3,0,9,
      0,0,7,0,9,0,0,5,2,
      0,0,5,7,6,0,0,0,0,
      8,0,0,4,0,0,9,0,0,
      6,0,0,5,8,0,7,0,0
    ),
    Array(
      0,0,0,0,7,6,0,4,0,
      7,0,0,0,0,3,2,0,1,
      0,0,5,4,2,0,3,8,0,
      0,0,9,3,0,7,8,5,0,
      0,0,0,0,8,0,0,0,0,
      0,3,6,5,0,4,1,0,0,
      0,5,3,0,6,9,7,0,0,
      1,0,2,7,0,0,0,0,9,
      0,7,0,2,3,0,0,0,0
    )
  )

  val mediumGridSchema = Array(
    Array(
      0,0,7,2,0,0,0,0,0,
      0,0,0,0,1,0,4,3,7,
      1,0,3,0,7,0,8,0,0,
      5,1,0,8,0,0,0,0,0,
      0,7,0,4,0,1,0,8,0,
      0,0,0,0,0,7,0,1,3,
      0,0,8,0,9,0,1,0,4,
      7,5,9,0,4,0,0,0,0,
      0,0,0,0,0,5,6,0,0
    ),
    Array(
      7,0,0,0,0,0,5,0,4,
      0,0,1,0,4,2,3,0,0,
      0,0,0,0,0,7,0,1,0,
      0,5,8,4,0,9,0,0,0,
      9,0,0,0,3,0,0,0,2,
      0,0,0,6,0,1,4,9,0,
      0,4,0,1,0,0,0,0,0,
      0,0,3,2,6,0,7,0,0,
      5,0,7,0,0,0,0,0,3

    )
  )

  val hardGridSchema = Array(
    Array(
      0,0,0,0,0,0,7,1,0,
      1,0,0,0,0,0,0,0,6,
      0,0,5,0,9,1,0,8,0,
      0,0,3,0,0,8,5,4,0,
      7,0,0,3,0,9,0,0,1,
      0,4,2,5,0,0,9,0,0,
      0,8,0,9,5,0,1,0,0,
      2,0,0,0,0,0,0,0,5,
      0,6,4,0,0,0,0,0,0
    ),
    Array(
      0,0,0,7,0,0,0,0,0,
      0,3,0,0,1,0,6,0,0,
      0,5,1,3,0,0,9,0,0,
      0,1,0,0,0,0,8,0,0,
      2,0,7,5,0,4,1,0,6,
      0,0,4,0,0,0,0,2,0,
      0,0,8,0,0,3,5,7,0,
      0,0,2,0,8,0,0,4,0,
      0,0,0,0,0,9,0,0,0
    )
  )


}
