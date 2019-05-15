package Grids

import Model.Grid
import Model.Square

object initGrid {

  val gridSchema = Array(
    0,9,1,0,2,4,0,0,6,
    0,0,4,0,0,3,0,0,7,
    0,0,0,0,5,7,2,0,0,
    9,1,0,0,7,0,6,0,0,
    5,0,8,1,0,6,3,0,9,
    0,0,7,0,9,0,0,5,2,
    0,0,5,7,6,0,0,0,0,
    8,0,0,4,0,0,9,0,0,
    6,0,0,5,8,0,7,0,0
  )

  /**
    *
    * @return a new grid from the model
    */
  def init: Grid = {
    val squares = gridSchema.foldLeft(List[Square]())((acc, line) => {
          var squareStatus = if (line != 0) 4 else 2

          acc :+ new Square(acc.length % 9, Math.round(acc.length/9),line,squareStatus,List() )

    })

    return new Grid(squares,false)
  }


}
