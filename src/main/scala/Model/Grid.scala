package Model
import Model.Square
/**
  * this class represent the grid of a sudoku game including it's 9 Regions
  * @param squares an array with the nine regions
  * @param isValid true if all the values are valid on the grid, false either
  */
case class Grid (squares: List[Square],
            isValid: Boolean){

  def getSquare( x:Int, y:Int): Square = {
    squares.find(p => p.x == x && p.y == y).get
  }

  def updateSquare(square: Square, newValue: Int): List[Square] = {

    def updateSqr(list: List[Square], square: Square, newValue: Int): List[Square] = {
      if (list.isEmpty) Nil
      else if (list.head.equals(square))
        //update the changed square and put it's status at -2
        list.head.copy(status = 0, value = newValue) :: updateSqr(list.tail, square, newValue)
      else list.head :: updateSqr(list.tail, square, newValue)
    }

    updateSqr(squares, square, newValue)
  }

  def checkValues(): Grid = {
    val newSquares = this.squares.foldLeft(List[Square]())((acc, square) => {

      if(square.status < 2) {
        // if status != 2 then it's a user answer square so process the verification
        if(square.x == 6 && square.y == 0) {
          println(checkLine(this, square))
          println(checkColumn(this, square))
          println(checkSquare(this, square))
        }
        var resultStatus = 1
        if(checkLine(this, square) && checkColumn(this, square) && checkSquare(this, square)) {
          resultStatus = 3
        }
        acc :+ new Square(Math.round(acc.length/9),
          acc.length % 9,
          square.value,
          resultStatus,
          square.possibleValues )
      } else {
        // if status == 2 then it's a default square so no process
        acc :+ square
      }


    })
    return Grid(newSquares, this.isValid)
  }

  def checkLine(grid: Grid, square: Square): Boolean = {
    grid.squares.find(s => s.y == square.y &&  s.x != square.x && s.value == square.value)match {
      case Some(square) => false
      case None => true
    }
  }

  def checkColumn(grid: Grid, square: Square): Boolean = {
    grid.squares.find(s => s.x == square.x && s.y != square.y && s.value == square.value)match {
      case Some(square) => false
      case None => true
    }
  }

  def checkSquare(grid: Grid, square: Square): Boolean = {
    grid.squares.find(s => Math.round(s.x / 3) == Math.round(square.x / 3)
      && Math.round(s.y / 3) == Math.round(square.y / 3)
      && (s.x != square.x && s.y != square.y)
      && s.value == square.value)match {
      case Some(square) => false
      case None => true
    }
  }

  def filPossibleValueSquares(): Grid = {
    // Ading all possible value to the squares ufilled
    val newSquares = this.squares.foldLeft(List[Square]())((acc, square) => {
      var possibleValues = List[Int]()
      if(square.status < 3) {
        // if status < 3 then it's a user answer square or default empty value so process the array of possibilites fill up
        for(i <- 1 to 9) {
          val testSquare = Square(square.x, square.y, i,0,List())
          if(testSquare.x == 6 && testSquare.y == 0) {
            println("-----------------------------")
            println(i)
            println(checkLine(this, testSquare))
            println(checkColumn(this, testSquare))
            println(checkSquare(this, testSquare))
          }
          if(checkLine(this, testSquare) && checkColumn(this, testSquare) && checkSquare(this, testSquare)) {
            possibleValues = i :: possibleValues
          }
        }
        acc :+ Square(square.x,
          square.y,
          square.value,
          square.status,
          possibleValues )
      } else {
        // if status > 2 then it's a default square or validate square so no process
        acc :+ square
      }
    })
    return Grid(newSquares, this.isValid)
  }

  def isCompleted():Boolean = {
    var result  = true
    this.squares.foldLeft(List[Square]())((acc, square) => {
      if(square.status < 3) result = false
      acc
    })
    return result
  }


}