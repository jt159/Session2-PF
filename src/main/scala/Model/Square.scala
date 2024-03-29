package Model

/**
  *
  * @param x The x coordonate of the square
  * @param y The y coordonate of the square
  * @param value The value of the square (between 1 and 9)
  * @param status Either 0 if the value is valid 1 neither
  * @param possibleValues An array of possible values for this Square
  */
case class Square (x: Int,
              y: Int,
              value: Int,
              status: Int,
              possibleValues: List[Int]) {

  /**
    *
    * @param square a square to check
    * @return true if the to square are at the same position false either
    */
  def equals(square : Square): Boolean = {
    square.x == this.x && square.y == this.y
  }


}
