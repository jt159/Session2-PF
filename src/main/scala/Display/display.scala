package Display

import Model.Grid
import Model.Square

object display {

  def renderStartScreen = {
    println("===============================")
    println("WELCOME TO A NEW SUDOKU GAME ! ")
    println("===============================")
    println("###############################")
  }

  def renderMenu (): Int = {
    println("===============================")
    println("=========== New game ==========")
    println("")
    println("Choose the difficulty of the grid to solve for this new game : ")
    println("- 1 (easy)")
    println("- 2 (medium)")
    println("- 3 (hard)")
    println("###############################")
    val value = scala.io.StdIn.readLine("Your choice : ").toInt
    if (value == 1 || value == 2 || value == 3) {
      value
    }else{
      print("\u001B[31m" + "Wrong choice ! Try again ! " + "\u001B[0m")
      renderMenu()
    }

  }

  def renderPlayingMenu (): Int = {
    println("===============================")
    println("Type of turn : ")
    println("- 1 (fill up the grid)")
    println("- 2 (let the AI play 1 turn)")
    println("- 3 (let the AI resolve all the grid)")
    println("###############################")
    val value = scala.io.StdIn.readLine("Your choice : ").toInt
    if (value == 1 || value == 2 || value == 3) {
      value
    }else{
      print("\u001B[31m" + "Wrong choice ! Try again ! " + "\u001B[0m")
      renderPlayingMenu()
    }

  }


  def renderGrid(grid: Grid) =  {
    println("GRID : ")
    println(" y ||x: 0 |  1  |  2  ||  3  |  4  |  5  ||  6  |  7  |  8  ")
    println("============================================================")
    var lineValue = 0;
    grid.squares.foldLeft(Array[Square]())((acc, square) => {
      if(acc.length % 9 == 0 && acc.length>0)
      {
        if(acc.length % 27 == 0)
          {
            println()
            println("============================================================")
          }
        else {
          println()
          println("------------------------------------------------------------")
        }

      }

      if(acc.length % 9 == 0) {
        print(" " + Math.round(acc.length / 9) + " ||" )
      }
      square.status match {
        case 4 => {
          print("  " + square.value  + "  ")
        }
        case 3 => {
          print("  " + "\u001B[32m" + square.value + "\u001B[0m" + "  ")
        }
        case 2 => {
          print("     ")

        }
        case 1 => {
          print("  " + "\u001B[31m" + square.value + "\u001B[0m" + "  ")
        }
        case 0 => {
          print("  " + "\u001B[34m" + square.value + "\u001B[0m" + "  ")
        }
      }

      if(acc.length % 3 == 2 && acc.length>0 && acc.length % 9 != 8)
        {
          print("||")
        }else if(acc.length % 9 != 8){
        print("|")
      }

      lineValue = lineValue + 1
      acc :+ square
    })
    println()
  }

  def askInputs(grid: Grid): Grid =  {
    println("Which square do you want to update ? ")
    val x = scala.io.StdIn.readLine("Enter x : ")
    val y = scala.io.StdIn.readLine("Enter y : ")
    val value = scala.io.StdIn.readLine("New value : ")
    //TODO check x and y value are wrong or not


    //save new value
    val gridCopy = Grid(grid.updateSquare(Square(x.toInt, y.toInt, 0,0,List()), value.toInt), grid.isValid)

    //Check if the user wants to change another square
    val answer = scala.io.StdIn.readLine("Do you need to update another square ? (y or n) : ")
    if(answer == 'y')
      askInputs(gridCopy)

    //User has finished his update
    renderGrid(gridCopy: Grid)
    return gridCopy
  }

  def endGame() = {
    println()
    println("\u001B[32m" + "============================================")
    println("===============" + " GRID SOLVED " + "================")
    println("============================================" + "\u001B[0m")


  }

}
