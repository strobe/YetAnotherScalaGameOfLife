import scala.collection.mutable.HashSet


object LifeGame {

  type Board = HashSet[(Int,Int)]

  var mouseClicks: List[(Int,Int)] = List()

  /**
    * @note X
    *       ^
    *       |
    *       ----------------------
    *       |     |        |     |
    *       ----------------------
    *       |     | (x, y) |     |
    *       ----------------------
    *       |     |        |     |
    *       0-----------------------> Y
    */
  def neighbors(p: (Int,Int)): HashSet[(Int, Int)] = p match {
    case (x,y) =>
      for(q <- HashSet((x+1,y-1), (x+1,y), (x+1,y+1),
                       (x,y-1),            (x,y+1),
                       (x-1,y-1), (x-1,y), (x-1,y+1))
          ) yield q
  }


  /**
    * @note Called on the each timestep update:
    *       Update each cell according to GameOfLife rules:
    *       - Any live cell with 2 or 3 live neighbours lives on to the next generation
    *       - Any dead cell with exactly 3 live neighbours becomes a live cell
    *       - Otherwise cell will be dead
    */
  def computeNextGeneration(board: Board): Board = {
    val newBoard = new Board()
    val candidates = board flatMap neighbors  // making board with neighbors cells + old cells

    for (p <- candidates) {
      val count =  neighbors(p).count(board.contains(_))  // count of live neighbors cells
      // adding new live cells to the new board
      if(count == 3                              // live or dead cell with 3 neighbours
        || (count == 2 && board.contains(p))) {  // live cell with 2 neighbours
        newBoard.add(p)
      }
    }
    // adding mouse clicks cells
    for (i <- mouseClicks) {
      newBoard.add(i)
    }
    mouseClicks = List()

    newBoard
  }

  def computePaintedCells(board: Board): Board = {

    // adding mouse clicks cells
    for (i <- mouseClicks) {
      board.add(i)
    }
    board
  }

  def addMouseClick(x: Int, y: Int) = {
    mouseClicks = mouseClicks ++ List((x, y))
  }
}
