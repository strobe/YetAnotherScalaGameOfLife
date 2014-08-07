import scala.util.Random

object BoardConfiguration {

  def buildBoard(b: List[(Int,Int)]) = {
    val s = new LifeGame.Board()
    for( p <- b ) {s.add(p)}
    s
  }

  var blinker      = buildBoard(List((0,-1),(0,0),(0,1)))
  var square       = buildBoard(List((0,0),(1,0),(1,1),(0,1)))
  var gosperGlider = buildBoard(List((0,0),(1,0),(1,1),(0,1)))
  var glider       = buildBoard(List((0,0),(1,1),(2,1),(1,2),(2,0)))
  var rpentomino   = buildBoard(List((1,0),(0,1),(1,1),(1,2),(2,2)))

  /**
    * random values inside 100x100 square
    */
  def getRandom = {
    val list = (for {
      x <- 0 to Random.nextInt(100)
      y <- 0 to Random.nextInt(100)
    } yield (x, y)).toList
    buildBoard(list)
  }

  /**
    * test for the 2^64 board size
    */
  val bigBoard     = buildBoard(List((0,0),
    (math.pow(2, 64).toInt,0),
    (math.pow(2, 64).toInt, math.pow(2, 64).toInt),
    (0, math.pow(2, 64).toInt)) ++ getRandom)
}
