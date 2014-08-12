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

  val gliderGUn = buildBoard(List(
    (1,5),(1,4),
    (2,5),(2,4),
    (11,6),(11,5),(11,4),
    (12,7),(12,3),
    (13,8),(13,2),
    (14,8),(14,2),
    (15,5),
    (16,7),(16,3),
    (17,6),(17,5),(17,4),
    (18,5),
    (21,4),(21,3),(21,2),
    (22,4),(22,3),(22,2),
    (23,5),(23,1),
    (25,6),(25,5),(25,1),(25,0),
    (35,3),(35,2),
    (36,3),(36,2)
  ))
}
