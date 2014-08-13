//  Graphics is forked from github.com/petrifast/scala-snippets/scala-buffered-animation-sample

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import java.io.{FileInputStream, ObjectInputStream, ObjectOutputStream, FileOutputStream}
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Panel
import scala.swing.event._
import scala.swing._
import javax.swing.Timer

import java.awt.image._
import Constants._

object Mode extends Enumeration {
  type Mode = Value
  val Simulation, Drawing = Value
}

object GameApp extends SimpleSwingApplication {
  import Mode._

  var xOffset: Int = 0
  var yOffset: Int = 0

  var delayMs    = 20 // NOTE: delay between frames
  var framecount = 0
  var fps        = 0
  var image: BufferedImage = null

  var kernel: Kernel       = null
  var convolve: ConvolveOp = null

  var lifeBoard: LifeGame.Board = null

  var plotScale: Int = 7

  var isSimStarted: Boolean = false
  var mode: Mode            = Drawing


  def top = new MainFrame {
    title    = "YetAnotherScalaGameOfLife"
    menuBar  = menu
    contents = panel
  }

  def initializeLife() {
    lifeBoard = BoardConfiguration.gliderGun.clone()
  }

  def initializeConvolveKernel() {
    val nkernel = 11
    val kernel_desc: Array[Float] = Array.fill(nkernel*nkernel)(1.0f/(nkernel*nkernel))
    kernel = new Kernel(nkernel, nkernel, kernel_desc)
    convolve = new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null)
  }

  //..Timers: These generate the events that drive the animation
  val repainter = new Timer(delayMs, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      panel.repaint()

      mode match {
        case Drawing    => lifeBoard = LifeGame.computePaintedCells(lifeBoard)
        case Simulation => lifeBoard = LifeGame.computeNextGeneration(lifeBoard)
      }
    }
  })

  val framerateChecker = new Timer(1000, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      fps = framecount
      framecount = 0
    }
  })

  //..constructor
  initializeLife()
  initializeConvolveKernel()
  repainter.start()
  framerateChecker.start()

  val menu = new MenuBar {
    def write(o: LifeGame.Board) {
        val output = new ObjectOutputStream(new FileOutputStream(saveFilename))
        output.writeObject(o)
        output.close()
      }

    def read() = {
      try {
        val input = new ObjectInputStream(new FileInputStream(saveFilename))
        val obj = input.readObject()
        input.close()
        obj
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }

    contents += new Menu("Commands") {
      contents += new MenuItem(Action("Save State"){
        write(lifeBoard)
      })
      contents += new MenuItem(Action("Load State") {
        lifeBoard = try { read().asInstanceOf[LifeGame.Board] }
      })
      contents += new Separator
      contents += new MenuItem(Action("Reset Board Offset") {
        xOffset = 0
        yOffset = 0
        panel.repaint()
      })
      contents += new Separator
      contents += new MenuItem(Action("Reset Board") {
        lifeBoard = new LifeGame.Board()
        panel.repaint()
        initializeLife()
      })
      contents += new Separator
      contents += new MenuItem(Action("Clear Board") {
        lifeBoard = new LifeGame.Board()
        panel.repaint()
      })
      contents += new MenuItem(Action("Reset To Rpentomino") {
        lifeBoard = new LifeGame.Board()
        panel.repaint()
        lifeBoard = BoardConfiguration.rpentomino.clone()
      })
      contents += new MenuItem(Action("Reset To GliderGun") {
        lifeBoard = new LifeGame.Board()
        panel.repaint()
        lifeBoard = BoardConfiguration.gliderGun.clone()
      })
      contents += new MenuItem(Action("Reset To Random") {
        lifeBoard = new LifeGame.Board()
        panel.repaint()
        lifeBoard = BoardConfiguration.getRandom.clone()
      })
      contents += new Separator
      val a = new MenuItem(Action("zoom +") { if (plotScale + 1 < hiZoomLimit) plotScale += 1 })
      val b = new MenuItem(Action("zoom -") { if (plotScale - 1 > lowZoomLimit) plotScale -= 1})
      val mutex = new ButtonGroup(a,b)
      contents ++= mutex.buttons
    }
    contents += new Menu("Mode") {
      val a = new CheckMenuItem("Drawing")
      val b = new CheckMenuItem("Simulation")
      val mutex = new ButtonGroup(a,b)
      mutex.select(a)
      contents ++= mutex.buttons
      listenTo(a,b)
      reactions += { 
        case ButtonClicked(`a`) => {
          mode = Mode.Drawing
          isSimStarted = false
        }
      }
      reactions += {
        case ButtonClicked(`b`) => {
          mode = Mode.Simulation
          isSimStarted = true
        } 
      }
    }
  }

  // the animation panel class
  val panel = new Panel {

    preferredSize = new Dimension(640, 480)
    listenTo(mouse.clicks)
    listenTo(mouse.wheel)
    listenTo(keys)
    focusable = true
    requestFocus


    reactions += {
      case MousePressed(_, p, _, _, _) => mouseClick(p.x, p.y)
      case MouseWheelMoved(_, _, _, r) => wheelMoved(r)
      case KeyPressed(_, k, _, _)      => onKey(k)
    }

    private def onKey(k: Key.Value) = {
      k match {
        case Key.Down  => yOffset -= plotScale * offsetStep
        case Key.Up    => yOffset += plotScale * offsetStep
        case Key.Left  => xOffset += plotScale * offsetStep
        case Key.Right => xOffset -= plotScale * offsetStep
        case _ => {}
      }
    }

    private def mouseClick(x: Int, y: Int) {
      val w: Int = size.width
      val h: Int = size.height
      // user to screen
      val x2: Int = (x - xOffset) / plotScale
      val y2: Int = (y - yOffset) / plotScale
      LifeGame.addMouseClick(x2, y2)
    }

    private def wheelMoved(r: Int) = {
      if (plotScale + r > lowZoomLimit && plotScale + r < hiZoomLimit) {
        plotScale += r
        xOffset = 0
        yOffset = 0
        println(s"plot scale: , $plotScale")
      }
    }

    override def paintComponent(g: Graphics2D) {
      g.drawImage(image, 0, 0, null)
      drawLife()
      plotFPS(g)
      plotMode(g)
      framecount += 1
    }

    def drawGrid(data: Array[Int]) = {
      val w:Int = size.width
      val h:Int = size.height
      val color             = 0x25   // 8 bit color value
      val cellSizeInPixels  = 1 * plotScale
      val vCount            = (w / cellSizeInPixels) - 1
      val hCount            = (h / cellSizeInPixels) - 1

      for(xp <- 0 to vCount.toInt; yp <- 0 to hCount.toInt) {  // grid cells
        val x: Int = plotScale * xp
        val y: Int = plotScale * yp

        val index: Int = y*w + x
        if(index < data.length && index>= 0) {
          for (l <- 1 to plotScale) {  // grid x&y lines after zero point

            if ((x + l) < w && (x + l) > 0) {          // horizontal
              data(y*w + x + l) = color
            }
            if ((y + l) < h - 1 && (x + l) < w && (x + l) > 0) {          // vertical
              data((y+l)*w + x) = color
            }
          }
        }
      }
    }

    def drawLife() {
      val w:Int = size.width
      val h:Int = size.height

      // Check if our image is null or window has been resized, requiring new image
      if (null == image || image.getWidth() != w || image.getHeight() != h) {
        image = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY)
      }

      var data: Array[Int] = Array.fill(w * h)(0)
      if(plotScale > lowGridZoomLimit) drawGrid(data)

      for((xp,yp) <- lifeBoard) {
        for( i <- 0 until plotScale; j <- 0 until plotScale) {
          val x: Int =  (xOffset + xp*plotScale + i).toInt
          val y: Int =  (yOffset + yp*plotScale + j).toInt
          val index: Int = y*w + x
          if( index < data.length && index>= 0 && x < w && x > 0) {
            data( y*w + x) = Integer.MAX_VALUE
          }
        }
      }

      image.getRaster().setPixels(0, 0, w, h, data)
    }

    def plotFPS(g: Graphics2D) {
      showFps match {
        case false =>
          // add blur behind FPS
          val xblur = size.width  - 130
          val yblur = size.height - 32
          val bc = image.getSubimage(xblur, yblur, 115, 32)
          val bs = new BufferedImage(bc.getWidth(),
                                     bc.getHeight(),
                                     BufferedImage.TYPE_BYTE_GRAY)
          convolve.filter(bc, bs)
          g.drawImage(bs, xblur, yblur , null)
        case true =>
          // add FPS text; case fallthough is deliberate
          g.setColor(Color.RED)
          g.setFont(new Font("Monospaced", Font.BOLD, 20))
          g.drawString("FPS: " + fps, size.width - 120, size.height - 10);
      }
    }

    def plotMode(g: Graphics2D): Unit = {
      if (showMode) {
        g.setColor(Color.YELLOW)
        g.setFont(new Font("Monospaced", Font.BOLD, 20))
        g.drawString("Mode: " + mode.toString, 20, size.height - 10)
      }
    }


  } //end Panel 


}


