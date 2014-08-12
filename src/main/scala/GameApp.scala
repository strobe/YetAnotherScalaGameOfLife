//  Graphics is forked from github.com/petrifast/scala-snippets/scala-buffered-animation-sample

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import java.io.{FileInputStream, ObjectInputStream, ObjectOutputStream, FileOutputStream}
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Panel
import scala.swing.Rectangle
import scala.swing.event.{ MouseWheelMoved, MousePressed}
import scala.swing._
import javax.swing.Timer

import java.awt.image._


import Constants._

object GameApp extends SimpleSwingApplication {
  var delay_ms   = 1 // NOTE: delay between frames
  var framecount = 0
  var fps        = 0
  var image: BufferedImage = null

  var kernel: Kernel       = null
  var convolve: ConvolveOp = null

  var life_board: LifeGame.Board = null

  var plot_scale: Int = 7

  var isStarted: Boolean = false


  def top = new MainFrame {
    title = "YetAnotherScalaGameOfLife"
    menuBar = menu
    contents = panel
  }

  def initializeLife() {
    life_board = BoardConfiguration.gliderGUn.clone()
  }

  def initializeConvolveKernel() {
    val nkernel = 11
    val kernel_desc: Array[Float] = Array.fill(nkernel*nkernel)(1.0f/(nkernel*nkernel))
    kernel = new Kernel(nkernel, nkernel, kernel_desc)
    convolve = new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null)
  }

  //..Timers: These generate the events that drive the animation
  val repainter = new Timer(delay_ms, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      panel.repaint()
      if (isStarted) { life_board = LifeGame.computeNextGeneration(life_board) }
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
        val output = new ObjectOutputStream(new FileOutputStream("statefile.obj"))
        output.writeObject(o)
        output.close()
      }

    def read() = {
      try {
        val input = new ObjectInputStream(new FileInputStream("statefile.obj"))
        val obj = input.readObject()
        input.close()
        obj
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }

    contents += new Menu("Commands") {
      contents += new MenuItem(Action("Start/Resume") {
        isStarted = true
      })
      contents += new MenuItem(Action("Pause") {
        isStarted = false
      })
      contents += new Separator
      contents += new MenuItem(Action("Save State"){
        write(life_board)
      })
      contents += new MenuItem(Action("Load State") {
        life_board = try { read().asInstanceOf[LifeGame.Board] }
      })

      contents += new MenuItem(Action("Reset Board") {
        life_board = new LifeGame.Board()
        panel.repaint()
        initializeLife()
      })
      contents += new Separator
      val a = new MenuItem(Action("zoom: 2") { plot_scale = 2 })
      val b = new MenuItem(Action("zoom: 5") { plot_scale = 5 })
      val c = new MenuItem(Action("zoom: 7") { plot_scale = 7 })
      val mutex = new ButtonGroup(a,b,c)
      contents ++= mutex.buttons
    }
  }

  // the animation panel class
  val panel = new Panel {

    preferredSize = new Dimension(640, 480)
    listenTo(mouse.clicks)
    listenTo(mouse.wheel)


    reactions += {
      case MousePressed(_, p, _, _, _) => mouseClick(p.x, p.y)
      case MouseWheelMoved(_, _, _, r) => wheelMoved(r)
    }

    private def mouseClick(x: Int, y: Int) {
      val w:Int = size.width
      val h:Int = size.height
      // user to screen
      val x2: Int = (x - (x_offset * w).toInt) / plot_scale
      val y2: Int = (y - (y_offset * h).toInt) / plot_scale
      LifeGame.addMouseClick(x2, y2)
    }

    private def wheelMoved(r: Int) = {
      val hiZoomLimit = 8
      val lowZoomLimit = 0
      if (plot_scale + r > lowZoomLimit && plot_scale + r < hiZoomLimit) {
        plot_scale += r
      }
    }

    override def paintComponent(g: Graphics2D) {
      g.drawImage(image, 0, 0, null)
//      plotRasterGrid(g)
      drawLife()
      //plotGrid(g)
      plotFPS(g)
      framecount += 1
    }


//    def plotGrid(g: Graphics2D) {
//      val w:Int = size.width
//      val h:Int = size.height
//
//      val sizeInPixels  = 1*plot_scale
//      val vCount = w / sizeInPixels
//      val hCount = h / sizeInPixels
//
//
//      g.setColor(Color.darkGray)
//
//      for(i <- 0 to vCount.toInt; j <- 0 to hCount.toInt)
//      {
//        val grid: Rectangle = new Rectangle((x_offset*w).toInt + i*plot_scale * sizeInPixels/plot_scale,
//                                            (x_offset*w).toInt + j*plot_scale * sizeInPixels/plot_scale,
//                                             sizeInPixels, sizeInPixels)
//        g.draw(grid)
//      }
//    }

    def drawGrid(data: Array[Int]) = {
      val w:Int = size.width
      val h:Int = size.height
      val color             = 0x25   // 8 bit color value
      val cellSizeInPixels  = 1 * plot_scale
      val vCount            = (w / cellSizeInPixels) - 1
      val hCount            = (h / cellSizeInPixels) - 1

      for(i <- 0 to vCount.toInt; j <- 0 to hCount.toInt) {  // grid cells
        val x: Int = plot_scale * i
        val y: Int = plot_scale * j

        for (l <- 0 to plot_scale) {  // grid x&y lines after zero point
          if ((x + l) < w - 1) {      // horizontal
            data(y*w + x + l) = color
          }
          if ((y + l) < h - 1) {      // vertical
            data((y+l)*w + x) = color
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
      drawGrid(data)

      for( (xp,yp) <- life_board) {
        for( i <- 0 until plot_scale; j <- 0 until plot_scale) {
          val x: Int =  (x_offset*w).toInt + xp*plot_scale + i
          val y: Int =  (y_offset*h).toInt + yp*plot_scale + j
          val index: Int = y*w + x
          if( index < data.length && index>= 0) {
            data( y*w + x) = Integer.MAX_VALUE
          }
        }
      }

      image.getRaster().setPixels(0, 0, w, h, data)
    }

    def plotFPS(g: Graphics2D) {
      show_fps match {
        case false =>
          // add blur behind FPS
          val xblur = size.width - 130
          val yblur = size.height - 32;
          val bc = image.getSubimage(xblur, yblur, 115, 32);
          val bs = new BufferedImage(bc.getWidth(), bc.getHeight(),
            BufferedImage.TYPE_BYTE_GRAY);
          convolve.filter(bc, bs);
          g.drawImage(bs, xblur, yblur , null);
        case true =>
          // add FPS text; case fallthough is deliberate
          g.setColor(Color.RED);
          g.setFont(new Font("Monospaced", Font.BOLD, 20));
          g.drawString("FPS: " + fps, size.width - 120, size.height - 10);
      }
    }


  } //end Panel 


}


