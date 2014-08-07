//  Graphics is forked from github.com/petrifast/scala-snippets/scala-buffered-animation-sample

import java.awt.event.{ActionEvent, ActionListener}
import java.io.{FileInputStream, ObjectInputStream, ObjectOutputStream, FileOutputStream}
import scala.swing.event.{ MouseWheelMoved, MousePressed}
import scala.swing._
import javax.swing.Timer
import java.awt.{Font, Color, Graphics2D, Dimension}

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

  var plot_scale: Int = 1

  var isStarted: Boolean = false


  def top = new MainFrame {
    title = "YetAnotherScalaGameOfLife"
    menuBar = menu
    contents = panel
  }

  def initializeLife() {
    life_board = BoardConfiguration.bigBoard.clone()
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
      })
      contents += new Separator
      val a = new MenuItem(Action("zoom: 1") { plot_scale = 1 })
      val b = new MenuItem(Action("zoom: 2") { plot_scale = 2 })
      val c = new MenuItem(Action("zoom: 3") { plot_scale = 3 })
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
      val zoomLimit = 8
      if (plot_scale + r > 0 && plot_scale + r < zoomLimit) plot_scale += r
    }

    override def paintComponent(g: Graphics2D) {
      drawLife()
      g.drawImage(image, 0, 0, null);
      plotFPS(g)
      framecount += 1
    }

    def drawLife() {
      val w:Int = size.width
      val h:Int = size.height

      // Check if our image is null or window has been resized, requiring new image
      if (null == image || image.getWidth() != w || image.getHeight() != h) {
        image = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY)
      }

      var data: Array[Int] = Array.fill(w * h)(0);
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
      image.getRaster().setPixels(0, 0, w, h, data);
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


