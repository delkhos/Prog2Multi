package rogue 

import swing._
import swing.event._
import java.awt.{Color,Graphics2D, Graphics}
import java.awt.image.BufferedImage                                           
import java.awt.event.KeyEvent
import java.net._ ;
import java.io._ ;
import org.json._

/*
 * This class extends the swing Panel.
 * It is the swing object in which all our game is handled
 * , from logic, to drawing.
 */

class GamePanelDuo(main_frame: MainFrame, socket: Socket ) extends Panel {
  val game_matrix_dim = new Dimension(50,30)
  //50 30

  val ui_dim = new Dimension(20, 12)

  val screen_matrix_dim = new Dimension(game_matrix_dim.width + ui_dim.width + 1,game_matrix_dim.height + ui_dim.height +1)

  val renderer = new Renderer()

  val vars = new MainLoopVars(game_matrix_dim, renderer.getTileSize())

  val input:InputStream = socket.getInputStream();
  val reader:BufferedReader = new BufferedReader(new InputStreamReader(input));
  val output:OutputStream = socket.getOutputStream();    
  val writer:PrintWriter = new PrintWriter(output, true); 

  var game:JSONObject = new JSONObject(reader.readLine());

  if(!renderer.initIsOk()){
    println("Couldn't load tileset, exiting!!.\n")
    System.exit(1)
  }
  val updater = new Updater()
  updater.start()
  val drawer = new Drawer(this)
  drawer.start()

  this.preferredSize = new java.awt.Dimension(screen_matrix_dim.width*renderer.getTileSize() , screen_matrix_dim.height*renderer.getTileSize())
  
  override def paintComponent(g : Graphics2D) {
    renderer.drawGameJSON(g, vars.current_size, screen_matrix_dim, ui_dim, game, vars.dpos, vars.mousepos, vars.mousepos_absolute)
  } 
  // LISTENING TO MOUSE AND KEYBOARD
  focusable = true
  listenTo(mouse.clicks)
  listenTo(mouse.moves)
  listenTo(mouse.wheel)
  listenTo(keys)
  //**************************************************************//
  //************* reactions corresponds to the MainLoop *************//
  //**************************************************************//
  this.reactions += {
    case e => {
      requestFocus()
      INPUTS.input(writer,e, vars, renderer)
    }
  }

  class Updater()  extends Thread {
    override def run()
    {
      try {
        var game_string = " "
        while(main_frame.visible && game_string != null){
          game_string = reader.readLine();
          game.synchronized{
            
            if (game_string != null){
              game = new JSONObject(game_string)
            }
          }
        }
      } catch {    
       case ex : IOException  => {    
          println("Server exception: " + ex.getMessage());    
          ex.printStackTrace();    
       }    
       case ex : Throwable => {     
          println("Unknown exception: " + ex.getMessage());    
          ex.printStackTrace();    
       }    
     }
     main_frame.dispose()
    }
  }
  class Drawer(panel: GamePanelDuo)  extends Thread 
  {
      override def run()
      {
        while(updater.isAlive()){
          Thread.sleep(34)
          panel.repaint()
        }
      }
  }
}
