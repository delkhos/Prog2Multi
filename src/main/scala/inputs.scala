package rogue

import java.awt.event.KeyEvent
import org.json._
import java.io._ ;
import swing.event._
import java.awt.event.KeyEvent

object INPUTS {
  def input ( writer: PrintWriter, e: Event, vars: MainLoopVars, renderer: Renderer)
  {
    val msg = new JSONObject()
    e match {
      case KeyPressed(_,k,_,_) =>
        k match {
          case Key.Z => {
            msg.put("type","movement")
            msg.put("direction","UP")
            writer.println(msg.toString())
          }
          case Key.S => {
            msg.put("type","movement")
            msg.put("direction","WAIT")
            writer.println(msg.toString())
          }
          case Key.X => {
            msg.put("type","movement")
            msg.put("direction","DOWN")
            writer.println(msg.toString())
          }
          case Key.Q => {
            msg.put("type","movement")
            msg.put("direction","LEFT")
            writer.println(msg.toString())
          }
          case Key.D => {
            msg.put("type","movement")
            msg.put("direction","RIGHT")
            writer.println(msg.toString())
          }
          case Key.A => {
            msg.put("type","movement")
            msg.put("direction","UPLEFT")
            writer.println(msg.toString())
          }
          case Key.E => {
            msg.put("type","movement")
            msg.put("direction","UPRIGHT")
            writer.println(msg.toString())
          }
          case Key.W => {
            msg.put("type","movement")
            msg.put("direction","DOWNLEFT")
            writer.println(msg.toString())
          }
          case Key.C => {
            msg.put("type","movement")
            msg.put("direction","DOWNRIGHT")
            writer.println(msg.toString())
          }
          case _ => {
          }

        }
      case MouseMoved(_,coord,_) => //detects where the mouse is, relative to the player, and executes controls similarly to Key.Up, in the direction ofthe mouse
      {
          var clicked_x = ((coord.x -vars.dpos.x)/(vars.current_size.toFloat)).toInt
          var clicked_y = ((coord.y -vars.dpos.y)/(vars.current_size.toFloat)).toInt
          var ui_clicked_x = (coord.x/(renderer.tileset_handler.getSize().toFloat)).toInt
          var ui_clicked_y = (coord.y/(renderer.tileset_handler.getSize().toFloat)).toInt
          vars.mousepos = new Position(clicked_x,clicked_y)
          vars.mousepos_absolute = new Position(ui_clicked_x,ui_clicked_y)
      }
      case _ => {
      }
    }
  }
}
