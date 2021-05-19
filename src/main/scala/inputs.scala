package rogue

import java.awt.event.KeyEvent
import org.json._
import java.io._ ;
import swing.event._
import java.awt.event.KeyEvent

object INPUTS {
  val game_matrix_dim = new Dimension(50,30)
  //50 30

  val ui_dim = new Dimension(20, 12)

  val matrix_dim = new Dimension(game_matrix_dim.width + ui_dim.width + 1,game_matrix_dim.height + ui_dim.height +1)

  var selecting = false
  var chose_item = -1
  def input ( writer: PrintWriter, e: Event, vars: MainLoopVars, renderer: Renderer, game: JSONObject)
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
      case MouseClicked(_,coord,mod,nclick,_) => //determines if the player has clicked on a gametile, or on the ui and executes the corresponding action 
          // allow both movements and attacks of the player using the mouse
          // For now it is the only way to walk diagonaly
          println("MOUSE CLICKED")
          var clicked_x = ((coord.x -vars.dpos.x)/(vars.current_size.toFloat)).toInt
          var clicked_y = ((coord.y -vars.dpos.y)/(vars.current_size.toFloat)).toInt
          val max_x = (matrix_dim.width-ui_dim.width-2) 
          val max_y = (matrix_dim.height-ui_dim.height-2)
          val id = game.getInt("id")
          val players = game.getJSONArray("players_info")
          val player = players.getJSONObject(id)
          val inventory = player.getJSONArray("inventory")

          var ui_clicked_x = (coord.x/(renderer.tileset_handler.getSize().toFloat)).toInt
          var ui_clicked_y = (coord.y/(renderer.tileset_handler.getSize().toFloat)).toInt
          val istart = (matrix_dim.width-ui_dim.width)
          val iend = (matrix_dim.width-1)
          val jstart = (matrix_dim.height-ui_dim.height-1-(inventory.length/(ui_dim.width-2)+1)-1)-1
          val jend = (matrix_dim.height-ui_dim.height-1)-1
          val inventory_coord = ui_clicked_x-(istart+1)+(ui_clicked_y-(jstart+1))*(iend-istart-1)
          if(!selecting){ 
            // if we are not selecting then we are trying to click on inventory
            if(inventory_coord>= 0 && inventory_coord < inventory.length && !inventory.getJSONObject(inventory_coord).getBoolean("empty")){ // the player has clicked on the inventory
              val item = inventory.getJSONObject(inventory_coord) 
              if(mod==0){
                if(item.getBoolean("targetable")){
                  chose_item = inventory_coord
                  selecting = true
                }else{
                  msg.put("type","use")
                  msg.put("item",inventory_coord)
                  writer.println(msg.toString())
                }
              }else if(mod==256){
                msg.put("type","drop")
                msg.put("item",inventory_coord)
                writer.println(msg.toString())
              }
            }
          }else{
            if( ! (ui_clicked_x>max_x || ui_clicked_y > max_y || clicked_x>max_x || clicked_y > max_y || clicked_x<0 || clicked_y < 0)){
              msg.put("type","use_targetable")
              msg.put("item",chose_item)
              msg.put("pos",(new Position(clicked_x,clicked_y)).to_json())
              writer.println(msg.toString())
              //game.selecting = false
              //game.selectCallback = null
            }else{
            }
            // if we clicked on a invalid square,
            // cancel the selection
            selecting = false
          }
      case _ => {
      }
    }
  }
}
