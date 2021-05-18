package rogue

import org.json._

object MSGTypes {
  val Movement = "movement"
}


object MSGHandler {
  def treatMessage(msg: JSONObject, game: GameObject, id_arg: Int){
    msg.getString("type") match {
      case MSGTypes.Movement => {
        val id = id_arg
        val pos = game.players(id).pos
        val new_pos = new Position(pos.x,pos.y)
        
        msg.getString("direction") match {
          case "UP" => new_pos.y = new_pos.y - 1
          case "DOWN" => new_pos.y = new_pos.y + 1
          case "LEFT" => new_pos.x = new_pos.x - 1
          case "RIGHT" => new_pos.x = new_pos.x + 1
          case "UPRIGHT" => {
            new_pos.x = new_pos.x + 1
            new_pos.y = new_pos.y - 1
          }
          case "DOWNRIGHT" => { 
            new_pos.x = new_pos.x + 1
            new_pos.y = new_pos.y + 1
          }
          case "UPLEFT" => { 
            new_pos.x = new_pos.x - 1
            new_pos.y = new_pos.y - 1
          }
          case "DOWNLEFT" => {
            new_pos.x = new_pos.x - 1
            new_pos.y = new_pos.y + 1
          }
          case "WAIT" => {
          }
        }
        game.move(id, new_pos)
      }
      case _ => println("Error : Unknown message")
    }
  }
  
}
