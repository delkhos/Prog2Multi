package rogue

import swing._
import java.net._ ;

// This is the starting point of the swing application
class My_app(socket: Socket) extends MainFrame 
{
  title = "Dungeons Duo !"
  resizable = false
  visible = true
  //contents = new GamePanel(this, socket)
  contents = new GamePanelDuo(this,socket)
}
