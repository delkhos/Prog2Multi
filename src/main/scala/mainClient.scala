package rogue

import java.io._ ;
import java.net._ ;
import java.util.Date;
import java.awt.event.KeyEvent;
import java.awt.Color

object RogueClient {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) return;
 
    val hostname = args(0);
    val port = Integer.parseInt(args(1));

    try {
      val socket = new Socket(hostname, port);
      
      val app = new My_app(socket)
      app.visible = true

    } catch {
      case ex: UnknownHostException => {
        println("Server not found: " + ex.getMessage());
      }
      case ex: IOException => {
        System.out.println("I/O error: " + ex.getMessage());
      }
    }
  }
}
