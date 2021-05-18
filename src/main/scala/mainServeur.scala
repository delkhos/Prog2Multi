package rogue

import java.awt.Color
import java.io._ ;
import java.net._ ;
import java.util.Date;
import java.awt.event.KeyEvent;
import org.json._


object RogueServer {

  var continue = true

  var instance = new GameObject(new Dimension(50,30),15,2)

  class SendGameState(writer: PrintWriter, father: Thread, id: Int) extends Thread 
  {
    override def run()
    {
      // Displaying the thread that is running 
      try {
        while(father.isAlive()){
          Thread.sleep(17)
          instance.synchronized {
            val json = instance.to_json(id)
            json.put("id",id)
            writer.println(json.toString())
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
     continue = false
    }
  }
  class ListenerPlayer(reader: BufferedReader, father: Thread, id: Int) extends Thread 
  {
    override def run()
    {
      // Displaying the thread that is running 
      try {
        while(father.isAlive()){
          val msg = reader.readLine();
          val msg_json = new JSONObject(msg)
          instance.synchronized {
            MSGHandler.treatMessage(msg_json, instance,id)
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
     continue = false
    }
  }


  def main(args: Array[String]): Unit = {
    println("Starting server")

    if (args.length < 1) return;
 
    val port = Integer.parseInt(args(0));

    try {
      val serverSocket = new ServerSocket(port);

        System.out.println("Server is listening on port " + port);

        val socketp1 = serverSocket.accept();

        System.out.println("Player 1 connected");

        val outputp1:OutputStream = socketp1.getOutputStream();
        val writerp1:PrintWriter = new PrintWriter(outputp1, true);
        val inputp1:InputStream = socketp1.getInputStream();    
        val readerp1:BufferedReader = new BufferedReader(new InputStreamReader(inputp1));  
        val player1_thread = new SendGameState(writerp1, Thread.currentThread(),0)
        player1_thread.start()
        val player1_listener = new ListenerPlayer(readerp1, Thread.currentThread(),0)
        player1_listener.start()


        val socketp2 = serverSocket.accept();

        System.out.println("Player 2 connected");

        val outputp2:OutputStream = socketp2.getOutputStream();
        val writerp2:PrintWriter = new PrintWriter(outputp2, true);
        val inputp2:InputStream = socketp2.getInputStream();    
        val readerp2:BufferedReader = new BufferedReader(new InputStreamReader(inputp2));  
        val player2_thread = new SendGameState(writerp2, Thread.currentThread(),1)
        player2_thread.start()
        val player2_listener = new ListenerPlayer(readerp2, Thread.currentThread(),1)
        player2_listener.start()
        

        while(continue){
          Thread.sleep(10)
          instance.synchronized {
            instance.update(10)
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
     println("Stopping Server...\n");
  }
}
