package rogue

import org.json._

/*
 * This is a submessage.
 * It is defined by a string and by a color.
 */
class SubMessage(str: String, col: String){
  val text = str
  val color = col
}

/*
 * A log message is a list of SubMessages.
 * We define it this way to print complex colored strings
 */
class LogMessage(arg_sub_messages: List[SubMessage]){
  val sub_messages = arg_sub_messages
  def to_json(): JSONArray = {
    val submsgs = new JSONArray()
    for ( submsg <- sub_messages ){
      val submsg_json:JSONObject = new JSONObject()
      submsg_json.put("color",submsg.color)
      submsg_json.put("text",submsg.text)
      submsgs.put(submsg_json)
    }
    return submsgs
  }
}

/*
 * The log is a finite list messages, to be printed to the screen
 */
object Log{ //register the messages to display on screen
  var messages = List[LogMessage]()
  def addLogMessage(msg: LogMessage){
    if(messages.length == 14){
      messages = messages.reverse.tail.reverse
    }
    messages = msg :: messages
  }
  def to_json(): JSONArray = {
    val json = new JSONArray()
    for(msg <- messages){
      val msg_json = msg.to_json()
      json.put(msg_json)      
    }
    return json
  }
  def displayWin(){
    addLogMessage( new LogMessage( List( new SubMessage(" YOU WIN ","255255000") )))
    addLogMessage( new LogMessage( List(
      new SubMessage(" C","204000204"), 
      new SubMessage("O","153000255"), 
      new SubMessage("N","102000255"),
      new SubMessage("G","102051255"),
      new SubMessage("R","102163255"),
      new SubMessage("A","102204204"), 
      new SubMessage("T","102255051"),
      new SubMessage("U","102255000"),
      new SubMessage("L","255255000"),
      new SubMessage("A","255204000"),
      new SubMessage("T","255102000"),
      new SubMessage("I","255051000"), 
      new SubMessage("O","255025000"),
      new SubMessage("N","255000000"),
      new SubMessage("S","255000000")
    ) ) )
  }
}
