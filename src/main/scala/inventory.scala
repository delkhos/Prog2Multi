package rogue

import scala.util.control.Breaks._
import org.json._

/*
 * The inventory handles the player's items
 */
class Inventory(x: Int, owner: Player){ //definition of the inventory that will be displayed on the side of the screen
  val contents = Array.fill[Item](x)(null)
  val amount = Array.fill[Int](x)(0)
  val size = x

  def addItem(item: Item){ // if an item is picked up, the inventory is updated, and the item disappears from the ground
    // if the item is stackable, we find the first spot that
    // has this item, and is not full
    var right_index = -1 
    if(item.max_stack > 1){
      for(i <- 0 to (size-1)){
        if(contents(i) != null){
          if(contents(i).name.text == item.name.text && amount(i) < item.max_stack)
            right_index = i
        }
      }
    }



    for(i <- 0 to (size-1)){
      if( ( contents(i) == null && right_index == -1 ) || (right_index==i)){ //looks for the first availabe slot in the inventory
        if(contents(i) ==null){
          contents(i) = item //fills the slot
          item.pos_in_inventory = i
          amount(i) = 0
        }
        item.on_the_ground = false //removes the item from the ground. If no slot is available, the item stays on the ground
        amount(i)+=1
        Log.addLogMessage( new LogMessage( List(
          owner.name , new SubMessage(" picked up a ", "255255255")
            , item.name )
          )
        )
        return // leave early if the inventory was not full
      }
    }
    Log.addLogMessage( new LogMessage( List(
      owner.name , new SubMessage(" inventory is full", "255255255"))
      )
    )
  }
  def dropItem(item: Int,game: GameObject, floor: Int,  id: Int){
    val itm_to_drop = contents(item)
    val amount_to_drop = amount(item)
    itm_to_drop.on_the_ground = true
    itm_to_drop.pos = new Position(game.players(id).pos.x,game.players(id).pos.y)
    itm_to_drop.floor = floor
    game.items = itm_to_drop::game.items
    for(i <- 2 to amount_to_drop){
      val copy =  Class.forName(itm_to_drop.implementationName).newInstance().asInstanceOf[Item] 
      copy.pos = new Position(game.players(id).pos.x,game.players(id).pos.y)
      copy.floor = floor
      game.items = copy::game.items
    }
  }

  def to_json(): JSONArray = {
    val json = new JSONArray()
    for(i <- 0 to (size-1)){
      val item = contents(i)
      val itm_json = new JSONObject()
      if (item == null){
        itm_json.put("empty",true)
      }else{
        itm_json.put("empty",false)
        itm_json.put("sprite", item.sprite.to_json())
        itm_json.put("amount", amount(i))
        itm_json.put("name_color", item.name.color)
        itm_json.put("name", item.name.text)
        itm_json.put("targetable", item.targetable) 
        if(item.max_stack > 1){
          itm_json.put("stackable",true)
        }else{
          itm_json.put("stackable",false)
        }
      }
      json.put(itm_json)
    }
    return json
  }

}
