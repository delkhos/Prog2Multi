package rogue

import java.awt.{Color,Graphics2D, Graphics}

abstract class QuestGiver(sprite: Sprite, collidable: Boolean, name_arg: String,name_color: String) extends LivingEntity(Origin, 0,sprite, collidable, 1, 1, 1, name_arg, name_color) {
  // this function is the function when the hero interacts with the pnj
  def interact(game: GameObject, id: Int){
  }
  // this function is called when the pnj is spawned,
  // for example to spawn the hammer of the blacksmith
  def spawn(game: GameObject, floor: Int){
  }
  def questAchieved(game: GameObject, reward: Item ){
    val itm = reward
    itm.pos = pos
    itm.floor = floor
    game.items = itm::game.items
    
  }
}

object ArchMage extends QuestGiver(
  new Sprite( Array[SubSprite](new SubSprite(2,SColor.BatPurple), new SubSprite(1,SColor.White)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ), true, "ArchMage", SColor.Grey){
    override def spawn(game: GameObject, floor: Int){
    val r = scala.util.Random
    var roll = r.nextInt(game.max_floor)
    println(floor)
    println(roll)
    while(roll == floor){
      roll = r.nextInt(game.max_floor)
      println(roll)
    }
    game.placeItemPNJSafe(new MysteriousScroll(), roll)
  }
  override def interact(game: GameObject, id: Int){
    if( game.players(id).inventory.contents.filter(itm => itm!=null && itm.name.text=="MysteriousScroll").length > 0){
      Log.addLogMessage( new LogMessage( List(
          new SubMessage( "ArchMage: \"Here it is\" ", SColor.White))
        )
      )
      this.state = State.Dead
      game.players(id).inventory.contents(game.players(id).inventory.contents.filter(itm => itm!=null && itm.name.text=="MysteriousScroll")(0).pos_in_inventory) = null
      questAchieved(game,new OblivionScroll())
    }else{
      Log.addLogMessage( new LogMessage( List(
        new SubMessage( "ArchMage: \"If you find any suspicious scroll,",SColor.White)
          )
        )
      )
      Log.addLogMessage( new LogMessage( List(
        new SubMessage("bring it to me, I'll give you something nice\" ", SColor.White)
          )
        )
      )
    }
  }
}


object Lady extends QuestGiver(
  new Sprite( Array[SubSprite](new SubSprite(2,SColor.Yellow), new SubSprite(1,SColor.Black)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ), true, "Lady", SColor.Grey){
    override def spawn(game: GameObject, floor: Int){
    val r = scala.util.Random
    var roll = r.nextInt(game.max_floor)
    println(floor)
    println(roll)
    while(roll == floor){
      roll = r.nextInt(game.max_floor)
      println(roll)
    }
    game.placeItemPNJSafe(new Collar(), roll)
  }
  override def interact(game: GameObject, id: Int){
    if( game.players(id).inventory.contents.filter(itm => itm!=null && itm.name.text=="Collar").length > 0){
      Log.addLogMessage( new LogMessage( List(
          new SubMessage( "Lady: \"Thank you ! Here is a gift for you\" ", SColor.White))
        )
      )
      this.state = State.Dead
      game.players(id).inventory.contents(game.players(id).inventory.contents.filter(itm => itm!=null && itm.name.text=="Collar")(0).pos_in_inventory) = null
      questAchieved(game,new Sapphire())
    }else{
      Log.addLogMessage( new LogMessage( List(
          new SubMessage( "Lady: \"My collar fell on the road...\" ", SColor.White)
          )
        )
      )
    }

  }
}




object Blacksmith extends QuestGiver(
  new Sprite( Array[SubSprite](new SubSprite(2,SColor.Grey),new SubSprite(1,SColor.White)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ), true, "Blacksmith", SColor.Grey){
    // spawn the hammer of the blacksmith
  override def spawn(game: GameObject, floor: Int){
    val r = scala.util.Random
    var roll = r.nextInt(game.max_floor)
    // we choose a random floor for the pnj
    println(floor)
    println(roll)
    while(roll == floor){
      roll = r.nextInt(game.max_floor)
      println(roll)
    }
    game.placeItemPNJSafe(new Hammer(), roll)
  }
  // when the player doesn't have the hammer, say that he needs it
  // otherwise, disappear and gives the rewards
  override def interact(game: GameObject, id: Int){
    if( game.players(id).inventory.contents.filter(itm => itm!=null && itm.name.text=="Hammer").length > 0){
      Log.addLogMessage( new LogMessage( List(
          new SubMessage( "Blacksmith: \"Thank you !\" ", SColor.White))
        )
      )
      this.state = State.Dead
      game.players(id).inventory.contents(game.players(id).inventory.contents.filter(itm => itm!=null && itm.name.text=="Hammer")(0).pos_in_inventory) = null
      questAchieved(game,new GoldSword())
      // peut être voir pour une récompense
    }else{
      Log.addLogMessage( new LogMessage( List(
          new SubMessage( "Blacksmith: \"I lost my Hammer\" ", SColor.White)
          )
        )
      )
    }

  }
}

object Quests{
  var quests = List(Blacksmith,Lady,ArchMage)
}
