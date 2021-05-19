package rogue

//definition of the player, its inventory and its ability to wait one turn
class Player(pos: Position, sprite: Sprite, collidable: Boolean, maxHealth: Int, hitChance: Int, hitDamage: Int,name_arg: String,name_color: String) extends LivingEntity(pos,0,sprite,collidable,maxHealth,hitChance, hitDamage, name_arg, name_color) {
  var canChangeFloor = true
  var canPickUp = true
  var firstSomethingElse = true
  move_cd = 100
  move_cd_max = 100
  armor = new LeatherArmor()
  val inventory = new Inventory(12, this)
  def waitAction(){
    Log.addLogMessage( new LogMessage( List(
      name , new SubMessage(" waited ", "255255255"))))
  }
}

abstract class Status(sprite: Sprite, argDuration: Int) {
  var ended = false
  var name = ""
  var duration: Int = argDuration
  def effect(){}
}

class Poison(sprite: Sprite, argDuration: Int, target: LivingEntity) extends Status(sprite, argDuration){
  override def effect(){
    if (duration <= 0){
      ended = true
    }

    if (duration % 100 == 0){
      target.health -= 2
    }
  }
}

class Enraged(sprite: Sprite, argDuration: Int, target: LivingEntity) extends Status(sprite, argDuration){
  name = "enraged"
  override def effect(){  
    if (duration <= 0){
      ended = true
    }
  }

}

class BronzeSkin(sprite: Sprite, argDuration: Int, target: LivingEntity) extends Status(sprite, argDuration){
  name = "bronzeSkin"
  override def effect(){  
    if (duration <= 0){
      ended = true
    }
  }


}



class HealOverTime(sprite: Sprite, argDuration: Int, target: LivingEntity) extends Status(sprite, argDuration){
  override def effect(){
    target.health+= 3
  }
}

