package rogue

//definition of the player, its inventory and its ability to wait one turn
class Player(pos: Position, sprite: Sprite, collidable: Boolean, maxHealth: Int, hitChance: Int, hitDamage: Int,name_arg: String,name_color: String) extends LivingEntity(pos,0,sprite,collidable,maxHealth,hitChance, hitDamage, name_arg, name_color) {
  var canChangeFloor = true
  var canPickUp = true
  var firstSomethingElse = true
  move_cd = 100
  move_cd_max = 100
  armor = new GoldArmor()
  val inventory = new Inventory(12, this)
  def waitAction(){
    Log.addLogMessage( new LogMessage( List(
      name , new SubMessage(" waited ", "255255255"))))
  }
}

abstract class Status(sprite: Sprite, argDuration: Int) {
  var duration: Int = argDuration
  def effect(game: GameObject){}
}

class Poison(sprite: Sprite, argDuration: Int, target: LivingEntity) extends Status(sprite, argDuration){
  override def effect(game:GameObject){
    target.health -= 2
  }
}

class HealOverTime(sprite: Sprite, argDuration: Int, target: LivingEntity) extends Status(sprite, argDuration){
  override def effect(game: GameObject){
    target.health+= 3
  }
}

