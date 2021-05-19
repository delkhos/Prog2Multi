package rogue

import java.awt.Color
import java.awt.Graphics2D

/*
 * This class defines an entity, which can be anything that can be drawn on the screen
 * it has a Position and a Sprite
 */
abstract class Entity (arg_pos: Position, arg_floor: Int, sprite_arg: Sprite) {
  var pos = arg_pos
  var floor = arg_floor
  var lastSeenPos: Array[Position] = Array(null,null)
  var sprite = sprite_arg

}


/*
 * A LivingEntity is a class that inherits Entity.
 * It represents an entity that has health points,
 * that can potentialy attack another entity,
 * that has a name.
 * It also has a state to work as a state machine
 * (and also to know if it is dead)
 */
abstract class LivingEntity(arg_pos: Position, arg_floor: Int, sprite: Sprite, collidable: Boolean, maxHealth_arg: Int, hitChance: Int, hitDamage: Int, name_arg: String, name_color: String) extends Entity(arg_pos,arg_floor,sprite) {
  var health = maxHealth_arg
  var max_health = maxHealth_arg
  val name = new SubMessage(name_arg,name_color)
  var state = State.Idle
  var weapon:Weapon = null
  var ruby = false
  var sapphire = false
  var emerald = false
  var armor:Armor = null
  var status: List[Status] = List()
  var move_cd = 0
  var move_cd_max = 200
  var attack_cd = 0
  var attack_cd_max = 300
  def implementationName = this.getClass().getName().stripSuffix("$")

  def removeStatus( statusList: List[Status]): List[Status] = {
    statusList match {
      case List() => { return (List()) }

      case (s :: tl) => { 
        if (s.ended){
          return removeStatus(tl)
        }else{
          return (s :: removeStatus(tl))
        }
      }
    }
  }



  def update(t: Int){
    move_cd = (move_cd - t).max(0)
    attack_cd = (attack_cd - t).max(0)
    status.foreach (s => {
      s.effect()
      s.duration-=t
    })
    this.removeStatus(status)  
  }


  // This is the attack method, it rolls a number to know if the attack lends.
  // It takes into account the equipped weapon and the armor of the attackee
  // If it does, it reduces the targets health, and prints a message to the log.
  // If it does not, it prints a message to convey "the miss".
  // Also if the target health drops below 0, this method is in charge of putting it in the dead state
  def attack( attackee: LivingEntity){ 
    val r = scala.util.Random
    var damage = hitDamage

    var atkmul = 1
    var defdiv = 1
    if (status.exists(s => {s.name == "enraged"})){
      atkmul *= 2
    }

    if (attackee.status.exists(s => {s.name == "enraged"})){
      defdiv *= 2
    }
    if (attackee.status.exists(s => {s.name == "bronzeSkin"})){
      defdiv = 0
    }

    if(weapon != null){
      damage += weapon.damage
    
      if (ruby){
        damage += 3
      }
    } 
    if(attackee.armor != null){
      damage -= attackee.armor.defense
      if(attackee.sapphire){
        damage -=1
      }
      if(damage < 0)
        damage = 0
    }
    val attack_try = r.nextInt(100)
    if(attack_try <= hitChance){
      damage = damage*defdiv*atkmul
      attackee.health -= damage
      Log.addLogMessage( new LogMessage( List(
        name , new SubMessage(" attacked ", "255255255")
          , attackee.name , new SubMessage(" for ", "255255255")
          , new SubMessage( damage.toString, "255000000")
          , new SubMessage( " damage(s).", "255255255")
         )
        )
      )
      if (attackee.health<=0) {
        attackee.state = State.Dead
        Log.addLogMessage( new LogMessage( List( attackee.name , new SubMessage(" died.", "255255255"))))
      }
    }else{
      Log.addLogMessage( new LogMessage( List(
        name , new SubMessage(" attacked ", "255255255")
          , attackee.name , new SubMessage(" and missed.", "255255255")
         )
        )
      )
    }
  }
  
  // This function is to take damage from indirect sources
  // for example the fire scroll
  def takeDamage(amount:Int){
    this.health -= amount
    Log.addLogMessage( new LogMessage( List(
      this.name , new SubMessage(" lost ", "255255255")
        , new SubMessage(amount.toString, SColor.Red)
        , new SubMessage( " health point(s).", "255255255")
       )
      )
    )
    if (this.health<=0) {
      this.state = State.Dead
      Log.addLogMessage( new LogMessage( List( this.name , new SubMessage(" died.", "255255255"))))
    }
  }
}
