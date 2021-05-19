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
  var lastSeenPos: Position = null
  var sprite = sprite_arg

  // This function is used to draw the entity to the screen
  def draw(g: Graphics2D, current_size: Int, tileset_handler: TileSetHandler, dpos: DPosition){
    g.setColor(sprite.getBgColor());
    val size:Float = current_size
    val sx1:Float = (pos.x)*size +dpos.x //-rx
    val sy1:Float = (pos.y)*size + dpos.y //-ry 
    val sx2:Float = sx1 + (size)
    val sy2:Float = sy1 + (size)
    g.fillRect(sx1.toInt,sy1.toInt, current_size, current_size)

    val elements = sprite.getElements()
    for(i <- 0 to (elements.size-1) ){
      val c = elements(i).getCharCode()
      val xv:Int = c % 16 
      val yv:Int = c / 16 
      val dx1:Int = xv*tileset_handler.getSize() 
      val dy1:Int = yv*tileset_handler.getSize()
      val dx2:Int = dx1+(tileset_handler.getSize())
      val dy2:Int = dy1+(tileset_handler.getSize())
      g.drawImage(tileset_handler.getColoredTileset(elements(i).getColor()), sx1.toInt, sy1.toInt, sx2.toInt, sy2.toInt, dx1, dy1, dx2, dy2, null)
    }
  }
  // This is an overload of the above function used to draw an entity at a position on the screen
  // that is not its own position. This is used to draw items in the inventory once they have been picked up
  def draw(g: Graphics2D, current_size: Int, tileset_handler: TileSetHandler, dpos: DPosition,arg_pos: Position){
    g.setColor(sprite.getBgColor());
    val size:Float = current_size
    val sx1:Float = (arg_pos.x)*size +dpos.x //-rx
    val sy1:Float = (arg_pos.y)*size + dpos.y //-ry 
    val sx2:Float = sx1 + (size)
    val sy2:Float = sy1 + (size)
    g.fillRect(sx1.toInt,sy1.toInt, current_size, current_size)

    val elements = sprite.getElements()
    for(i <- 0 to (elements.size-1) ){
      val c = elements(i).getCharCode()
      val xv:Int = c % 16 
      val yv:Int = c / 16 
      val dx1:Int = xv*tileset_handler.getSize() 
      val dy1:Int = yv*tileset_handler.getSize()
      val dx2:Int = dx1+(tileset_handler.getSize())
      val dy2:Int = dy1+(tileset_handler.getSize())
      g.drawImage(tileset_handler.getColoredTileset(elements(i).getColor()), sx1.toInt, sy1.toInt, sx2.toInt, sy2.toInt, dx1, dy1, dx2, dy2, null)
    }
  }
  // This is another overload, that does the same as the original, but has 
  // one more argument, so as to draw a greyer version of the entity
  def draw(g: Graphics2D, current_size: Int, tileset_handler: TileSetHandler, dpos: DPosition, ratio: Double){
    val bg = sprite.getBgColor()
    g.setColor(new Color( ((bg.getRed() * ratio) / 255.0).toFloat , ((bg.getGreen() * ratio)/255.0).toFloat, ((bg.getBlue() * ratio)/ 255.0).toFloat, (bg.getAlpha()/255.0).toFloat   ));
    val size:Float = current_size
    val sx1:Float = (lastSeenPos.x)*size + dpos.x //-rx
    val sy1:Float = (lastSeenPos.y)*size + dpos.y //-ry 
    val sx2:Float = sx1 + (size)
    val sy2:Float = sy1 + (size)
    g.fillRect(sx1.toInt,sy1.toInt, current_size, current_size)

    val elements = sprite.getElements()
    for(i <- 0 to (elements.size-1) ){
      val fg = elements(i).getColor()
      val c = elements(i).getCharCode()
      val xv:Int = c % 16 
      val yv:Int = c / 16 
      val dx1:Int = xv*tileset_handler.getSize() 
      val dy1:Int = yv*tileset_handler.getSize()
      val dx2:Int = dx1+(tileset_handler.getSize())
      val dy2:Int = dy1+(tileset_handler.getSize())
      val red = "%03d".format((fg.substring(0,3).toInt*ratio).toInt)
      val green = "%03d".format((fg.substring(3,6).toInt*ratio).toInt)
      val blue = "%03d".format((fg.substring(6,9).toInt*ratio).toInt)
      g.drawImage(tileset_handler.getColoredTileset(red+green+blue), sx1.toInt, sy1.toInt, sx2.toInt, sy2.toInt, dx1, dy1, dx2, dy2, null)
    }
  }
  // This function function is used by the renderer so as to draw entities, according to whether or not

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


  def update(t: Int){
    move_cd = (move_cd - t).max(0)
    attack_cd = (attack_cd - t).max(0)
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
