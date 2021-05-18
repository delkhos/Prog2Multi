package rogue

import java.awt.{Color,Graphics2D, Graphics}

/*
 * An item is an entity that can be both on the ground and in the player's inventory
 * It has a pickup method to get inside the inventory.
 * It also has a "use" method to define what happens when it is used
 */
abstract class Item (arg_pos: Position, arg_floor: Int, sprite: Sprite, arg_equipable: Boolean, name_arg: String, name_color: String, max_stack_arg: Int, tall_arg: Boolean) extends Entity(arg_pos,arg_floor,sprite) {
  var equipable = arg_equipable 
  var on_the_ground = true
  var targetable = false
  var tall = tall_arg
  var pos_in_inventory = 0
  var max_stack = max_stack_arg
  val name = new SubMessage(name_arg,name_color)
  def implementationName = this.getClass().getName().stripSuffix("$")
  def pickUp(game: GameObject ){
    if( game.players(0).pos == pos && game.players(0).floor == floor ){
      if(game.players(0).canPickUp){
        game.players(0).inventory.addItem(this)
        game.players(0).canPickUp = false
      }else if( game.players(0).firstSomethingElse){
        game.players(0).firstSomethingElse = false
        Log.addLogMessage( new LogMessage( List(
          new SubMessage( "There is something else here...", SColor.White)
            )
          )
        )
      }
    } else if( game.players(1).pos == pos  && game.players(1).floor == floor ){
      if(game.players(1).canPickUp){
        game.players(1).inventory.addItem(this)
        game.players(1).canPickUp = false
      }else if( game.players(1).firstSomethingElse){
        game.players(1).firstSomethingElse = false
        Log.addLogMessage( new LogMessage( List(
          new SubMessage( "There is something else here...", SColor.White)
            )
          )
        )
      }
    }
  }
  def use(game: GameObject, id: Int):Boolean = {
    return false
  }
}

abstract class Armor (defense_arg: Int, name: String, color: String, skin: Sprite) extends Item (
  Origin, 0,
  skin,
  true,
  name,
  color,
  1, false){
  var defense = defense_arg
  override def use(game: GameObject, id: Int):Boolean = {
    val tmp = game.players(id).armor
    game.players(id).armor = this
    game.players(id).inventory.contents(pos_in_inventory) = tmp
    if(tmp==null){
      game.players(id).inventory.amount(pos_in_inventory) = 0
    }else{
      tmp.pos_in_inventory = pos_in_inventory
    }
    return true
  }
}

class LeatherArmor extends Armor (1, "Leather Armor",SColor.Leather, 
  new Sprite( Array[SubSprite](new SubSprite(240,SColor.Leather)),new Color(0.0f,0.0f,0.0f,0.0f))
  ){}
class ChainMail extends Armor (2, "Chainmail",SColor.Steel, 
  new Sprite( Array[SubSprite](new SubSprite(240,SColor.Steel)),new Color(0.0f,0.0f,0.0f,0.0f))
  ){}
class IronPlate  extends Armor (3, "Iron Plate",SColor.Cyan, 
  new Sprite( Array[SubSprite](new SubSprite(240,SColor.Cyan)),new Color(0.0f,0.0f,0.0f,0.0f))
  ){}

class GoldArmor extends Armor (10, "Gold Armor",SColor.Gold, 
  new Sprite( Array[SubSprite](new SubSprite(240,SColor.Gold)),new Color(0.0f,0.0f,0.0f,0.0f))
  ){}
abstract class Weapon (damage_arg: Int, name: String, color: String, skin: Sprite) extends Item (
  Origin, 0,
  skin,
  true,
  name,
  color,
  1, false){
  var damage = damage_arg
  override def use(game: GameObject, id: Int):Boolean ={
    val tmp = game.players(id).weapon
    game.players(id).weapon = this
    game.players(id).inventory.contents(pos_in_inventory) = tmp
    if(tmp==null){
      game.players(id).inventory.amount(pos_in_inventory) = 0
    }else{
      tmp.pos_in_inventory = pos_in_inventory
    }
    return true
  }
}
class RustySword extends Weapon (5, "Rusty Sword",SColor.Rust, 
  new Sprite( Array[SubSprite](new SubSprite(47,SColor.Rust)),new Color(0.0f,0.0f,0.0f,0.0f))
  ){}
class SteelSword extends Weapon (8, "Steel Sword",SColor.Steel, 
  new Sprite( Array[SubSprite](new SubSprite(47,SColor.Steel)),new Color(0.0f,0.0f,0.0f,0.0f))
  ){}
class GoodSword extends Weapon (12, "Good Sword", SColor.Cyan,
  new Sprite( Array[SubSprite](new SubSprite(47, SColor.Cyan)),new Color(0.0f,0.0f,0.0f,0.0f))
  ){}

class GoldSword extends Weapon (20, "Gold Sword",SColor.Gold, 
  new Sprite( Array[SubSprite](new SubSprite(47,SColor.Gold)),new Color(0.0f,0.0f,0.0f,0.0f))
  ){}
object EquipFactory{
  var tier1: List[String] = List()
  var tier2: List[String] = List()
  var tier3: List[String] = List()
  tier1 = "LeatherArmor"::tier1
  tier1 = "RustySword"::tier1
  tier2 = "ChainMail"::tier2
  tier2 = "SteelSword"::tier2
  tier3 = "GoodSword"::tier3
  tier3 = "IronPlate"::tier3
  
  def spawnEquipTier1():Item = {
    val r = scala.util.Random
    return Class.forName("rogue."+tier1(r.nextInt(tier1.length))).newInstance().asInstanceOf[Item] 
  }
  def spawnEquipTier2():Item = {
    val r = scala.util.Random
    return Class.forName("rogue."+tier2(r.nextInt(tier2.length))).newInstance().asInstanceOf[Item] 
  }
  def spawnEquipTier3():Item = {
    val r = scala.util.Random
    return Class.forName("rogue."+tier3(r.nextInt(tier3.length))).newInstance().asInstanceOf[Item] 
  }
  def spawnEquip(tier: Int):Item = {
    tier match {
      case 1 => return spawnEquipTier1
      case 2 => return spawnEquipTier2
      case 3 => return spawnEquipTier3
    }
  }
}


class HealingPotion () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(3,SColor.DeepPink)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Healing potion",
  SColor.DeepPink,
  5, false){

  override def use(game: GameObject, id: Int):Boolean = {
    // heals 5 health points
    Log.addLogMessage( new LogMessage( List(
        game.players(id).name,
        new SubMessage( " recovers", SColor.DeepPink),
        new SubMessage( " 5 health points", SColor.White))
      )
    )
    game.players(id).health += 5
    if(game.players(id).health > game.players(id).max_health)
      game.players(id).health = game.players(id).max_health
    // Removes itself from the inventory
    game.players(id).inventory.amount(pos_in_inventory) -= 1
    if( game.players(id).inventory.amount(pos_in_inventory) == 0 )
      game.players(id).inventory.contents(pos_in_inventory) = null
    return true
  }
}
class HealingPotionP () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(3,SColor.DeepPink),new SubSprite(43,SColor.Blue)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Healing potion +",
  SColor.DeepPink,
  4, false){

  override def use(game: GameObject, id: Int):Boolean = {
    // heals 5 health points
    Log.addLogMessage( new LogMessage( List(
        game.players(id).name,
        new SubMessage( " recovers", SColor.DeepPink),
        new SubMessage( " 8 health points", SColor.White))
      )
    )
    game.players(id).health += 8
    if(game.players(id).health > game.players(id).max_health)
      game.players(id).health = game.players(id).max_health
    // Removes itself from the inventory
    game.players(id).inventory.amount(pos_in_inventory) -= 1
    if( game.players(id).inventory.amount(pos_in_inventory) == 0 )
      game.players(id).inventory.contents(pos_in_inventory) = null
    return true
  }
}
  
class HealingPotionX () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(3,SColor.DeepPink),new SubSprite(88,SColor.BatPurple)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Healing potion X",
  SColor.DeepPink,
  3, false){

  override def use(game: GameObject, id: Int):Boolean = {
    // heals 5 health points
    Log.addLogMessage( new LogMessage( List(
        game.players(id).name,
        new SubMessage( " recovers", SColor.DeepPink),
        new SubMessage( " 12 health points", SColor.White))
      )
    )
    game.players(id).health += 12
    if(game.players(id).health > game.players(id).max_health)
      game.players(id).health = game.players(id).max_health
    // Removes itself from the inventory
    game.players(id).inventory.amount(pos_in_inventory) -= 1
    if( game.players(id).inventory.amount(pos_in_inventory) == 0 )
      game.players(id).inventory.contents(pos_in_inventory) = null
    return true
  }
}


class Hammer () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(26,SColor.White)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Hammer",
  SColor.White,
  1, false){
}
class MysteriousScroll () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(13,SColor.White)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "MysteriousScroll",
  SColor.White,
  1, false){
}

class Collar() extends Item (
  Origin,0,
  new Sprite( Array[SubSprite](new SubSprite(9,SColor.White)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Collar",
  SColor.White,
  1, false){
}

class Sapphire () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(4,SColor.Cyan)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Sapphire",
  SColor.Cyan,
  1, false){
    override def pickUp(game: GameObject){
      if( game.players(0).pos == pos  && game.players(0).floor == floor ){
        if(game.players(0).canPickUp){
          game.players(0).inventory.addItem(this)
          game.players(0).sapphire = true
          game.players(0).canPickUp = false
        }else if( game.players(0).firstSomethingElse){
          game.players(0).firstSomethingElse = false
          Log.addLogMessage( new LogMessage( List(
            new SubMessage( "There is something else here...", SColor.White)
              )
            )
          )
        }
      }else if( game.players(1).pos == pos  && game.players(1).floor == floor ){
        if(game.players(1).canPickUp){
          game.players(1).inventory.addItem(this)
          game.players(1).sapphire = true
          game.players(1).canPickUp = false
        }else if( game.players(1).firstSomethingElse){
          game.players(1).firstSomethingElse = false
          Log.addLogMessage( new LogMessage( List(
            new SubMessage( "There is something else here...", SColor.White)
              )
            )
          )
        }
      }
    }
}
class Emerald () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(4,SColor.Green)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Emerald",
  SColor.Green,
  1, false){
    override def pickUp(game: GameObject ){
      if( game.players(0).pos == pos  && game.players(0).floor == floor ){
        if(game.players(0).canPickUp){
          game.players(0).inventory.addItem(this)
          if (!game.players(0).emerald){
            game.players(0).max_health = 75
          }
          game.players(0).emerald = true
          game.players(0).canPickUp = false
        }else if( game.players(0).firstSomethingElse){
          game.players(0).firstSomethingElse = false
          Log.addLogMessage( new LogMessage( List(
            new SubMessage( "There is something else here...", SColor.White)
              )
            )
          )
        }
      }else if( game.players(1).pos == pos  && game.players(1).floor == floor ){
        if(game.players(1).canPickUp){
          game.players(1).inventory.addItem(this)
          if (!game.players(1).emerald){
            game.players(1).max_health = 75
          }
          game.players(1).emerald = true
          game.players(1).canPickUp = false
        }else if( game.players(1).firstSomethingElse){
          game.players(1).firstSomethingElse = false
          Log.addLogMessage( new LogMessage( List(
            new SubMessage( "There is something else here...", SColor.White)
              )
            )
          )
        }
      }
    }
}
class Ruby () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(4,SColor.Red)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Ruby",
  SColor.Red,
  1, false){
    override def pickUp(game: GameObject ){
      if( game.players(0).pos == pos  && game.players(0).floor == floor ){
        if(game.players(0).canPickUp){
          game.players(0).inventory.addItem(this)
          game.players(0).ruby = true
          game.players(0).canPickUp = false
        }else if( game.players(0).firstSomethingElse){
          game.players(0).firstSomethingElse = false
          Log.addLogMessage( new LogMessage( List(
            new SubMessage( "There is something else here...", SColor.White)
              )
            )
          )
        }
      }else if( game.players(1).pos == pos  && game.players(1).floor == floor ){
        if(game.players(1).canPickUp ){
          game.players(1).inventory.addItem(this)
          game.players(1).ruby = true
          game.players(1).canPickUp = false
        }else if( game.players(1).firstSomethingElse){
          game.players(1).firstSomethingElse = false
          Log.addLogMessage( new LogMessage( List(
            new SubMessage( "There is something else here...", SColor.White)
              )
            )
          )
        }
      }
    }
}
class FakeWall () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(282,SColor.FakeWallGrey)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "",
  SColor.Black,
  1, true){
    override def pickUp(game: GameObject){
      if( game.players(0).pos == pos && floor == game.players(0).floor ){
        this.on_the_ground = false
      }else if( game.players(1).pos == pos && floor == game.players(1).floor ){
        this.on_the_ground = false
      }
    }
}

class FireScroll () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(14,SColor.Orange)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Fire Scroll",
  SColor.Orange,
  1, false){

  override def use(game: GameObject, id: Int):Boolean = {
    Log.addLogMessage( new LogMessage( List(
        new SubMessage( "A wave of ", SColor.White),
        new SubMessage( "fire ", SColor.Orange),
        new SubMessage( " engulfs ", SColor.White),
        game.players(id).name
        )
      )
    )
    game.monsters.foreach( m => {
      val deltax = scala.math.abs(m.pos.x -game.players(id).pos.x) 
      val deltay = scala.math.abs(m.pos.y -game.players(id).pos.y) 
      if( deltax<=1 && deltay<=1 && m.floor == game.current_floor){
        m.takeDamage(20)
      }
    }
    )
    game.players(id).inventory.contents(pos_in_inventory) = null
    game.players(id).inventory.amount(pos_in_inventory) = 0
    return true
  }
}

  // describes an item that needs a selection, the callback is called when the selection has been made
trait Callbackable {
  def callback(mpos:Position, game_arg:GameObject):Boolean = {
    return false
  }
}

class TeleportScroll () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(14,SColor.White)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Teleport Scroll",
  SColor.White,
  1, false) {

  targetable = true

  def usePos(mpos:Position, game_arg:GameObject, id: Int) {
    if(!game_arg.occupied2players(mpos,game_arg.players(id).floor)){
      Log.addLogMessage( new LogMessage( List(
          game_arg.players(id).name,
          new SubMessage( " teleported.", SColor.White)
        )
        )
      )
      game_arg.players(id).pos = mpos
      game_arg.players(id).inventory.contents(pos_in_inventory) = null
      game_arg.players(id).inventory.amount(pos_in_inventory) = 0
    }else {
      Log.addLogMessage( new LogMessage( List(
          game_arg.players(id).name,
          new SubMessage( " can't teleport here.", SColor.White)
        )
        )
      )
    }
  }
}

class LightningScroll () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(14,SColor.Yellow)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Lightning Scroll",
  SColor.Yellow,
  1, false) {

  targetable = true

  def usePos(mpos:Position, game_arg:GameObject,id: Int){
    if(game_arg.lineOfSight(game_arg.players(id).pos,mpos,game_arg.players(id).floor)){
      Log.addLogMessage( new LogMessage( List(
          new SubMessage( "A", SColor.White),
          new SubMessage( " bolt ", SColor.Yellow),
          new SubMessage( "zaps the ground.", SColor.White)
        )
        )
      )
      game_arg.monsters.foreach( m =>  if(m.pos == mpos){
          m.takeDamage(30)
        }
      
      )
      game_arg.players(id).inventory.contents(pos_in_inventory) = null
      game_arg.players(id).inventory.amount(pos_in_inventory) = 0
    }else {
      Log.addLogMessage( new LogMessage( List(
          game_arg.players(id).name,
          new SubMessage( " can't zap here.", SColor.White)
        )
        )
      )
    }
  
  }
}

class OblivionScroll () extends Item (
  Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(13,SColor.BatPurple)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "Oblivion Scroll",
  SColor.Yellow,
  1, false){ 

  override def use(game: GameObject, id: Int):Boolean = {
     Log.addLogMessage( new LogMessage( List(
        new SubMessage( "A wave of ", SColor.White),
        new SubMessage( "Shadow ", SColor.WolfGrey),
        new SubMessage( " spread over the floor ", SColor.White)
        )
      )
    )
    game.monsters.foreach( m => {
      m.takeDamage(40)
    }
    )
    game.players(id).inventory.contents(pos_in_inventory) = null
    game.players(id).inventory.amount(pos_in_inventory) = 0
    return true
  }
}

 
object ItemFactory{
  var tier1: List[String] = List()
  var tier2: List[String] = List()
  var tier3: List[String] = List()
  tier1 = "LightningScroll"::tier1
  tier1 = "TeleportScroll"::tier1
  tier1 = "HealingPotion"::tier1
  tier1 = "HealingPotion"::tier1
  tier1 = "HealingPotion"::tier1
  tier1 = "HealingPotion"::tier1
  tier1 = "HealingPotion"::tier1
  tier1 = "HealingPotion"::tier1
  tier1 = "HealingPotion"::tier1
  tier1 = "HealingPotion"::tier1
  tier1 = "FireScroll"::tier1
  tier2 = "HealingPotionP"::tier2
  tier3 = "HealingPotionX"::tier3
  
  def spawnItemTier1():Item = {
    val r = scala.util.Random
    return Class.forName("rogue."+tier1(r.nextInt(tier1.length))).newInstance().asInstanceOf[Item] 
  }
  def spawnItemTier2():Item = {
    val r = scala.util.Random
    return Class.forName("rogue."+tier2(r.nextInt(tier2.length))).newInstance().asInstanceOf[Item] 
  }
  def spawnItemTier3():Item = {
    val r = scala.util.Random
    return Class.forName("rogue."+tier3(r.nextInt(tier3.length))).newInstance().asInstanceOf[Item] 
  }
  def spawnItem(tier: Int):Item = {
    tier match {
      case 1 => return spawnItemTier1
      case 2 => return spawnItemTier2
      case 3 => return spawnItemTier3
    }
  }
}

// really complicated item
class Trophy (arg_pos: Position, arg_floor: Int) extends Item (
  arg_pos, arg_floor,
  new Sprite( Array[SubSprite]( new SubSprite(294,SColor.Yellow), new SubSprite(295, SColor.LeatherBrown) ),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "trophy",
  SColor.Yellow,
  0, false){
  override def pickUp(game: GameObject ){
    // count the three rocks
    var hasRocks = 0
    if(game.players(0).emerald){
      hasRocks += 1
    }
    if(game.players(0).ruby){
      hasRocks += 1
    }
    if(game.players(0).sapphire){
      hasRocks += 1
    }
    if(game.players(1).emerald){
      hasRocks += 1
    }
    if(game.players(1).ruby){
      hasRocks += 1
    }
    if(game.players(1).sapphire){
      hasRocks += 1
    }



    if( ((game.players(1).pos == pos && floor == game.players(1).floor) || (game.players(0).pos == pos && floor == game.players(0).floor)) && game.tier3Boss.length == 0 ){
      // if the player doesn't have all rocks, win but with a message informing
      // the player that he missed something
      if( hasRocks < 3){
        Log.addLogMessage( new LogMessage( List(
            new SubMessage( "You missed ", SColor.Red),
            new SubMessage( (3-hasRocks).toString, SColor.White),
            new SubMessage( " thing(s)", SColor.Red)
            )
          )
        )
        game.win = true
      }else{
        // if he has allrocks teleport the hero to the secret floor to fight the true last boss
        val pos = new Position(game.r.nextInt(game.dim.width),game.r.nextInt(game.dim.height))
        while(game.occupied2players(pos,15) == true || game.lineOfSight(game.trueLastBoss(0).pos,pos,15) ){
          pos.x = game.r.nextInt(game.dim.width)
          pos.y = game.r.nextInt(game.dim.height)
        }
        game.players(0).pos = pos
        game.players(0).floor = 15
        val pos2 = game.closestNonOccupied(pos,15)
        game.players(1).pos = pos2
        game.players(1).floor = 15
      }
    }else if (game.tier3Boss.length!= 0 &&  ((game.players(1).pos == pos && floor == game.players(1).floor) || (game.players(0).pos == pos && floor == game.players(0).floor))){
      // refuses to be used if the boss still lives
      Log.addLogMessage( new LogMessage( List(
          new SubMessage( "The boss still lives", SColor.White)
          )
        )
      )
    }else{
     // println("fail")
    }
  }
}

class DownwardStairs (pos: Position, arg_floor: Int) extends Item (
  pos, arg_floor,
  new Sprite( Array[SubSprite]( new SubSprite(25,SColor.Red)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "upward stair",
  SColor.White,
  0, false){
  
  
  override def pickUp(game: GameObject){
    //println(game.player.pos + "  "+ pos + "  " + floor + "  " + game.current_floor)
    if( (game.players(0).pos == pos) && (game.players(0).floor == floor) ){
      if(floor == 4 && game.tier1Boss.length == 0){
        game.players(0).floor+=1
        val stair_pos = game.floors(floor+1).upward_stairs.pos
        game.players(0).pos = game.closestNonOccupied(stair_pos,floor+1)
      }else if(floor == 9 && game.tier2Boss.length == 0){
        game.players(0).floor+=1
        val stair_pos = game.floors(floor+1).upward_stairs.pos
        game.players(0).pos = game.closestNonOccupied(stair_pos,floor+1)
      }else if(floor != 4 && floor != 9){
        game.players(0).floor+=1
        val stair_pos = game.floors(floor+1).upward_stairs.pos
        game.players(0).pos = game.closestNonOccupied(stair_pos,floor+1)
      }else{
      // refuses to be used if the boss still lives
        Log.addLogMessage( new LogMessage( List(
            new SubMessage( "The boss still lives", SColor.White)
          )
          )
        )
      }


    }else if(game.players(1).pos == pos && game.players(1).floor == floor ){
      if(floor == 4 && game.tier1Boss.length == 0){
        game.players(1).floor+=1
        val stair_pos = game.floors(floor+1).upward_stairs.pos
        game.players(1).pos = game.closestNonOccupied(stair_pos,floor+1)
      }else if(floor == 9 && game.tier2Boss.length == 0){
        game.players(1).floor+=1
        val stair_pos = game.floors(floor+1).upward_stairs.pos
        game.players(1).pos = game.closestNonOccupied(stair_pos,floor+1)
      }else if(floor != 4 && floor != 9){
        game.players(1).floor+=1
        val stair_pos = game.floors(floor+1).upward_stairs.pos
        game.players(1).pos = game.closestNonOccupied(stair_pos,floor+1)
      }else {
      // refuses to be used if the boss still lives
        Log.addLogMessage( new LogMessage( List(
            new SubMessage( "The boss still lives", SColor.White)
          )
          )
        )
      }


    }else {
      //println("on descend pas")
    }
  }
}

class UpwardStairs (pos: Position, arg_floor: Int) extends Item (
  pos, arg_floor,
  new Sprite( Array[SubSprite]( new SubSprite(24,SColor.Red)),new Color(0.0f,0.0f,0.0f,0.0f)),
  false,
  "upward stair",
  SColor.White,
  0, false){
  
  override def pickUp(game: GameObject){
    println("c'est moi")
    if(game.players(0).pos == pos && game.players(0).floor == floor ){
      game.players(0).floor-=1
      val stair_pos = game.floors(floor-1).downward_stairs.pos
      game.players(0).pos = game.closestNonOccupied(stair_pos,floor+1)
    }else if(game.players(1).pos == pos && game.players(1).floor == floor ){
      game.players(1).floor-=1
      val stair_pos = game.floors(floor-1).downward_stairs.pos
      game.players(1).pos = game.closestNonOccupied(stair_pos,floor+1)
    }else {
      //println("on monte pas")
    }
  }
}
