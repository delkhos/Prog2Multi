package rogue

import java.awt.{Color,Graphics2D, Graphics}

// definition of the montsers, with their properties
class Monster(arg_pos: Position, arg_floor: Int, sprite: Sprite, collidable: Boolean, maxHealth: Int, ia: ArtificialIntelligence, hitChance: Int, hitDamage: Int,name_arg: String,name_color: String, dropEquipChance: Int, dropEquipTier: Int, dropItemChance: Int, dropItemTier: Int) extends LivingEntity(arg_pos, arg_floor ,sprite, collidable, maxHealth, hitChance, hitDamage, name_arg, name_color) {
  var spawn = "Goblin"
  var split1 = "Goblin"
  move_cd_max = 500
  attack_cd_max = 500
  var split2 = "Goblin"
  val own_ia = ia
  var held_items:List[Item] = List()
  def processDecision(game: GameObject){
    own_ia.processDecision(game,this)
  }
  // when the monster dies, drop held items
  // and drop equiments and items according to the chosen tier, with a chosen chance
  def die(game: GameObject){
    val r = scala.util.Random
    val roll1 = r.nextInt(101)
    val roll2 = r.nextInt(101)
    if(roll1 <= dropEquipChance){
      val equip = EquipFactory.spawnEquip(dropEquipTier)
      equip.pos = new Position(pos.x,pos.y)
      game.items = equip::game.items
      equip.floor = floor
    }
    if(roll2 <= dropItemChance){
      val itm = ItemFactory.spawnItem(dropItemTier)
      itm.pos = new Position(pos.x,pos.y)
      game.items = itm::game.items
      itm.floor = floor
    }
    held_items.foreach(
      itm => {
        itm.pos = new Position(pos.x,pos.y)
        game.items = itm::game.items
        itm.floor = floor
      }
    )
  }
}

// creation of some monsters

class Calamity() extends Monster (Origin,0, 
  new Sprite( Array[SubSprite](new SubSprite(64,SColor.GoblinGreen), new SubSprite(235, SColor.Blue)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,100,new IdleChaseIA,75,10,"Calamity",SColor.White,
  0,1,0,1){
  override def die(game: GameObject){
      var m1 = MonsterFactory.spawnMonsterSplit("Malice")
      var tpos = new Position(0,0)
      var d1 = 1000000
      for(x1 <- 1 to (game.dim.width-2) ){
        for(y1 <- 1 to (game.dim.height-2) ){
          if( !game.occupied2players(new Position(x1,y1),15) ){
              var d = (y1-pos.y)*(y1-pos.y) + (x1-pos.x)*(x1-pos.x)
              println(d)
              if(d < d1){ //looks for the smallest distance
                tpos.x = x1 
                tpos.y = y1 
                d1 = d 
              }
          }
        }
      }
      m1.floor = 15
      m1.pos = tpos
      game.trueLastBoss=m1::game.trueLastBoss
      game.monsters = m1::game.monsters
      var m2 = MonsterFactory.spawnMonsterSplit("Malice")
      var tpos2 = new Position(0,0)
      d1 = 1000000
      for(x1 <- 1 to (game.dim.width-2) ){
        for(y1 <- 1 to (game.dim.height-2) ){
          if( !game.occupied2players(new Position(x1,y1),15) ){
              var d = (y1-pos.y)*(y1-pos.y) + (x1-pos.x)*(x1-pos.x)
              if(d < d1){ //looks for the smallest distance
                tpos2.x = x1 
                tpos2.y = y1 
                d1 = d 
              }
          }
        }
      }
      m2.floor = 15
      m2.pos = tpos2
      game.trueLastBoss=m2::game.trueLastBoss
      game.monsters = m2::game.monsters
      game.trueLastBoss.foreach(m=> println(m+" "+m.pos+" "+m.floor))
    }
}

// unused monster but will soon be added to the trueLastBoss fight
class Malice() extends Monster (Origin,0, 
  new Sprite( Array[SubSprite](new SubSprite(9,SColor.GoblinGreen), new SubSprite(7, SColor.Blue)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,50,new IdleChaseIA,75,8,"Malice",SColor.Grey,
  0,1,0,1){
    override def die(game: GameObject){
      var m1 = MonsterFactory.spawnMonsterSplit("Mucus")
      var tpos = new Position(0,0)
      var d1 = 100000
      for(x1 <- 1 to (game.dim.width-2) ){
        for(y1 <- 1 to (game.dim.height-2) ){
          if( !game.occupied2players(new Position(x1,y1),15) ){
              var d = (y1-pos.y)*(y1-pos.y) + (x1-pos.x)*(x1-pos.x)
              if(d < d1){ //looks for the smallest distance
                tpos.x = x1 
                tpos.y = y1 
                d1 = d 
              }
          }
        }
      }
      m1.floor = 15
      m1.pos = tpos
      game.trueLastBoss=m1::game.trueLastBoss
      game.monsters = m1::game.monsters
      var m2 = MonsterFactory.spawnMonsterSplit("Mucus")
      var tpos2 = new Position(0,0)
      d1 = 100000
      for(x1 <- 1 to (game.dim.width-2) ){
        for(y1 <- 1 to (game.dim.height-2) ){
          if( !game.occupied2players(new Position(x1,y1),15) ){
              var d = (y1-pos.y)*(y1-pos.y) + (x1-pos.x)*(x1-pos.x)
              if(d < d1){ //looks for the smallest distance
                tpos2.x = x1 
                tpos2.y = y1 
                d1 = d 
              }
          }
        }
      }
      m2.floor = 15
      m2.pos = tpos2
      game.trueLastBoss=m2::game.trueLastBoss
      game.monsters = m2::game.monsters
    }
}

class Mucus() extends Monster(Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(7,SColor.GoblinGreen)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,20,new IdleChaseIA,75,6,"Mucus",SColor.Green,
  10,1,20,1){}


class Goblin() extends Monster(Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(288,SColor.GoblinGreen), new SubSprite(287, SColor.LeatherBrown)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,20,new IdleChaseIA,75,3,"Goblin",SColor.Green,
  10,1,20,1){}

class Hunter() extends Monster(Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(289,SColor.WolfGrey),new SubSprite(290,SColor.Red)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,25,new HunterIA,85,6,"Hunter",SColor.Grey,
  100, 1, 20, 1){}

class Hoblin() extends Monster(Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(291,SColor.ZwiffyGreen), new SubSprite(292, SColor.LeatherBrown)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,30,new IdleChaseIA,50,7,"Hoblin",SColor.ZwiffyGreen,
  5,1,10,1){}

class Bat() extends Monster(Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(293,SColor.BatPurple)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,14,new IdleChaseIA,90,4,"Bat",SColor.BatPurple,
  5,1, 50, 1){}

class Jormungand() extends Monster(Origin,0,
  new Sprite( Array[SubSprite](new SubSprite(296,SColor.GoblinGreen)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,50, new SerpentIA, 75,6,"Jormungand",SColor.BatPurple,100,3,100,3){}  
 
  

class Vampire() extends Monster(Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(293,SColor.Red)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,40,new IdleChaseIA,75,4,"Vampire",SColor.Red,
  35,2, 35, 2){
    override def attack( attackee: LivingEntity){ 
    val r = scala.util.Random
    var damage = 4
    if(attackee.armor != null){
      damage -= 3*attackee.armor.defense/4
      if (attackee.sapphire){
        damage -= 1
      }
      if(damage < 0)
        damage = 0
    }
    val attack_try = r.nextInt(100)
    if(attack_try <= 75){
      attackee.health -= damage
      if (!attackee.emerald){
        this.health += 1
      }
      if (this.health > 40){
        this.health = 40
      }
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


  }

class GoblinKing() extends Monster(Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(288,SColor.Gold), new SubSprite(287, SColor.LeatherBrown)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,100,new IdleChaseIA,75,6,"Goblin King",SColor.Gold,
  100,2,100,2){}

class Alpha() extends Monster(Origin, 0,
  new Sprite( Array[SubSprite](new SubSprite(289,SColor.BrightBlue),new SubSprite(290,SColor.Red)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),
  true,80,new HunterIA,85,6,"The Alpha",SColor.BrightBlue,
  75, 2, 75, 2
  ){}

class CavernDrake() extends Monster(Origin, 0,
  new Sprite( Array[SubSprite] (new SubSprite(298,SColor.Red)),  new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ),true,125,new DragonIA,90,8,"Dragon",SColor.Red,
  35,2,35,2){}

class Unicorn() extends Monster (Origin, 0,  new Sprite( Array[SubSprite](new SubSprite(301,SColor.White)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat) ), true , 50, new PassiveIA, 0, 0, "Unicorn", SColor.DeepPink, 0, 1, 0, 1){
  held_items = List(new UnicornHorn())
}


object MonsterFactory{
  // these lists, that can be seen
  // as a list of probabilities, handle the item pools
  var tier1: List[String] = List()
  var tier2: List[String] = List()
  var tier3: List[String] = List()
  tier1 = "Hoblin"::tier1
  tier1 = "Hoblin"::tier1
  tier1 = "Hoblin"::tier1
  tier1 = "Hoblin"::tier1
  tier1 = "Hoblin"::tier1
  tier1 = "Hoblin"::tier1
  tier1 = "Hoblin"::tier1
  tier1 = "Hoblin"::tier1
  tier1 = "Bat"::tier1
  tier1 = "Bat"::tier1
  tier1 = "Bat"::tier1
  tier1 = "Bat"::tier1
  tier1 = "Bat"::tier1
  tier1 = "Bat"::tier1
  tier1 = "Bat"::tier1
  tier1 = "Bat"::tier1
  tier1 = "Goblin"::tier1
  tier1 = "Goblin"::tier1
  tier1 = "Goblin"::tier1
  tier1 = "Goblin"::tier1
  tier1 = "Goblin"::tier1
  tier1 = "Goblin"::tier1
  tier1 = "Goblin"::tier1
  tier1 = "Goblin"::tier1
  tier1 = "Unicorn"::tier1
  
  tier2 = "Hunter"::tier2
  tier2 = "Hunter"::tier2
  tier2 = "Hunter"::tier2
  tier2 = "Hunter"::tier2
  tier2 = "Hunter"::tier2
  tier2 = "Hoblin"::tier2
  tier2 = "Hoblin"::tier2
  tier2 = "Hoblin"::tier2
  tier2 = "Goblin"::tier2
  tier2 = "Goblin"::tier2
  tier2 = "Goblin"::tier2
  tier2 = "Goblin"::tier2
  tier2 = "Unicorn"::tier2


  tier3 = "Hunter"::tier3
  tier3 = "Hunter"::tier3
  tier3 = "Hunter"::tier3
  tier3 = "Hunter"::tier3
  tier3 = "Hunter"::tier3
  tier3 = "Alpha"::tier3
  tier3 = "Alpha"::tier3

  def spawnMonsterTier1():Monster = {
    val r = scala.util.Random
    return Class.forName("rogue."+tier1(r.nextInt(tier1.length))).newInstance().asInstanceOf[Monster] 
  }
  def spawnMonsterTier2():Monster = {
    val r = scala.util.Random
    return Class.forName("rogue."+tier2(r.nextInt(tier2.length))).newInstance().asInstanceOf[Monster] 
  }
  def spawnMonsterTier3():Monster = {
    val r = scala.util.Random
    return Class.forName("rogue."+tier3(r.nextInt(tier3.length))).newInstance().asInstanceOf[Monster] 
  }
  def spawnMonster(tier: Int):Monster = {
    tier match {
      case 1 => return spawnMonsterTier1
      case 2 => return spawnMonsterTier2
      case 3 => return spawnMonsterTier3
    }
  }
  
  def spawnMonsterSplit(monsterName: String): Monster = {
  return Class.forName("rogue."+monsterName).newInstance().asInstanceOf[Monster]
  }

  def spawnTier1Boss(game: GameObject) {
    val r = scala.util.Random
    val roll = r.nextInt(2)
    roll match {
      case 1 => {
        val boss1 = new Vampire()
        game.placeMonster(boss1,4)
        game.tier1Boss = boss1::game.tier1Boss
        val boss2 = new Vampire()
        game.placeMonster(boss2,4)
        game.tier1Boss = boss2::game.tier1Boss
        val boss3 = new Vampire()
        game.placeMonster(boss3,4)
        game.tier1Boss = boss3::game.tier1Boss 
      }

      case 0 => {
        val boss = new Alpha()
        game.placeMonster(boss,4)
        game.tier1Boss = boss::game.tier1Boss
      }
    }
  }
  def spawnTier2Boss(game: GameObject) {
    val r = scala.util.Random
    val roll = r.nextInt(2)
    roll match {
      case 0 => {
        val boss = new Jormungand()
        game.placeMonster(boss,9)
        game.tier2Boss = boss::game.tier2Boss
      }
      case 1 => {
        val boss = new GoblinKing()
        game.placeMonster(boss,9)
        game.tier2Boss = boss::game.tier1Boss
      }
    }
  }
  def spawnTier3Boss(game: GameObject) {
    val r = scala.util.Random
    val roll = r.nextInt(1)
    roll match {
      case 0 => {
        val boss = new CavernDrake()
        game.placeMonster(boss,14)
        game.tier3Boss = boss::game.tier3Boss
      }
    }
  }
  def spawnTrueLastBoss(game: GameObject){
    val boss = new Calamity()
    game.placeMonster(boss,15)
    game.trueLastBoss = boss::game.trueLastBoss
  }
  
}
