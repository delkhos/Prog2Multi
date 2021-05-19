package rogue
import java.awt.{Color,Graphics2D, Graphics}
import scala.math.abs

abstract class ArtificialIntelligence(){ // definition of the core elements to rule the behavior of the PNJs
  var path = List[Position]()
  def processDecision(game: GameObject, monster: Monster){
  }

  var target: Player = null
  def chooseTarget(pos: Position,floor: Int, game: GameObject): Player = {
    var t1: Player = null
    var t2: Player = null
    if(floor == game.players(0).floor && game.lineOfSight(pos,game.players(0).pos, floor) ){
      t1 = game.players(0)
    }
    if(floor == game.players(1).floor && game.lineOfSight(pos,game.players(1).pos, floor) ){
      t2 = game.players(1)
    }
    if(t1 != null && t2 != null){
      if(pos.dist(t1.pos) < pos.dist(t2.pos)){
        return t1
      }else{
        return t2
      }
    }else if (t1 != null ){
      return t1
    }else if (t2 != null ){
      return t2
    }else {
      return null
    }
  }
  def chooseTargetOmniscient(pos: Position,floor: Int, game: GameObject): Player = {
    var t1: Player = null
    var t2: Player = null
    if(floor == game.players(0).floor){
      t1 = game.players(0)
    }
    if(floor == game.players(1).floor){
      t2 = game.players(1)
    }
    if(t1 != null && t2 != null){
      if(pos.dist(t1.pos) < pos.dist(t2.pos)){
        return t1
      }else{
        return t2
      }
    }else if (t1 != null ){
      return t1
    }else if (t2 != null ){
      return t2
    }else {
      return null
    }
  }


}

// This object serves the purpose of an enumeration
object State{ //definitions to make the code easier to understand
  
  val Dead = -1
  val Idle = 0
  val Chasing = 1
  val Attacking = 2
}



//a simple AI, if the monster sees you, it will try to follow you, until it can attack  
// if the monster can't see you anymore it will go to the last known position of the hero
class IdleChaseIA extends ArtificialIntelligence(){ 
  var lastPlayerSeenPosition: Position = null //it has not seen the player already
  override def processDecision(game: GameObject , monster: Monster){
    if(monster.state != State.Dead){ //test in case the monster is dead in this turn and still is in the memory
      //println(monster + " " + monster.pos.x + " " + monster.pos.y)
      if(target == null){
        target = chooseTarget(monster.pos,monster.floor, game)
      }
      if(target != null && game.lineOfSight(monster.pos,target.pos, game.current_floor)){ //determines if the monster can see the player
        if(abs(monster.pos.x-target.pos.x) <= 1 && abs(monster.pos.y-target.pos.y)<=1){ //determine if the player is within the attack range
          monster.state = State.Attacking
        }else{
          lastPlayerSeenPosition = new Position(target.pos.x, target.pos.y) //updates the known player position if the monster can see it
          if(monster.state == State.Idle){
            // when the monster spots the player, prints a message in the log stating that event
            Log.addLogMessage( new LogMessage( List(
              monster.name , new SubMessage(" spotted ", SColor.White)
                , target.name )
              )
            )
          }
          monster.state = State.Chasing
        }
        }else if(lastPlayerSeenPosition != null && monster.pos== lastPlayerSeenPosition){ //if the monster loses the track of the player, it will idle again
        target = null
        monster.state = State.Idle
      }
      if(monster.state == State.Idle){ //random movement while idling
        var nextPositions = List((1,1),(1,0),(1,-1),(-1,0),(-1,1),(-1,-1),(0,1),(0,-1))
        val r = scala.util.Random
        var i = r.nextInt(nextPositions.length)
        var delta = nextPositions(i)
        while( game.occupied2players(monster.pos.translate(delta._1,delta._2),game.current_floor) && nextPositions.length > 0 ) //look for an unoccupied tile around the monster
        {
          nextPositions = nextPositions.take(i) ++ nextPositions.drop(i + 1)
          if(nextPositions.length > 0){
            i = r.nextInt(nextPositions.length)
            delta = nextPositions(i)
          }
        }
        if(nextPositions.length == 0){//if the monster can not move,it does nothing
      
        }else{
          if(monster.move_cd == 0){
            monster.pos = monster.pos.translate(delta._1, delta._2) // else it moves
            monster.move_cd = monster.move_cd_max
          }
        }
      }else if(monster.state==State.Chasing){
        var path = game.a_star_path(monster.pos, lastPlayerSeenPosition,game.current_floor) //follow the player, using an A* algorithm, see game_object file
        if(path!=null && monster.move_cd == 0) {
          monster.pos = path(0)
          monster.move_cd = monster.move_cd_max
        }
      }else if(monster.state==State.Attacking){ //the monster attack the player and then turn into a non-attacking state, to avoid unwanted attack with turn-based mechanics
        if(  monster.attack_cd == 0 ){
          monster.attack(target)
          monster.attack_cd = monster.attack_cd_max
        }
        monster.state=State.Chasing
      }
    }
  }
}

class PassiveIA extends ArtificialIntelligence(){ 
  var lastPlayerSeenPosition: Position = null //it has not seen the player already
  override def processDecision(game: GameObject , monster: Monster){
    if(monster.state != State.Dead){ //test in case the monster is dead in this turn and still is in the memory
      var nextPositions = List((1,1),(1,0),(1,-1),(-1,0),(-1,1),(-1,-1),(0,1),(0,-1))
      val r = scala.util.Random
      var i = r.nextInt(nextPositions.length)
      var delta = nextPositions(i)
      while( game.occupied2players(monster.pos.translate(delta._1,delta._2),game.current_floor) && nextPositions.length > 0 ) //look for an unoccupied tile around the monster
      {
        nextPositions = nextPositions.take(i) ++ nextPositions.drop(i + 1)
        if(nextPositions.length > 0){
          i = r.nextInt(nextPositions.length)
          delta = nextPositions(i)
        }
      }
      if(nextPositions.length == 0){//if the monster can not move,it does nothing
    
      }else{
        if(monster.move_cd == 0){
          monster.pos = monster.pos.translate(delta._1, delta._2) // else it moves
          monster.move_cd = monster.move_cd_max
        }
      }
    }
  }
}



class HunterIA extends ArtificialIntelligence(){ 
  var lastPlayerSeenPosition: Position = null //it has not seen the player already
  override def processDecision(game: GameObject , monster: Monster){
    if(monster.state != State.Dead){ //test in case the monster is dead in this turn and still is in the memory
      //println(monster + " " + monster.pos.x + " " + monster.pos.y)
      if(target == null){
        target = chooseTargetOmniscient(monster.pos,monster.floor, game)
      }
      if(target != null ){ //determines if the monster can see the player
        if(abs(monster.pos.x-target.pos.x) <= 1 && abs(monster.pos.y-target.pos.y)<=1){ //determine if the player is within the attack range
          monster.state = State.Attacking
        }else{
          lastPlayerSeenPosition = new Position(target.pos.x, target.pos.y) //updates the known player position if the monster can see it
          if(monster.state == State.Idle){
            // when the monster spots the player, prints a message in the log stating that event
            Log.addLogMessage( new LogMessage( List(
              monster.name , new SubMessage(" spotted ", SColor.White)
                , target.name )
              )
            )
          }
          monster.state = State.Chasing
        }
        }else if( target.floor != monster.floor){ //if the monster loses the track of the player, it will idle again
        target = null
        monster.state = State.Idle
      }
      if(monster.state == State.Idle){ //random movement while idling
        var nextPositions = List((1,1),(1,0),(1,-1),(-1,0),(-1,1),(-1,-1),(0,1),(0,-1))
        val r = scala.util.Random
        var i = r.nextInt(nextPositions.length)
        var delta = nextPositions(i)
        while( game.occupied2players(monster.pos.translate(delta._1,delta._2),game.current_floor) && nextPositions.length > 0) //look for an unoccupied tile around the monster
        {
          nextPositions = nextPositions.take(i) ++ nextPositions.drop(i + 1)
          if(nextPositions.length > 0){
            i = r.nextInt(nextPositions.length)
            delta = nextPositions(i)
          }
        }
        if(nextPositions.length == 0){//if the monster can not move,it does nothing
      
        }else{
          if(monster.move_cd == 0){
            monster.pos = monster.pos.translate(delta._1, delta._2) // else it moves
            monster.move_cd = monster.move_cd_max
          }
        }
      }else if(monster.state==State.Chasing && monster.move_cd == 0){
        var path = game.a_star_path(monster.pos, lastPlayerSeenPosition,game.current_floor) //follow the player, using an A* algorithm, see game_object file
        if(path!=null){
          monster.pos = path(0)
          monster.move_cd = monster.move_cd_max
        }
        }else if(monster.state==State.Attacking && monster.attack_cd == 0){ //the monster attack the player and then turn into a non-attacking state, to avoid unwanted attack with turn-based mechanics
          if( monster.attack_cd == 0 ){
            monster.attack(target)
            monster.attack_cd = monster.attack_cd_max
          }
        monster.state=State.Chasing
      }
    }
  }
}

class SerpentIA extends ArtificialIntelligence(){ 
  var lastPlayerSeenPosition: Position = null //it has not seen the player already
  var cooldown = 0
  var duration = 500


  override def processDecision(game: GameObject , monster: Monster){
    if(monster.state != State.Dead){ //test in case the monster is dead in this turn and still is in the memory
      //println(monster + " " + monster.pos.x + " " + monster.pos.y)
      if(target == null){
        target = chooseTarget(monster.pos,monster.floor, game)
      }
      if(target != null && game.lineOfSight(monster.pos,target.pos, game.current_floor)){ //determines if the monster can see the player
        if(abs(monster.pos.x-target.pos.x) <= 1 && abs(monster.pos.y-target.pos.y)<=1){ //determine if the player is within the attack range
          monster.state = State.Attacking
        }else{
          lastPlayerSeenPosition = new Position(target.pos.x, target.pos.y) //updates the known player position if the monster can see it
          if(monster.state == State.Idle){
            // when the monster spots the player, prints a message in the log stating that event
            Log.addLogMessage( new LogMessage( List(
              monster.name , new SubMessage(" spotted ", SColor.White)
                , target.name )
              )
            )
          }
          monster.state = State.Chasing
        }
        }else if(lastPlayerSeenPosition != null && monster.pos== lastPlayerSeenPosition){ //if the monster loses the track of the player, it will idle again
        target = null
        monster.state = State.Idle
      }
      if(monster.state == State.Idle){ //random movement while idling
        var nextPositions = List((1,1),(1,0),(1,-1),(-1,0),(-1,1),(-1,-1),(0,1),(0,-1))
        val r = scala.util.Random
        var i = r.nextInt(nextPositions.length)
        var delta = nextPositions(i)
        while( game.occupied2players(monster.pos.translate(delta._1,delta._2),game.current_floor) && nextPositions.length > 0 ) //look for an unoccupied tile around the monster
        {
          nextPositions = nextPositions.take(i) ++ nextPositions.drop(i + 1)
          if(nextPositions.length > 0){
            i = r.nextInt(nextPositions.length)
            delta = nextPositions(i)
          }
        }
        if(nextPositions.length == 0){//if the monster can not move,it does nothing
      
        }else{
          if(monster.move_cd == 0){
            monster.pos = monster.pos.translate(delta._1, delta._2) // else it moves
            monster.move_cd = monster.move_cd_max
          }
        }
      }else if(monster.state==State.Chasing){
        var path = game.a_star_path(monster.pos, lastPlayerSeenPosition,game.current_floor) //follow the player, using an A* algorithm, see game_object file
        if(path!=null && monster.move_cd == 0) {
          monster.pos = path(0)
          monster.move_cd = monster.move_cd_max
        }
      }else if(monster.state==State.Attacking){ //the monster attack the player and then turn into a non-attacking state, to avoid unwanted attack with turn-based mechanics
        if(  monster.attack_cd == 0 ){
          if (cooldown != 0){
            monster.attack(target)
            monster.attack_cd = monster.attack_cd_max
          }else{
            target.status = (new Poison(new Sprite(Array[SubSprite](new SubSprite(299,SColor.BatPurple)), new Color((1.0).toFloat,(1.0).toFloat,(1.0).toFloat,(0.0).toFloat)),    duration,target))::target.status                     
             Log.addLogMessage( new LogMessage( List(
               monster.name , new SubMessage(" poisoned ", SColor.BatPurple)
                 , target.name 
               )
              )
             )
          }
        }
        monster.state=State.Chasing
        cooldown += 1
        cooldown %= 3
      }
    }
  }
}

class DragonIA extends ArtificialIntelligence(){ 
  var cooldown = 0 
  var targetedPos: Position = null
  var lastPlayerSeenPosition: Position = null //it has not seen the player already
  override def processDecision(game: GameObject , monster: Monster){
    if(monster.state != State.Dead){ //test in case the monster is dead in this turn and still is in the memory
      //println(monster + " " + monster.pos.x + " " + monster.pos.y)
      if(target == null){
        target = chooseTarget(monster.pos,monster.floor, game)
      }
      if(target != null && game.lineOfSight(monster.pos,target.pos, game.current_floor)){ //determines if the monster can see the player
        lastPlayerSeenPosition = new Position(target.pos.x, target.pos.y) //updates the known player position if the monster can see it
        if(monster.state == State.Idle){
          // when the monster spots the player, prints a message in the log stating that event
          Log.addLogMessage( new LogMessage( List(
            monster.name , new SubMessage(" spotted ", SColor.White)
              , target.name )
            )
          )
          monster.state = State.Attacking
        }
      
      }else if(lastPlayerSeenPosition != null){ //if the monster loses the track of the player, it will idle again
        target = null
        monster.state = State.Idle
      }
      if(monster.state == State.Idle){ //random movement while idling     
      }else if(monster.state==State.Chasing){
      }else if(monster.state==State.Attacking){ //the monster attack the player and then turn into a non-attacking state, to avoid unwanted attack with turn-based mechanics
        if(  monster.attack_cd == 0 ){
          if (cooldown == 0){
            targetedPos =  new Position(target.pos.x, target.pos.y) 
            monster.attack_cd = monster.attack_cd_max

          }else{
            game.players.foreach( player =>  
                if (game.between(monster.pos,targetedPos, player.pos,game.current_floor) || game.between(monster.pos, player.pos, targetedPos, game.current_floor)){
                  monster.attack(player)
                }
                )
            monster.attack_cd = monster.attack_cd_max

          } 
        }
        
        cooldown += 1
        cooldown %= 2
      }
    }
  }
}


/*

class DragonIA extends ArtificialIntelligence() {
  var lastPlayerSeenPosition: Position = null
  var target: Position = null
  var turnCount: Int = 0
  var turnMem: Int = 0
  override def processDecision(game: GameObject , monster: Monster){
    turnCount += 1
    turnCount %= 2
    if(monster.state != State.Dead){
      if(game.lineOfSight(monster.pos,game.player.pos, game.current_floor)){
          monster.state = State.Attacking
          lastPlayerSeenPosition = new Position(game.player.pos.x, game.player.pos.y)
          if(monster.state == State.Idle){
            Log.addLogMessage( new LogMessage( List(
              monster.name , new SubMessage(" spotted ", SColor.White)
                , game.player.name )
              )
            )
          }
        }else if(lastPlayerSeenPosition != null && monster.pos== lastPlayerSeenPosition){
        monster.state = State.Idle
      }
      if(monster.state == State.Idle){
      }
      else if(monster.state==State.Attacking){
        if (turnMem == 0){
          target = new Position(game.player.pos.x, game.player.pos.y)
        }
        if (turnMem == 1){
          if (game.between(monster.pos,target, game.player.pos,game.current_floor) || game.between(monster.pos, game.player.pos, target, game.current_floor)){
            monster.attack(game.player)
          }
        }
        turnMem+=1
        turnMem%=2
      }
    }
  }
}
*/


