package rogue 

import java.awt.{Color,Graphics2D, Graphics}
import org.json._


/*
 * This class is in charge of all the information that defines a game.
 * It has the responsibility to instanciate the player, and the map when created.
 * It also has all of the informations about the monsters and items.
 * It also keeps track of the state of the game.
 */
class GameObject(dim_arg: Dimension, last_floor: Int, n_players: Int) { 
  var dim = dim_arg
  val r = scala.util.Random
  var floors = List[Map]()
  for(i <- 0 to last_floor){
    floors  = (new MapAutomata(dim,i)) ::floors
  }
  var players = Array (new Player(Origin, new Sprite( Array[SubSprite](new SubSprite(2,SColor.Cyan), new SubSprite(1,SColor.Black)) , new Color(1.0f,1.0f,1.0f,0.0f)), true, 50, 85, 5,"Player 1",SColor.Cyan) ,null )
  if (n_players == 2){
    players(1) = new Player(Origin, new Sprite( Array[SubSprite](new SubSprite(2,SColor.Red), new SubSprite(1,SColor.Black)) , new Color(1.0f,1.0f,1.0f,0.0f)), true, 50, 85, 5,"Player 2",SColor.Red)
  }

  def current_floor(): Int = {
    return players(0).floor
  }

  var monsters: List[Monster] = List() 
  var pnjs: List[QuestGiver] = List() 
  var turn: Int = 0
  var items: List[Item] = List()
  var win = false
  var lose = false
  val max_floor = last_floor
  var quests = 3
  var tier1Boss:List[Monster] = List()
  var tier2Boss:List[Monster] = List()
  var tier3Boss:List[Monster] = List()
  var trueLastBoss:List[Monster] = List()
  var selectCallback:Callbackable = null

  def closestNonOccupied(pos: Position, floor: Int): Position = {
    var res: Position = null
    var dist = 10000000
    for(i <- 0 to (dim.width-1)){
      for(j <- 0 to (dim.height-1)){
        val npos = new Position(i,j)
        val dist_tmp = pos.dist(npos)
        if( !occupied2players(new Position(i,j), floor) &&  dist_tmp < dist){
          res = npos
          dist = dist_tmp
        }
      }
    }
    return res
  }
  def closestNonOccupiedSafe(pos: Position, floor: Int): Position = {
    var res: Position = null
    var dist = 10000000
    for(i <- 0 to (dim.width-1)){
      for(j <- 0 to (dim.height-1)){
        val npos = new Position(i,j)
        val dist_tmp = pos.dist(npos)
        if( !occupied2players_safe(new Position(i,j), floor) &&  dist_tmp < dist){
          res = npos
          dist = dist_tmp
        }
      }
    }
    return res
  }

  def to_json(id: Int): JSONObject = {
   
    val map_json = new JSONArray()
    for(i <- 0 to (dim_arg.width-1)){
      for(j <- 0 to (dim_arg.height-1)){
        if( floors(players(id).floor).hasBeenSeen(i)(j) == 0){
          floors(players(id).floor).hasBeenSeen(i)(j) = if (lineOfSight(new Position(i,j), players(id).pos, players(id).floor) ){ 1 } else { 0 }
        }
        if( floors(players(id).floor).hasBeenSeen(i)(j) != 0 ){
          val tile:JSONObject = new JSONObject()
          tile.put("x",i)
          tile.put("y",j)
          tile.put("sprite", floors(players(id).floor).floor(i)(j).to_json())
          tile.put("visible", lineOfSight(players(id).pos, new Position(i,j), players(id).floor ))
          map_json.put(tile)
        }
      }
    }
    val json:JSONObject = new JSONObject()
    json.put("map",map_json)
    json.put("width",dim_arg.width)
    json.put("height",dim_arg.height)
    val players_info = new JSONArray()
    for( i <- 0 to 1){
      val p_json = new JSONObject()
      p_json.put("pos",players(i).pos.to_json)
      p_json.put("sprite",players(i).sprite.to_json())
      p_json.put("health",players(i).health)
      p_json.put("max_health",players(i).max_health)
      p_json.put("floor",players(i).floor)
      p_json.put("inventory",players(i).inventory.to_json())
      if(players(i).floor == players(id).floor &&  lineOfSight( players(i).pos, players(id).pos, players(id).floor)){
        p_json.put("visible",true)
      }else{
        p_json.put("visible",false)
      }
      if(players(i).weapon != null){
        p_json.put("hasWeapon",true)
        p_json.put("weapon_sprite",players(i).weapon.sprite.to_json())
        p_json.put("weapon_damage",players(i).weapon.damage)
      }else{
        p_json.put("hasWeapon",false)
      }
      if(players(i).armor != null){
        p_json.put("hasArmor",true)
        p_json.put("armor_sprite",players(i).armor.sprite.to_json())
        p_json.put("armor_defense",players(i).armor.defense)
      }else{
        p_json.put("hasArmor",false)
      }
      p_json.put("inventory",players(i).inventory.to_json())
      players_info.put(p_json)
    }
    json.put("players_info",players_info)
    val monsters_info = new JSONArray()
    for( i <- 0 to (monsters.length -1)){
      if( (monsters(i).floor == players(id).floor && ( lineOfSight(monsters(i).pos, players(id).pos, players(id).floor) || monsters(i).lastSeenPos!=null))){
        val m_json = new JSONObject()
        if(lineOfSight( monsters(i).pos, players(id).pos, players(id).floor)){
          m_json.put("visible",true)
          m_json.put("pos",monsters(i).pos.to_json)
          m_json.put("sprite",monsters(i).sprite.to_json())
        }else {
          if(monsters(i).lastSeenPos != null){
            m_json.put("visible",false)
            m_json.put("pos",monsters(i).pos.to_json)
            m_json.put("sprite",monsters(i).sprite.to_json())
          }
        }
        m_json.put("name",monsters(i).name.text)
        m_json.put("name_color",monsters(i).name.color)
        monsters_info.put(m_json)
      }
    }
    json.put("monsters_info",monsters_info)

    val pnjs_info = new JSONArray()
    for( i <- 0 to (pnjs.length -1)){
      if( (pnjs(i).floor == players(id).floor && ( lineOfSight(pnjs(i).pos, players(id).pos, players(id).floor) || pnjs(i).lastSeenPos!=null))){
        val m_json = new JSONObject()
        if(lineOfSight( pnjs(i).pos, players(id).pos, players(id).floor)){
          m_json.put("visible",true)
          m_json.put("pos",pnjs(i).pos.to_json)
          m_json.put("sprite",pnjs(i).sprite.to_json())
        }else {
          if(pnjs(i).lastSeenPos != null){
            m_json.put("visible",false)
            m_json.put("pos",pnjs(i).pos.to_json)
            m_json.put("sprite",pnjs(i).sprite.to_json())
          }
        }
        m_json.put("name",pnjs(i).name.text)
        m_json.put("name_color",pnjs(i).name.color)
        pnjs_info.put(m_json)
      }
    }
    json.put("pnjs_info",pnjs_info)

    val items_info = new JSONArray()
    for( i <- 0 to (items.length -1)){
      if( (items(i).floor == players(id).floor && ( lineOfSight(items(i).pos, players(id).pos, players(id).floor) || items(i).lastSeenPos!=null))){
        val m_json = new JSONObject()
        if(lineOfSight( items(i).pos, players(id).pos, players(id).floor)){
          m_json.put("visible",true)
          m_json.put("pos",items(i).pos.to_json)
          m_json.put("sprite",items(i).sprite.to_json())
        }else {
          if(items(i).lastSeenPos != null){
            m_json.put("visible",false)
            m_json.put("pos",items(i).pos.to_json)
            m_json.put("sprite",items(i).sprite.to_json())
          }
        }
        m_json.put("name",items(i).name.text)
        m_json.put("name_color",items(i).name.color)
        items_info.put(m_json)
      }
    }
    json.put("items_info",items_info)
    json.put("max_floor",max_floor)
    
    json.put("log", Log.to_json())
    return json
  

  }

  placeStairs()
  
  placePlayer()
  placePNJs()
  placeMonsters()
  placeItems()
  placeEmerald()
  placeItemSafe(new Trophy(Origin,0),max_floor-1)
  //give the ruby to a random monster
  val chosenMonster = r.nextInt(monsters.length)
  monsters(chosenMonster).held_items = (new Ruby)::monsters(chosenMonster).held_items
  monsters(chosenMonster).ruby = true
  MonsterFactory.spawnTrueLastBoss(this)

  def placeEmerald(){
    //find suitable spot for emerald 
    var pos = new Position(1 + r.nextInt(dim.width-2) ,1 + r.nextInt(dim.height-2))
    var floor = 9
    while(
      !floors(floor).floor(pos.x)(pos.y).getBlocking ||
      !floors(floor).floor(pos.x+1)(pos.y).getBlocking ||
      !floors(floor).floor(pos.x-1)(pos.y).getBlocking ||
      !floors(floor).floor(pos.x)(pos.y-1).getBlocking ||
      !floors(floor).floor(pos.x-1)(pos.y-1).getBlocking ||
      !floors(floor).floor(pos.x+1)(pos.y-1).getBlocking ||
      !floors(floor).floor(pos.x)(pos.y+1).getBlocking ||
      !floors(floor).floor(pos.x-1)(pos.y+1).getBlocking ||
      !floors(floor).floor(pos.x+1)(pos.y+1).getBlocking 
      ){
      pos = new Position(1 + r.nextInt(dim.width-2),1 + r.nextInt(dim.height-2))
      //floor = r.nextInt(max_floor-1)
    }
    var emerald = new Emerald()

    //carve tunnel for the emerald
    floors(floor).floor(pos.x)(pos.y) = Empty 

    var tpos = new Position(0,0)
    var d1 = 100000
    var alongX = true
    for(x1 <- 1 to (dim.width-2) ){
      for(y1 <- 1 to (dim.height-2) ){
        if( !floors(floor).floor(x1)(y1).getBlocking && (x1 != pos.x || y1 != pos.y ) && (x1==pos.x || y1==pos.y ) ){
            var d = 0
            var alX = true
            if(x1==pos.x){
              d = scala.math.abs(y1-pos.y)
              alX = false
            }else{
              d = scala.math.abs(x1-pos.x)
              alX = true
            }
            if(d < d1){ //looks for the smallest distance
              tpos.x = x1 
              tpos.y = y1 
              d1 = d 
              alongX = alX
            }
        }
      }
    }
  
     
    if(alongX == true){ //replace the walls with empty tiles + fake walls
      if(pos.x <= tpos.x){
        for(i <- (pos.x+1) to (tpos.x-1)){
          floors(floor).floor(i)(pos.y) = Empty
          val fakeWall = new FakeWall()
          fakeWall.pos = new Position(i,pos.y)
          fakeWall.floor = floor
          items = fakeWall::items
        }
      }else{
        for(i <- (tpos.x+1) to (pos.x-1)){
          floors(floor).floor(i)(pos.y) = Empty
          val fakeWall = new FakeWall()
          fakeWall.pos = new Position(i,pos.y)
          fakeWall.floor = floor
          items = fakeWall::items
        }
      }
    }else{
      if(pos.y <= tpos.y){
        for(j <- (pos.y+1) to (tpos.y-1)){
          floors(floor).floor(pos.x)(j) = Empty
          val fakeWall = new FakeWall()
          fakeWall.pos = new Position(pos.x,j)
          fakeWall.floor = floor
          items = fakeWall::items
        }
      }else{
        for(j <- (tpos.y+1) to (pos.y-1)){
          floors(floor).floor(pos.x)(j) = Empty
          val fakeWall = new FakeWall()
          fakeWall.pos = new Position(pos.x,j)
          fakeWall.floor = floor
          items = fakeWall::items
        }
      }
    }
    
    
    emerald.pos = pos
    emerald.floor = floor
    items = emerald::items
    //println("emerald "+ floor + " "+ emerald.pos)
  }
  
  //for(i <- 0 to 20){
  //  placeItem(new HealingGoo(Origin))
 //}
  def placeMonsters(){
    for(i <- 0 to max_floor-1){
      if( i%5 != 4){
        // if we are not on a boss level, spawn monsters
        val tier = i/5+1
        var n = 4 + i%5
        
        for(k <- 1 to n){
          val roll = r.nextInt(101)
          if(roll<90)
            placeMonster(MonsterFactory.spawnMonster(tier),i)
        }
      }else{
        // on a boss level, spawn a boss
        if(i/5 == 0)
          MonsterFactory.spawnTier1Boss(this)
        if(i/5 == 1)
          MonsterFactory.spawnTier2Boss(this)
        if(i/5 == 2)
          MonsterFactory.spawnTier3Boss(this)
      }
    }
  }
  // used to test if, by adding a questgiver,
  // we break the map into two parts
  def connex(floor: Int): Boolean = {
    var x1 = 0
    var y1 = 0 
    var x2 = 0
    var y2 = 0
    while(x1 < dim.width){
      while(y1 < dim.height){
        while(x2 < dim.width){
          while(y2 < dim.height){
            if( !floors(floor).floor(x1)(y1).getBlocking && !floors(floor).floor(x2)(y2).getBlocking && a_star_path(new Position(x1,y1),new Position(x2,y2),floor)==null ){
              return false
            }

            y2 += 1
          }
          x2 +=1
        }
        x2 = 0
        y2 = 0
        y1 += 1
      }
      x1 +=1
    }
    return true
  }
  // this method is used to place the quest givers
  def placePNJs(){
    for(i <- 0 to quests-1){
      var roll = r.nextInt(15)
      //roll = 0
      val roll2 = r.nextInt(Quests.quests.length)
      val pnj = Quests.quests(roll2)
      Quests.quests = Quests.quests.take(roll2) ++ Quests.quests.drop(roll2 + 1)
      placePNJ(pnj,roll)
      // if by placing the pnj we break the connexity of the floors
      // we chose a new spot
      while(  !connex(roll) ){
        pnjs = pnjs.tail
        placePNJ(pnj,roll)
      }
      pnj.spawn(this, roll)
    }
  }
  def placeItems(){
    for(i <- 0 to max_floor-1){
      val tier = i/5+1
      var n = 6 + i%5
      for(k <- 1 to n){
        val roll = r.nextInt(101)
        if(roll<90)
          placeItem(ItemFactory.spawnItem(tier),i)
      }
      placeItem(EquipFactory.spawnEquip(tier),i)
      if(r.nextInt(100) < 20)
        placeItem(EquipFactory.spawnEquip(tier),i)
    }
  }
  
  // we place the stairs, except on the bonus floor
  def placeStairs(){
    for(i <- 0 to last_floor-1){
      if(i>0){
        placeUpwardStair(i)
      }
      if(i<last_floor-1){
        placeDownwardStair(i)
      }
    }
  }

  def getCurrentFloor(): Map = {
    return floors(current_floor)
  }
  // This function is used to place the player at a random spot
  def placePlayer(){
    val pos = new Position(r.nextInt(dim.width),r.nextInt(dim.height))
    while(occupied2players(pos,0) == true ){
      pos.x = r.nextInt(dim.width)
      pos.y = r.nextInt(dim.height)
    }
    players(0).pos = pos
    players(0).floor = 0
    val pos2 = closestNonOccupied(pos,0)
    players(1).pos = pos2
    players(1).floor = 0
  }

  // this method is used to place downard stairs, it is never used
  // on the last, and second to last floors
  def placeDownwardStair(floor: Int){
    val pos = new Position(r.nextInt(dim.width),r.nextInt(dim.height))
    while( occupied2players(pos, floor) || occupied_item_safe(pos,floor)){
      pos.x = r.nextInt(dim.width)
      pos.y = r.nextInt(dim.height)
    }
    val stair = new DownwardStairs(pos,floor)
    stair.floor = floor
    floors(floor).floor(pos.x)(pos.y)=Empty
    floors(floor).downward_stairs = stair
    items = stair :: items
  }
  // this method is used to place upward stairs, it is never used
  // on the last, and first floors
  def placeUpwardStair(floor: Int){
    val pos = new Position(r.nextInt(dim.width),r.nextInt(dim.height))
    while( occupied2players(pos, floor) || occupied_item_safe(pos,floor)){
      pos.x = r.nextInt(dim.width)
      pos.y = r.nextInt(dim.height)
    }
    val stair = new UpwardStairs(pos,floor)
    stair.floor = floor
    floors(floor).floor(pos.x)(pos.y)=Empty
    floors(floor).upward_stairs = stair
    items = stair :: items
  }
  // This function is used to place a monster at a random spot where it cannot be seen by the player
  def placeMonster(monster: Monster, floor: Int){
    val pos = new Position(r.nextInt(dim.width),r.nextInt(dim.height))
    while( ( occupied2players(pos, floor)== true || (floor==0 && lineOfSight(pos,players(0).pos,floor)==true) || (floor > 0 && ( floors(floor).upward_stairs != null && lineOfSight(pos,floors(floor).upward_stairs.pos,floor)==true)))){
      pos.x = r.nextInt(dim.width)
      pos.y = r.nextInt(dim.height)
    }
    monster.pos = pos
    monster.floor = floor
    monsters = monster :: monsters
  }
  // This function is used to place a questgiver at a random spot
  def placePNJ(pnj: QuestGiver, floor: Int){
    val pos = new Position(r.nextInt(dim.width),r.nextInt(dim.height))
    while( ( occupied2players(pos, floor)== true || (floor==0 && lineOfSight(pos,players(0).pos,floor)==true) || (floor > 0 && lineOfSight(pos,floors(floor).upward_stairs.pos,floor)==true))){
      pos.x = r.nextInt(dim.width)
      pos.y = r.nextInt(dim.height)
    }
    pnj.pos = pos
    pnj.floor = floor
    pnjs = pnj :: pnjs
  }


  // This function is used to place an item at a random spot
  def placeItem(item: Item, floor: Int){
    val pos = new Position(r.nextInt(dim.width),r.nextInt(dim.height))
    while( occupied_item(pos, floor)== true){
      pos.x = r.nextInt(dim.width)
      pos.y = r.nextInt(dim.height)
    }
    item.pos = pos
    item.floor = floor
    items = item :: items
  }
  // This function is used to place an item at a random spot where no other item lies
  def placeItemSafe(item: Item, floor: Int){
    val pos = new Position(r.nextInt(dim.width),r.nextInt(dim.height))
    while( occupied_item_safe(pos, floor)== true){
      pos.x = r.nextInt(dim.width)
      pos.y = r.nextInt(dim.height)
    }
    item.pos = pos
    item.floor = floor
    items = item :: items
  }
  // This function is a duplicate of placeItem that can be used for debug when 
  // spawning quest items
  def placeItemPNJSafe(item: Item, floor: Int){
    val pos = new Position(r.nextInt(dim.width),r.nextInt(dim.height))
    while( occupied_item(pos, floor)== true){
      pos.x = r.nextInt(dim.width)
      pos.y = r.nextInt(dim.height)
    }
    item.pos = pos
    item.floor = floor
    items = item :: items
  }
  // This function is used to test is a position is already occupied by item
  // , so as to avoid item stacking
  def occupied_item_safe(pos: Position, floor: Int):Boolean = {
    return (floors(floor)).getFloor()(pos.x)(pos.y).getBlocking || items.exists( (itm: Item)=>{
      return (pos == itm.pos && floor == itm.floor )
    })
  }
  // This function is used to test if a position isn't an empty space,
  // and if a questgiver is in this position
  def occupied_item(pos: Position, floor: Int):Boolean = {
    return (floors(floor)).getFloor()(pos.x)(pos.y).getBlocking || pnjs.exists( (pnj: QuestGiver)=>{
      return (pos == pnj.pos && floor == pnj.floor )
    })
  }


  // This is the line of sight function, used to test if two positions can see each other.
  // It uses Bresenham's line algorithme, to trace a line between the points, if it gets interrupted
  // by a wall or other sight blocking entity, then it returns false. Otherwise it returns true.
  def lineOfSight(pos1: Position, pos2: Position, floor: Int ): Boolean = {
    if (pos1 == pos2){
      return true
    }
    val deltaX = scala.math.abs(pos1.x-pos2.x)
    val deltaY = scala.math.abs(pos1.y-pos2.y)
    val signX = if((pos2.x-pos1.x)<0) -1 else 1
    val signY = if((pos2.y-pos1.y)<0) -1 else 1
    var x = pos1.x
    var y = pos1.y
    if(deltaX == deltaY && deltaY == 0){
      return true
    }
    if(deltaX > deltaY){
      var t = deltaY*2-deltaX
      do{
        if(t>=0){
          y += signY
          t -= deltaX*2
        }
        x += signX
        t += deltaY*2
        if(x==pos2.x && y==pos2.y){
          return true
        }
      }while((floors(floor)).getFloor()(x)(y).getTall()==false &&
        items.filter( itm => itm.pos.x == x && itm.pos.y == y && itm.floor == floor && itm.tall == true).length==0 )
        
      return false
    }else{
      var t = deltaX*2-deltaY
      do{
        if(t>=0){
          x += signX
          t -= deltaY*2
        }
        y += signY
        t += deltaX*2
        if(x==pos2.x && y==pos2.y){
          return true
        }
      }while((floors(floor)).getFloor()(x)(y).getTall()==false &&
        items.filter( itm => itm.pos.x == x && itm.pos.y == y && itm.tall == true).length==0 )
      return false
    }
  }

  def between(pos1: Position, pos2: Position, pos3: Position, floor: Int ): Boolean = {
    val deltaX = scala.math.abs(pos1.x-pos2.x)
    val deltaY = scala.math.abs(pos1.y-pos2.y)
    val signX = if((pos2.x-pos1.x)<0) -1 else 1
    val signY = if((pos2.y-pos1.y)<0) -1 else 1
    var x = pos1.x
    var y = pos1.y
    if(deltaX == deltaY && deltaY == 0){
      return true
    }

    if(deltaX > deltaY){
      var t = deltaY*2-deltaX
      do{
        if(t>=0){
          y += signY
          t -= deltaX*2
        }
        x += signX
        t += deltaY*2
        if(x==pos3.x && y == pos3.y){
          return true
        }

        if(x==pos2.x && y==pos2.y){
          return false
        }
      }while((floors(floor)).getFloor()(x)(y).getTall()==false &&
        items.filter( itm => itm.pos.x == x && itm.pos.y == y && itm.floor == floor && itm.tall == true).length==0 )
        
      return false
    }else{
      var t = deltaX*2-deltaY
      do{
        if(t>=0){
          x += signX
          t -= deltaY*2
        }
        y += signY
        t += deltaX*2
        if(x==pos3.x && y==pos3.y){
          return true
        }
        if(x==pos2.x && y==pos2.y){
          return false
        }
      }while((floors(floor)).getFloor()(x)(y).getTall()==false &&
        items.filter( itm => itm.pos.x == x && itm.pos.y == y && itm.tall == true).length==0 )
      return false
    }
  }

  // This is a helper function used to sort PositionPath
  // in the A* algorithm.
  def sortPos(p1: PositionPath, p2: PositionPath): Boolean = {
    return (p1.gcost + p1.hcost) < (p2.gcost + p2.hcost)
  }
  // This is the path finding algorithm, which is an implementation of the A* algorithm.
  def a_star_path(pos1: Position, pos2: Position, floor: Int ): List[Position] = {
    val start_node = new PositionPath(pos1.x,pos1.y,null, 0, 0)
    val end_node = new PositionPath(pos2.x,pos2.y, null, Int.MaxValue, Int.MaxValue)
    var open_set = List(start_node)
    var closed_set = List[PositionPath]()
    var path = List[PositionPath]()
    var i = 0
    while(open_set.length > 0){
      i += 1
      open_set = open_set.sortWith(sortPos)
      var current_node = open_set.head
      open_set = open_set.tail
      closed_set = current_node :: closed_set
      if( current_node == end_node){
        while(current_node != start_node){
          path = current_node :: path
          current_node = current_node.parent
        }
        return path
      }
      for(i <- -1 to 1){
        for(j <- -1 to 1){
          if(i!=0 || j!=0){
            if((!occupied2players(current_node.translate(i,j), floor) || (end_node.x == (current_node.x+i) && end_node.y == (current_node.y+j)) ) && !closed_set.exists((p: PositionPath)=>p.x == (current_node.x+i) && p.y == (current_node.y+j))){
              var neighbour_node = open_set.find(p => p.x == (current_node.x+i) && p.y == (current_node.y+j)).getOrElse(
                new PositionPath(current_node.x + i, current_node.y+j,null,Int.MaxValue,Int.MaxValue)
              )
              var is_in_open_set = open_set.exists((p: PositionPath)=>p.x == (neighbour_node.x) && p.y == (neighbour_node.y))
              var cost = current_node.gcost + heuristic_cost_estimate(current_node,neighbour_node)
              if(cost < neighbour_node.gcost || !is_in_open_set){
                neighbour_node.gcost = cost
                neighbour_node.hcost = heuristic_cost_estimate(neighbour_node, end_node)
                neighbour_node.parent = current_node 
                if(!is_in_open_set){
                  open_set = neighbour_node :: open_set
                }
              }
            }
          }
        }
      }
    }

    return null
  }
  // Cette fonction est l'heuristique utilisée pour le A* algorithme.
  // La version utilisée permet de prendre en compte les diagonales
  def heuristic_cost_estimate(nodeA: Position, nodeB: Position) : Int = {
    val deltaX = scala.math.abs(nodeA.x - nodeB.x);
    val deltaY = scala.math.abs(nodeA.y - nodeB.y);

    if (deltaX > deltaY)
      return 14 * deltaY + 10 * (deltaX - deltaY);
    return 14 * deltaX + 10 * (deltaY - deltaX);
  }

  // This function is used to create a new map
  /*
  def newMap(){
    first_floor = new MapAutomata(dim)
    placePlayer()
  }
  */

  def getPlayer(id: Int):Entity = {
    return players(id)
  }

  // This function is used to know if there is a monster at a given position
  def searchMonster(monsterList: List[Monster], position:Position,floor: Int): Boolean = {
    monsterList match{
      case m::tl=> { 
        return((m.pos==position && floor==m.floor) || searchMonster(tl,position,floor))
      }
      case emptyList => { return(false)}
    }
  }
  
  // This function is used to know if there is a PNJ at a given position
def searchPNJ(pnjList: List[QuestGiver], position: Position, floor: Int): Boolean = {
    pnjList match{
      case pnj::tl => {
        return((pnj.pos == position && pnj.floor == floor) || searchPNJ(tl, position,floor))
      }
      case emptyList => {return(false)}
    }
  }
  // This function is used to know if a position is occupied by
  // The hero 
  // or a monster 
  // or an block environment that is blocking
  // It is used for collisions

  def occupied2players(pos: Position, floor: Int): Boolean = {
    return (floors(floor)).getFloor()(pos.x)(pos.y).getBlocking || (pos==players(0).pos && floor == players(0).floor)  || (pos==players(1).pos && floor == players(1).floor) || searchMonster(monsters,pos,floor) || searchPNJ(pnjs,pos,floor)
  }
  def occupied2players_safe(pos: Position, floor: Int): Boolean = {
    return (floors(floor)).getFloor()(pos.x)(pos.y).getBlocking || (pos==players(0).pos && floor == players(0).floor)  || (pos==players(1).pos && floor == players(1).floor) || searchMonster(monsters,pos,floor) || searchPNJ(pnjs,pos,floor) || items.exists( (itm: Item)=>{
      return (pos == itm.pos && floor == itm.floor )
    })
  }

  def move(id: Int, npos: Position){
    if ( ! occupied2players(npos,players(id).floor) && players(id).move_cd == 0) {
      players(id).pos = npos
      players(id).move_cd = players(id).move_cd_max
    }else{
    }
    players(id).canChangeFloor = true
    players(id).canPickUp = true
  }
  
  // This function is used to make monsters and items interact with the hero
  // It will move items from the ground the hero's inventory if it is not already full
  // and if the hero is "walking" on an item
  def update(t: Int){
    //filter dead monsters and items and pnjs not on ground
    // we remove items that are not on the ground anymore
    // from the list of items
    items = items.filter( itm =>{ 
      itm.on_the_ground == true })
    // We make all monsters do the action that their AI has decided
    //println("monsters n = "+monsters.length)
    monsters.foreach((m: Monster)=> {
      if(m.floor == current_floor){
          //println(m)
          m.processDecision(this)
      }
    }
    )
    //we make monsters drop items when they die
    monsters.foreach( m => if(m.state==State.Dead)
        m.die(this)
    )
    // We filter dead monsters
    monsters = monsters.filter( m => m.state!=State.Dead)
    pnjs = pnjs.filter( m => m.state!=State.Dead)
    tier1Boss = tier1Boss.filter( m => m.state!=State.Dead)
    tier2Boss = tier2Boss.filter( m => m.state!=State.Dead)
    tier3Boss = tier3Boss.filter( m => m.state!=State.Dead)
    trueLastBoss = trueLastBoss.filter( m => m.state!=State.Dead)
    // if the player kills the true last boss
    // it is an instant win
    if(trueLastBoss.length == 0){
      win = true
    } 






    for(player <- players){
      player.update(t)
      player.status.foreach( s => { 
          s.effect(this)
          s.duration-=1
      })

      player.status.filter( s => {s.duration >0})
    }
    for(monster <- monsters){
      monster.update(t)
      if(monster.floor == players(0).floor || monster.floor == players(1).floor){
        monster.own_ia.processDecision(this, monster)
      }
    }
    for(item <- items){
      if(item.floor == players(0).floor || item.floor == players(1).floor){
        item.pickUp(this)
      }
    }
  }
}

