package rogue

import java.awt.{Color,Graphics2D, Graphics}
import org.json._

// This class is in charge of all the rendering to our GamePanel
// It posesses a tileset_handler. 
class Renderer {
  val tileset_handler = new TileSetHandler(16, "src/main/resources/spritetable.png")
  def getTileSize():Int = {
    return tileset_handler.getSize()
  }

  def initIsOk(): Boolean = {
    return tileset_handler.isReady()
  }
  def paintBackground(g: Graphics2D, pos: Position, bg: Color, current_size: Int, dpos: DPosition){
    val size:Float = current_size
    val sx1:Float = pos.x*size+dpos.x // -px 
    val sy1:Float = pos.y*size+dpos.y // -py 
    val sx2:Float = sx1 + (size)
    val sy2:Float = sy1 + (size)
    g.setColor(bg);
    g.fillRect(sx1.toInt,sy1.toInt, current_size, current_size)
  }
  def paintBackgroundGray(g: Graphics2D, pos: Position, bg: Color, current_size: Int, dpos: DPosition, ratio: Double){
    val size:Float = current_size
    val sx1:Float = pos.x*size+dpos.x // -px 
    val sy1:Float = pos.y*size+dpos.y // -py 
    val sx2:Float = sx1 + (size)
    val sy2:Float = sy1 + (size)
    g.setColor(new Color( ((bg.getRed() * ratio) / 255.0).toFloat , ((bg.getGreen() * ratio)/255.0).toFloat, ((bg.getBlue() * ratio)/ 255.0).toFloat, (bg.getAlpha()/255.0).toFloat   ));
    g.fillRect(sx1.toInt,sy1.toInt, current_size, current_size)
  }

  // We have drawing functions that allows use to print 
  // elements of our tileset onto the screen on a grid.
  // We also allow a displacement, used for the darring of the map
  def paintCharacter(g: Graphics2D,c: Int, pos: Position, fg: String, current_size: Int, dpos: DPosition){
    val x:Int = c % 16 
    val y:Int = c / 16 
    val dx1:Int = x*tileset_handler.getSize() 
    val dy1:Int = y*tileset_handler.getSize()
    val dx2:Int = dx1+(tileset_handler.getSize())
    val dy2:Int = dy1+(tileset_handler.getSize())
    val size:Float = current_size
    val sx1:Float = pos.x*size+dpos.x // -px 
    val sy1:Float = pos.y*size+dpos.y // -py 
    val sx2:Float = sx1 + (size)
    val sy2:Float = sy1 + (size)
    g.drawImage(tileset_handler.getColoredTileset(fg), sx1.toInt, sy1.toInt, sx2.toInt, sy2.toInt, dx1, dy1, dx2, dy2, null)
  }
  def paintCharacterGray(g: Graphics2D,c: Int, pos: Position, fg: String, current_size: Int, ratio: Double, dpos: DPosition){
    val x:Int = c % 16 
    val y:Int = c / 16 
    val dx1:Int = x*tileset_handler.getSize() 
    val dy1:Int = y*tileset_handler.getSize()
    val dx2:Int = dx1+(tileset_handler.getSize())
    val dy2:Int = dy1+(tileset_handler.getSize())
    val size:Float = current_size
    val sx1:Float = pos.x*size+dpos.x // -px 
    val sy1:Float = pos.y*size+dpos.y // -py 
    val sx2:Float = sx1 + (size)
    val sy2:Float = sy1 + (size)
    val red = "%03d".format((fg.substring(0,3).toInt*ratio).toInt)
    val green = "%03d".format((fg.substring(3,6).toInt*ratio).toInt)
    val blue = "%03d".format((fg.substring(6,9).toInt*ratio).toInt)
    g.drawImage(tileset_handler.getColoredTileset(red+green+blue), sx1.toInt, sy1.toInt, sx2.toInt, sy2.toInt, dx1, dy1, dx2, dy2, null)

  }
  def min(m:Int, n: Int): Int = {
    return if (m<=n) m else n
  }

  def drawString(g: Graphics2D, pos: Position, bg: Color, fg: String, current_size:Int,text: String){
    for(i <- 0 to (text.length()-1)){
      paintBackground(g,pos.translate(i,0),bg,current_size,DOrigin)
      paintCharacter(g, (text.charAt(i)).toInt ,pos.translate(i,0),fg,current_size,DOrigin)
    } 
  }
  def printLog(g: Graphics2D, matrix_dim: Dimension, ui_dim: Dimension, current_size: Int, game: JSONObject){
    val log = game.getJSONArray("log")
    val n = min(ui_dim.height - 3, log.length - 1 )
    drawString(g,new Position(2, matrix_dim.height-ui_dim.height), Color.BLACK, SColor.White, current_size, "LOG : ")
    for(i <- 0 to n)
    {
      val m = log.getJSONArray(i)
      for(j <- 0 to (m.length-1)){
        val sm = m.getJSONObject(j)
        if(j==0){
          drawString(g,new Position(1, matrix_dim.height-1-i), Color.BLACK, sm.getString("color"), current_size, sm.getString("text"))
        }else{
          var delta = 0
          for(k <- 0 to (j-1)){
            delta += m.getJSONObject(k).getString("text").length
          }
          drawString(g,new Position(1+delta, matrix_dim.height-1-i), Color.BLACK, sm.getString("color"), current_size, sm.getString("text"))
        }
      }
      
    }
  }

  def clearScreen(g: Graphics2D,matrix_dim:Dimension, current_size:Int){
    g.setColor(Color.BLACK);
    g.fillRect(0,0, matrix_dim.width*current_size, matrix_dim.height*current_size)
  }

  def drawRandomScreen(g: Graphics2D, current_size: Int, matrix_dim: Dimension){
    val r = scala.util.Random
    for(i <- 1 to matrix_dim.width ){
      for(j <- 1 to matrix_dim.height ){
        val c = r.nextInt(256)
        val red = "%03d".format(r.nextInt(11) * 25)
        val green = "%03d".format(r.nextInt(11) * 25)
        val blue = "%03d".format(r.nextInt(11) *25)
        paintCharacter(g, c , new Position(i,j), red+green+blue, current_size, DOrigin)
      }
    }
  }

  def drawSpriteJSON(g: Graphics2D, current_size: Int, pos: Position, sprite: JSONObject, dpos: DPosition, visible: Boolean){

    val subsprites = sprite.getJSONArray("subsprites")
    var ratio = 1.0
    if(!visible){
      ratio = 0.5
    }
      paintBackgroundGray( g, pos, new Color(sprite.getInt("red") , sprite.getInt("green"), sprite.getInt("blue"), sprite.getInt("alpha")  ) , current_size, dpos , ratio  )
    for ( j <- 0 to (subsprites.length-1)){
      val subsprite = subsprites.getJSONObject(j)
      paintCharacterGray(g,subsprite.getInt("charcode"), pos, subsprite.getString("color"), current_size, ratio, dpos)
    }
  }
  def drawPlayers(g: Graphics2D, current_size: Int, game: JSONObject, dpos: DPosition){
    val players = game.getJSONArray("players_info")
    for(i <- 0 to (players.length -1)){
      val player = players.getJSONObject(i)
      val visible: Boolean = player.getBoolean("visible")
      if (visible){
        val pos = player.getJSONObject("pos")
        val x = pos.getInt("x")
        val y = pos.getInt("y")
        val sprite = player.getJSONObject("sprite")
        drawSpriteJSON(g, current_size , new Position(x,y) ,sprite, dpos, visible)
      }
    }
  }
  def drawMonstersAndPNJs(g: Graphics2D, current_size: Int, game: JSONObject, dpos: DPosition){
    val monsters = game.getJSONArray("monsters_info")
    for(i <- 0 to (monsters.length -1)){
      val monster = monsters.getJSONObject(i)
      val visible: Boolean = monster.getBoolean("visible")
      val pos = monster.getJSONObject("pos")
      val x = pos.getInt("x")
      val y = pos.getInt("y")
      val sprite = monster.getJSONObject("sprite")
      drawSpriteJSON(g, current_size , new Position(x,y) ,sprite, dpos, visible)
    }
    val pnjs = game.getJSONArray("pnjs_info")
    for(i <- 0 to (pnjs.length -1)){
      val pnj = pnjs.getJSONObject(i)
      val visible: Boolean = pnj.getBoolean("visible")
      val pos = pnj.getJSONObject("pos")
      val x = pos.getInt("x")
      val y = pos.getInt("y")
      val sprite = pnj.getJSONObject("sprite")
      drawSpriteJSON(g, current_size , new Position(x,y) ,sprite, dpos, visible)
    }
  }
  def drawItems(g: Graphics2D, current_size: Int, game: JSONObject, dpos: DPosition){
    val items = game.getJSONArray("items_info")
    for(i <- 0 to (items.length -1)){
      val item = items.getJSONObject(i)
      val visible: Boolean = item.getBoolean("visible")
      val pos = item.getJSONObject("pos")
      val x = pos.getInt("x")
      val y = pos.getInt("y")
      val sprite = item.getJSONObject("sprite")
      drawSpriteJSON(g, current_size , new Position(x,y) ,sprite, dpos, visible)
    }
  }

  def drawMapJSON(g: Graphics2D, current_size: Int, game: JSONObject, dpos: DPosition){
    val map = game.getJSONArray("map")
    for ( i <- 0 to (map.length-1)){
      val x = map.getJSONObject(i).getInt("x")
      val y = map.getJSONObject(i).getInt("y")
      val visible = map.getJSONObject(i).getBoolean("visible")
      val pos = new Position(x,y)

      val sprite = map.getJSONObject(i).getJSONObject("sprite") 
      drawSpriteJSON(g, current_size , pos,sprite, dpos, visible)

    }
  }

  def drawStatusJSON(g: Graphics2D, matrix_dim: Dimension, ui_dim: Dimension, game: JSONObject){
    val id = game.getInt("id")
    val players = game.getJSONArray("players_info")
    val player = players.getJSONObject(id)
    val statuses = player.getJSONArray("status")
    drawString(g,new Position( matrix_dim.width-ui_dim.width, 7), Color.BLACK  , SColor.White, tileset_handler.getSize(), "STATUSES:")
    for(i <- 0 to (statuses.length-1)){
      val sprite = statuses.getJSONObject(i)
      drawSpriteJSON(g, tileset_handler.getSize(), new Position( matrix_dim.width-ui_dim.width + 2*i, 8  ), sprite , DOrigin, true)
      
    }
  }

  def drawUIJSON(g: Graphics2D, matrix_dim: Dimension, ui_dim: Dimension, game: JSONObject){
    // draw lines
    val size = tileset_handler.getSize()
    g.setColor(Color.BLACK);
    g.fillRect((matrix_dim.width-ui_dim.width-1)*size,0, matrix_dim.width*size, matrix_dim.height*size)
    g.fillRect(0,(matrix_dim.height-ui_dim.height-1)*size, matrix_dim.width*size, matrix_dim.height*size)
    for( j <- 0 to (matrix_dim.height-ui_dim.height-2) ){
      paintCharacter(g, 179, new Position(matrix_dim.width-ui_dim.width-1, j),  "000050200", tileset_handler.getSize(), DOrigin)
    }
    for( j <- 0 to (matrix_dim.width-ui_dim.width-2) ){
      paintCharacter(g, 196, new Position( j, matrix_dim.height-ui_dim.height-1), "000050200", tileset_handler.getSize(), DOrigin)
    }
    paintCharacter(g, 217, new Position(matrix_dim.width-ui_dim.width-1, matrix_dim.height-ui_dim.height-1), "000050200", tileset_handler.getSize() , DOrigin)

    val players = game.getJSONArray("players_info")
    val id = game.getInt("id")
    val r_id = 1+id
    // draw name
    drawString(g,new Position( matrix_dim.width-ui_dim.width, 0) , Color.BLACK, SColor.White, tileset_handler.getSize(),"YOU ARE : P"+r_id)    
    
    // draw hps
    drawString(g,new Position( matrix_dim.width-ui_dim.width, 3), Color.BLACK , SColor.White, tileset_handler.getSize(), ("P1 HP:"+players.getJSONObject(0).getInt("health")+"/"+ players.getJSONObject(0).getInt("max_health")) )    
    drawString(g,new Position( matrix_dim.width-ui_dim.width, 4), Color.BLACK  , SColor.White, tileset_handler.getSize(), ("P2 HP:"+players.getJSONObject(1).getInt("health")+"/"+ players.getJSONObject(1).getInt("max_health")) )    
    // draw floor
    drawString(g,new Position( matrix_dim.width-ui_dim.width, 2), Color.BLACK  , SColor.White, tileset_handler.getSize(), ("Floor:"+players.getJSONObject(id).getInt("floor")+"/"+(game.getInt("max_floor")-1)))

    val player = players.getJSONObject(id)
    val inventory = player.getJSONArray("inventory")

    // draw inventory
    val istart = (matrix_dim.width-ui_dim.width)
    val iend = (matrix_dim.width-1)
    val jstart = (matrix_dim.height-ui_dim.height-1-(inventory.length/(ui_dim.width-2)+1)-1)-1
    val jend = (matrix_dim.height-ui_dim.height-1)-1
    drawString(g,new Position(istart, jstart-1) , Color.BLACK, SColor.White, tileset_handler.getSize(),"Inventory : ")    
    // draw armor
    drawString(g,new Position(istart, jstart-10) , Color.BLACK, SColor.White, tileset_handler.getSize(),"Armor : ")    
    if(player.getBoolean("hasArmor")){
      drawSpriteJSON(g, tileset_handler.getSize(), new Position(istart+10,jstart-10), player.getJSONObject("armor_sprite"), DOrigin, true)
      drawString(g,new Position(istart+12, jstart-10) , Color.BLACK, SColor.White, tileset_handler.getSize(),"+"+player.getInt("armor_defense"))    
    }

    // draw weapon
    drawString(g,new Position(istart, jstart-9) , Color.BLACK, SColor.White, tileset_handler.getSize(),"Weapon : ")    
    if(player.getBoolean("hasWeapon")){
      drawSpriteJSON(g, tileset_handler.getSize(), new Position(istart+10,jstart-9), player.getJSONObject("weapon_sprite"), DOrigin, true)
      drawString(g,new Position(istart+12, jstart-9) , Color.BLACK, SColor.White, tileset_handler.getSize(),"+"+player.getInt("weapon_damage"))    
    }
    for(i <- istart  to iend){
      for(j <- jstart to jend ){
        val inventory_coord = i-(istart+1)+(j-(jstart+1))*(iend-istart-1)
        if(i== istart && j == jstart){
          paintCharacter(g,214, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(i==iend && j==jstart){
          paintCharacter(g,183, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(i==iend && j==jend){
          paintCharacter(g,189, new Position(i, j), "230230230", tileset_handler.getSize() , DOrigin)
        }else if(i==istart && j==jend){
          paintCharacter(g,211, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(i==iend || i==istart){
          paintCharacter(g,186, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(j==jend || j==jstart){
          paintCharacter(g,196, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(inventory_coord >=0 && inventory_coord < inventory.length && inventory.getJSONObject(inventory_coord).getBoolean("empty") != true ) {
          val item = inventory.getJSONObject(inventory_coord)
          // draw item
          drawSpriteJSON(g, tileset_handler.getSize(), new Position(i,j), item.getJSONObject("sprite"), DOrigin, true)
          if(item.getBoolean("stackable"))
            paintCharacter(g,item.getInt("amount") +48, new Position(i, j+2), "230230230", tileset_handler.getSize() , DOrigin)
        }else if(inventory_coord >=0 && inventory_coord < inventory.length && inventory.getJSONObject(inventory_coord).getBoolean("empty") == true) {
          // draw a grey box if the slot is free
          paintCharacter(g,('Z').toInt + 1, new Position(i, j), "120120120", tileset_handler.getSize() , DOrigin)
          paintCharacter(g,('Z').toInt + 3, new Position(i, j), "120120120", tileset_handler.getSize() , DOrigin)
        }else{
          // draw a white cross if this slot will never be free
          paintCharacter(g,('Z').toInt + 2, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
          paintCharacter(g,('0').toInt - 1, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }
      }
    }
  }

  def getMonsterHover(game: JSONObject, pos_arg: Position) : JSONObject = {
    val monsters = game.getJSONArray("monsters_info")
    for (i <- 0 to (monsters.length -1)){
      val monster = monsters.getJSONObject(i)
      val pos = monster.getJSONObject("pos")
      val x = pos.getInt("x")
      val y = pos.getInt("y")
      val mpos = new Position(x,y)
      if(pos_arg == mpos){
        return monster
      }
    }
    return null
  }
  def getPNJHover(game: JSONObject, pos_arg: Position) : JSONObject = {
    val pnjs = game.getJSONArray("pnjs_info")
    for (i <- 0 to (pnjs.length -1)){
      val pnj = pnjs.getJSONObject(i)
      val pos = pnj.getJSONObject("pos")
      val x = pos.getInt("x")
      val y = pos.getInt("y")
      val mpos = new Position(x,y)
      if(pos_arg==mpos){
        return pnj
      }
    }
    return null
  }
  def getItemsHover(game: JSONObject, pos_arg: Position) : List[JSONObject] = {
    val items = game.getJSONArray("items_info")
    var res = List[JSONObject]()
    for (i <- 0 to (items.length -1)){
      val item = items.getJSONObject(i)
      val pos = item.getJSONObject("pos")
      val x = pos.getInt("x")
      val y = pos.getInt("y")
      val mpos = new Position(x,y)
      if(pos_arg==mpos){
        res = item :: res
      }
    }
    return res
  }

  def drawHover(g: Graphics2D, mousepos: Position, mousepos_absolute: Position, game: JSONObject, matrix_dim: Dimension, ui_dim: Dimension ){
    val id = game.getInt("id")
    val players = game.getJSONArray("players_info")
    val player = players.getJSONObject(id)
    val inventory = player.getJSONArray("inventory")

    val istart = (matrix_dim.width-ui_dim.width) -4
    val iend = (matrix_dim.width-1)
    val jstart = (matrix_dim.height-ui_dim.height-1-(inventory.length/(ui_dim.width-2)+1)-1)+4
    val jend = matrix_dim.height-1
    drawString(g,new Position(istart, jstart-1) , Color.BLACK, SColor.White, tileset_handler.getSize(),"Hover : ")    
    val inventory_coord = mousepos_absolute.x-(istart+1+4)+(mousepos_absolute.y-(jstart+1-5))*(iend-(istart+4)-1)




    if(mousepos.x >=0 && mousepos.y >= 0 && mousepos.x < (matrix_dim.width - ui_dim.width-1) && mousepos.y < (matrix_dim.height- ui_dim.height-1) ){
      val monster = getMonsterHover(game,mousepos) 
      val pnj = getPNJHover(game,mousepos) 
      val items = getItemsHover(game,mousepos) 
      var printHeight = jstart+1
      // if mouse over a monster display its name
      if(monster != null){
        drawString(g,new Position(istart+1, printHeight) , Color.BLACK, monster.getString("name_color"), tileset_handler.getSize(),monster.getString("name") )    
        printHeight += 1
      }
      // if mouse over a npc display its name
      if(pnj != null){
        drawString(g,new Position(istart+1, printHeight) , Color.BLACK, pnj.getString("name_color"), tileset_handler.getSize(),pnj.getString("name") )    
        printHeight += 1
      }
      // if mouse over items display their names
      items.foreach(itm => {
        drawString(g,new Position(istart+1, printHeight) , Color.BLACK, itm.getString("name_color"), tileset_handler.getSize(),itm.getString("name") )    
        printHeight += 1    
      })
    }
    if(inventory_coord>= 0 && inventory_coord < inventory.length && inventory.getJSONObject(inventory_coord).getBoolean("empty") != true && mousepos_absolute.x >= (matrix_dim.width-ui_dim.width-2)){ // the player has clicked on the inventory
      drawString(g,new Position(istart+1, jstart+1) , Color.BLACK, inventory.getJSONObject(inventory_coord).getString("name_color") , tileset_handler.getSize(), inventory.getJSONObject(inventory_coord).getString("name") )    

    }
    for(i <- istart  to iend){
      for(j <- jstart to jend ){
        if(i== istart && j == jstart){
          paintCharacter(g,214, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(i==iend && j==jstart){
          paintCharacter(g,183, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(i==iend && j==jend){
          paintCharacter(g,189, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(i==istart && j==jend){
          paintCharacter(g,211, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(i==iend || i==istart){
          paintCharacter(g,186, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }else if(j==jend || j==jstart){
          paintCharacter(g,196, new Position(i, j),  "230230230", tileset_handler.getSize() , DOrigin)
        }
      }
    }
  }

  def drawGameJSON(g: Graphics2D, current_size: Int, matrix_dim: Dimension, ui_dim: Dimension,game: JSONObject, dpos: DPosition, mousepos: Position, mousepos_absolute: Position){
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_OFF)
    g.setRenderingHint(java.awt.RenderingHints.KEY_ALPHA_INTERPOLATION, java.awt.RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED)
    g.setRenderingHint(java.awt.RenderingHints.KEY_COLOR_RENDERING, java.awt.RenderingHints.VALUE_COLOR_RENDER_SPEED)

    clearScreen(g, matrix_dim, tileset_handler.getSize())
    val state = game.getString("state")
    if(state == "playing" ){
      drawMapJSON(g, current_size, game, dpos)
      drawPlayers(g, current_size, game, dpos)
      drawMonstersAndPNJs(g, current_size, game, dpos)
      drawItems(g, current_size, game, dpos)
      drawUIJSON(g, matrix_dim, ui_dim, game)
      drawStatusJSON(g, matrix_dim, ui_dim, game)
      drawHover(g, mousepos, mousepos_absolute, game, matrix_dim, ui_dim)
      printLog(g, matrix_dim, ui_dim, tileset_handler.getSize(), game)
      // draw ui
      if(mousepos_absolute.x <= (matrix_dim.width-ui_dim.width-2) && mousepos_absolute.y <= (matrix_dim.height-ui_dim.height-2) && INPUTS.selecting ){
        paintCharacter(g,9, mousepos, SColor.Yellow,current_size , dpos)
      }
    }else {
    drawString(g, new Position(matrix_dim.width/2-10,matrix_dim.height/2), new Color(255,255,0,170), SColor.Black, tileset_handler.getSize(), "WAITING FOR PLAYER 2")
    }
  }
  
}
