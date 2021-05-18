package rogue

import util.control.Breaks._

class Map(dim_arg: Dimension, n_arg: Int){ //define the core elements for a map
  var downward_stairs:Item = null
  var upward_stairs:Item = null
  var dim= dim_arg
  var floor = Array.ofDim[Environment](dim.width,dim.height)
  var hasBeenSeen = Array.ofDim[Int](dim.width,dim.height) 
  val number = n_arg
  
  def getFloor(): Array[Array[Environment]] = {
    return floor
  }
 
  def getSeen(): Array[Array[Int]] = {
    return hasBeenSeen
  }
  
}

class MapAutomata(dim_arg: Dimension, n_arg: Int) extends Map(dim_arg, n_arg){
  var floor2 = Array.ofDim[Environment](dim.width,dim.height) //an array that will contain the information whever a tile is a wall or a ground tile
  var floor3 = Array.ofDim[Int](dim.width,dim.height) //an array that will contain the room repartition of the floor
  var biomeMap = Array.ofDim[Biome](dim.width,dim.height) //an array that will contain the biome repartition of the floor

  val r = scala.util.Random
  val initial_wall_chance = 40

  val birth = 4 // values to help the generation of connected empty tiles, to form rooms
  val death = 3

  def getFloor2(): Array[Array[Int]] = {
    return floor3
  }
  def getBiome(): Array[Array[Biome]] = {
    return biomeMap
  }
  def intToBiome(biomeID:Int): Biome = { // a function that helps choosing randomly a biome
        biomeID match{
        case 0 => return( Lake )
        case 1 => return( Field )
        case 2 => return ( DirtField )
        case _ => return ( DirtField )
        }
  }
  def intToBiomeLastLevel(biomeID:Int): Biome = { // a function that helps choosing randomly a biome
        biomeID match{
        case 0 => return( BloodLake )
        case 1 => return( AshField )
        case 2 => return ( DirtField )
        case _ => return ( DirtField )
        }
  }

  def initBiomeMap () { 
    for (x<- 0 to  (dim.width-1)){
      for (y<- 0 to (dim.height-1)){
        biomeMap(x)(y) = Neutral
      }
    }
  }

  def oneGen():Boolean = {
    var continue = false
    for(x <- 0 to (dim.width-1) ){
      for(y <- 0 to (dim.height-1) ){
        var count1 = 0 //count the number of walls surrounding the tile
        for(i <- -1 to 1){
          for(j <- -1 to 1){
            if(i==0 && j==0){
            }else if(x+i<0 || (x+i)>=dim.width  || (y+j)<0 || (y+j)>=dim.height){
              count1+=1
            }else if(floor(x+i)(y+j)==Granite){
              count1+=1
            }
          }
        }
        if(floor(x)(y)==Granite){ //if a wall is surrounded by empty tiles, it will become an empty tile
          if(count1 < death){
            floor2(x)(y) = Empty
            continue = true // the value has changed, the state is not stationnary and the function will be applied
          }else{
            floor2(x)(y) = Granite
          }
        }else{
          if(count1 > birth){ //if an empty tile is surrounded by walls, it will become a wall
            floor2(x)(y) = Granite
            continue = true
          }else{
            floor2(x)(y) = Empty
          }
        }
      }
    }
    for(x <- 0 to (dim.width-1) ){
      for(y <- 0 to (dim.height-1) ){
          floor(x)(y) = floor2(x)(y) //updates the actual floor
      }
    }
    return continue //returns whever there was a change or not
  }

  def generate() {
    for(x: Int <- 0 to (dim.width-1) ){
      for(y: Int <- 0 to (dim.height-1) ){
        if(r.nextInt(100)<initial_wall_chance){ //randomly place walls accross the floor
          floor(x)(y)=Granite
        }else{
          floor(x)(y)=Empty
        }
      }
    }
    var k = 0
    while(oneGen() && k < 50 ){ //corrects some unwanted floor scheme, but use a counter to ensure the end of the process
      k+=1
    }
    for(y: Int <- 0 to (dim.height-1) ){ //put walls on the border
        floor(0)(y) = Granite
        floor(dim.width-1)(y) = Granite
    }
    for(x: Int <- 0 to (dim.width-1) ){
        floor(x)(0) = Granite
        floor(x)(dim.height-1) = Granite
    }
    for(x<- 0 to (dim.width-1) ){
      for(y: Int <- 0 to (dim.height-1) ){
          floor2(x)(y) = floor(x)(y)
      }
    }
  }

  def pouringRoom(x: Int, y: Int, n: Int):Int = { //tests the connexity with the bucket pour method, 
                                                                    //but also add biomes on the first use, and the room number to the tile
    if(floor3(x)(y)!=0 ){
      return 0
    }else{
        floor3(x)(y) = n
        return 1 + pouringRoom(x+1,y,n)+ pouringRoom(x-1,y,n) + pouringRoom(x,y+1,n) + pouringRoom(x,y-1,n) 
    }  
  }

  def getRooms(free: Int, room_number: Int):Boolean = {
    var i = 1
    var stop = false
    while(i <= (dim.width-2) && !stop ){
      var j = 1
      while(j <= (dim.height-2)  && !stop){ 
        val pour_value = pouringRoom(i,j,room_number) //i a tile is connected to every other tiles, the floor is connected
        if(pour_value!=free && pour_value!=0){ //leave the loop early if the floor is connected
          stop = true
        }
        j += 1
      }
      i += 1
    }
    return !stop // stop correspond to the floor connectivity
  }
  def getAllRooms():Int = {
    for(x <- 0 to (dim.width-1) ){
      for(y <- 0 to (dim.height-1) ){
        floor3(x)(y) = if (floor(x)(y)==Granite) -1 else 0 // initialization of the floor scheme in [floor3]
      }
    }
    var free = 0
    for(x <- 0 to (dim.width-1) ){
      for(y <- 0 to (dim.height-1) ){ 
        if(floor3(x)(y)!= -1){
          free += 1 //free counts the number of free tiles
        }
      }
    }
    var n = 1
    while(!getRooms(free,n)){ //if every tile has not been found yet, looks for new rooms
      n += 1 //number assiociated with the room
      free = 0
      for(x <- 0 to (dim.width-1) ){
        for(y <- 0 to (dim.height-1) ){ 
          if(floor3(x)(y)==0){
            free += 1
          }
        }
      }
    }
    return n //if there is more than one room, the floor is not connected
  }

  def carveOneTunnel(){ //chooses a wall and dig through to make the floor connected
    var tx1 = 0 
    var ty1 = 0 
    var d1 = 1000 
    var tx2 = 0 
    var ty2 = 0
    var alongX = true
    for(x1 <- 0 to (dim.width-1) ){
      for(y1 <- 0 to (dim.height-1) ){
        if(floor3(x1)(y1) == 1 && floor(x1)(y1)==Empty){
          for(x2 <- 0 to (dim.width-1) ){
            for(y2 <- 0 to (dim.height-1) ){
              if((x1!=x2 || y1!=y2) && (x1==x2 || y1==y2) && floor3(x2)(y2)>1) //choses the start and the direction in which the tunnel has to be carved
              {
                var d = 0
                var alX = true
                if(x1==x2){
                  d = scala.math.abs(y1-y2)
                  alX = false
                }else{
                  d = scala.math.abs(x1-x2)
                  alX = true
                }
                if(d < d1){ //looks for the smallest distance
                  tx1 = x1 
                  ty1 = y1 
                  d1 = d 
                  tx2 = x2 
                  ty2 = y2
                  alongX = alX
                }
              }
            }
          }
        }
      }
    }
    if(tx1==0 && ty1==0){
      return
    }
    
    if(alongX == true){ //replace the walls with empty tiles
      if(tx1 <= tx2){
        for(i <- tx1 to tx2){
          floor(i)(ty1) = Empty
          //biomeMap(i)(ty1) = Temple
          floor3(i)(ty1) = 0
        }
      }else{
        for(i <- tx2 to tx1){
          floor(i)(ty1) = Empty
          //biomeMap(i)(ty1) = Temple
          floor3(i)(ty1) = 0
        }
      }
    }else{
      if(ty1 <= ty2){
        for(j <- ty1 to ty2){
          floor(tx1)(j) = Empty
          //biomeMap(tx1)(j) = Temple
          floor3(tx1)(j) = 0
        }
      }else{
        for(j <- ty2 to ty1){
          floor(tx1)(j) = Empty
          //biomeMap(tx1)(j) = Temple
          floor3(tx1)(j) = 0
        }
      }
    }
  }

  def generateConnexByCarving():Int={ //generates a connected map with the carving method
    generate()
    initBiomeMap()
    val room_count=getAllRooms()
    while(getAllRooms()>1){ //the floor is connected iff there is one room
      carveOneTunnel()
    }
    return room_count
  }

  def pouringBiome(x: Int, y: Int, n: Int, biome: Biome, buffer: Array[Array[Int]]) { //tests the connexity with the bucket pour method, 
                                                                    //but also add biomes on the first use, and the room number to the tile
    //println(n)
    if( floor(x)(y).getBlocking() || n<=0 || buffer(x)(y) != 0 ){
      return 
    }else{
      biomeMap(x)(y) = biome
      buffer(x)(y) = 1
      pouringBiome(x+1,y,n-1, biome, buffer)
      pouringBiome(x-1,y,n-1, biome, buffer)
      pouringBiome(x,y+1,n-1, biome, buffer)
      pouringBiome(x,y-1,n-1, biome, buffer) 
    }  
  }

  
  def applyBiomes(){ //replaces ground tiles with a new biome-specific ground tile
    val r = scala.util.Random
    val n_biomes = r.nextInt(4) + 6
    println("Biomes picked " + n_biomes)
    for( i <- 1 to n_biomes){
      println("Treating " + i)
      val picked_biome = intToBiome(r.nextInt(3))
      val picked_size = picked_biome.minsize + r.nextInt(picked_biome.maxsize-picked_biome.minsize)
      var buffer = Array.ofDim[Int](dim.width,dim.height)
      var x = r.nextInt(dim.width)
      var y = r.nextInt(dim.height)
      while(floor(x)(y).getBlocking()){
        //println(floor(x)(y) + " "+ i)
        x = r.nextInt(dim.width)
        y = r.nextInt(dim.height)
      }
      
      println("Picked size " + picked_size)
      println("Picked biome " + picked_biome)
      pouringBiome(x,y,picked_size, picked_biome, buffer)
      println("Treated " + i)
      
    }
    for (x: Int <- 0 to (dim.width-1)){
      for (y: Int <- 0 to (dim.height-1)){
        if(!floor(x)(y).getBlocking())
          floor(x)(y) = biomeMap(x)(y).getElement()
      }
    }
  }
  def applyLastLevelBiomes(){ //replaces ground tiles with a new biome-specific ground tile
    val r = scala.util.Random
    val n_biomes = r.nextInt(4) + 6
    println("Biomes picked " + n_biomes)
    for( i <- 1 to n_biomes){
      println("Treating " + i)
      val picked_biome = intToBiomeLastLevel(r.nextInt(3))
      val picked_size = picked_biome.minsize + r.nextInt(picked_biome.maxsize-picked_biome.minsize)
      var buffer = Array.ofDim[Int](dim.width,dim.height)
      var x = r.nextInt(dim.width)
      var y = r.nextInt(dim.height)
      while(floor(x)(y).getBlocking()){
        //println(floor(x)(y) + " "+ i)
        x = r.nextInt(dim.width)
        y = r.nextInt(dim.height)
      }
      
      println("Picked size " + picked_size)
      println("Picked biome " + picked_biome)
      pouringBiome(x,y,picked_size, picked_biome, buffer)
      println("Treated " + i)
      
    }
    for (x: Int <- 0 to (dim.width-1)){
      for (y: Int <- 0 to (dim.height-1)){
        if(!floor(x)(y).getBlocking())
          floor(x)(y) = biomeMap(x)(y).getElement()
      }
    }
  }

  generateConnexByCarving()
  if(number > 0){
    applyBiomes()
  }else{
    applyLastLevelBiomes()
  }

}
