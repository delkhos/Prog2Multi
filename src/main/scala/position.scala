package rogue 

import org.json._

class Position(arg_x:Int , arg_y: Int){ // definition of the position object, that reunite the x and y coordinates
  var x = arg_x
  var y = arg_y
  def canEqual(a: Any) = a.isInstanceOf[Position]

  def translate(dx: Int, dy: Int): Position = {
    return new Position(x+dx,y+dy)
  }
  
  def adjacent(pos:Position): Boolean = {
    return (scala.math.abs(x-pos.x)<=1 && scala.math.abs(y-pos.y) <=1)
  }

  // override the "=" function to easily compare two positions
  override def equals(that: Any): Boolean =
    that match {
      case that: Position => that.x == x && that.y == y
      case _ => false
  }
  def dist(pos: Position): Int = {
    val dx = (pos.x-x).abs
    val dy = (pos.y-y).abs
    return dx*dx + dy*dy
  }


  def to_json(): JSONObject = {
    val json = new JSONObject()
    json.put("x",x)
    json.put("y",y)
    return json
  }


  override def toString():String ={
    return "x="+x+"  y="+y
  }

}
/*
 * This is the same as a position but, using floats
 */
class DPosition(arg_x:Float , arg_y: Float){
  var x = arg_x
  var y = arg_y
  def canEqual(a: Any) = a.isInstanceOf[DPosition]

  def translate(dx: Float, dy: Float): DPosition = {
    return new DPosition(x+dx,y+dy)
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: DPosition => that.x == x && that.y == y
      case _ => false
  }

}

/* A PositionPath is a position that holds a little bit more information,
 * ie it's cost for the A* pathfinding algorithm
 */
class PositionPath(arg_x:Int , arg_y: Int, arg_parent: PositionPath, arg_gcost: Int, arg_hcost: Int) extends Position(arg_x, arg_y){
  var gcost = arg_gcost
  var hcost = arg_hcost
  var parent = arg_parent

}

object Origin extends Position(0,0)
{}
object DOrigin extends DPosition(0.0f,0.0f)
{}


