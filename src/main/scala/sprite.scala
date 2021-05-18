package rogue

import java.awt.Color
import org.json._

// A SubSprite is an element of our tileset, and a color
// for this element
class SubSprite(char_code: Int, color: String){
  def getColor(): String = {
    return color
  }
  def getCharCode(): Int = {  
    return char_code
  }
}

// A sprite is a superposition of subsprites,
// and a background color
class Sprite(elements: Array[SubSprite], bg_color: Color){
  def getBgColor(): Color = {
    return bg_color
  }
  def getElements(): Array[SubSprite] = {  
    return elements
  }

  def to_json(): JSONObject = {
    val json:JSONObject = new JSONObject()
    val subsprites = new JSONArray()
    for ( subsprite <- elements ){
      val subsprite_json:JSONObject = new JSONObject()
      subsprite_json.put("color",subsprite.getColor())
      subsprite_json.put("charcode",subsprite.getCharCode())
      subsprites.put(subsprite_json)
    }
    
    json.put("subsprites",subsprites)

    val red = getBgColor().getRed() 
    val green = getBgColor().getGreen() 
    val blue = getBgColor().getBlue() 
    val alpha = getBgColor().getAlpha() 

    json.put("red",red)
    json.put("green",green)
    json.put("blue",blue)
    json.put("alpha",alpha)

    return json
  }
}

object SColor{
  val ZwiffyGreen = "255051000"//red, but with goblin vision  
  val Black = "000000000"
  val White = "255255255"
  val GoblinGreen ="161200060"
  val Cyan = "000191255"
  val Green = "000255000"
  val Red = "255000000"
  val LeatherBrown = "102051000"
  val Grey = "080080080"
  val WolfGrey = "072096112"
  val BatPurple = "153000204"
  val Blue = "000000255"
  val DeepPink = "255020147"
  val Yellow = "255255000"
  val Orange = "252132003"
  val Rust = "147058022"
  val Steel = "202204206"
  val Gold = "255215000"
  val Leather = "144107082"
  val BrightBlue = "000078255"
  val FakeWallGrey = "120120120"
  val CrimsonRed = "110029029"
  val DeepBlue = "005036161"
  val Lime = "017214024"
  val Cream = "255212059"
  val Ash1 = "170170170"
  val Ash2 = "173173173"
  val Ash3 = "180180180"
} 
