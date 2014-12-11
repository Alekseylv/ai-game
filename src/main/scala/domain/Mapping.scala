package domain

class Mapping(vertices:List[String])  {
  private val iterator = vertices.iterator

  val stringToInt:Map[String, Int] = vertices.zip(0 to vertices.length).groupBy(_._1).mapValues(_.head._2)
  val intToString:Array[String] = Array.fill(vertices.length)(iterator.next())

  def apply(s:String) = stringToInt(s)
  def apply(i:Int) = intToString(i)
}
