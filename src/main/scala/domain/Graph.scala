package domain

class Graph(val V: Int, val mapping: Mapping) {

  type EdgeOuter = (String, String)
  type EdgeInner = (Int, Int)

  if (V < 0) throw new IllegalArgumentException("Number of vertices must be nonnegative")

  private var adjecentInbound: Array[List[Int]] = Array.fill(V)(List())
  private var adjecentOutbound: Array[List[Int]] = Array.fill(V)(List())
  private var E: Int = 0

  def edges = E

  private def validateEdge(e: EdgeInner) {
    validateVertex(e._1)
    validateVertex(e._2)
  }

  private def validateVertex(v: Int) {
    if (v < 0 || v >= V) {
      throw new IndexOutOfBoundsException("Vertex " + v + " is not between 0 and " + (V - 1))
    }
  }

  def +(eo: EdgeOuter): this.type = {
    val e = outerToInnerEdge(eo)
    validateEdge(e)
    E += 1
    adjecentOutbound(e._1) ::= e._2
    adjecentInbound(e._2) ::= e._1
    this
  }

  def inbound(v: Int) = {
    validateVertex(v)
    adjecentInbound(v)
  }

  def outbound(v: Int) = {
    validateVertex(v)
    adjecentOutbound(v)
  }

  def degree(v: Int) = {
    validateVertex(v)
    outbound(v).length
  }

  def heuristic(v: Int) = {
    val numbers: List[Int] = mapping(v).toList.map(_.toString.toInt)
    val mod = numbers.sum % 6
    (if (mod == 2) 0.95 else 0.0) + (0.1 / mapping(v).toInt.toDouble)
  }

  private def round(d: Double) = (10000 * d).round * 0.0001

  override def toString = {
    val NEWLINE = System.getProperty("line.separator")
    (V + " vertices, " + E + " edges " + NEWLINE) +
      (0 to V - 1).map(x => {
        s"${mapping(x)} (${round(heuristic(x))}}): " + adjecentOutbound(x).map(mapping(_)).mkString(" ")
      }).mkString(NEWLINE)
  }

  def outerToInnerEdge(e: EdgeOuter): EdgeInner = (mapping(e._1), mapping(e._2))

  def mapWithObjective(v:Int) = (mapping(v), heuristic(v))
}
