import domain.{Graph, Mapping}

import scala.collection.mutable

object Main {
  type Result = List[List[String]]

  def main(args: Array[String]) {
    val map = mutable.HashMap[String, List[String]]()
    var current = List("632145".toList.map(_.toString))
    var result = List[Result](current)
    do {
      current = current.map(addToMapAndExplode(map, _)).flatten
      result ::= current
    } while (current.head.length > 1)
    val levels = result.reverse.map(_.map(_.mkString(""))).map(_.toSet.toList)
//    levels.foreach(println)

    val mapping = new Mapping(levels.flatten)
    val graph = new Graph(mapping.intToString.length, mapping)

    map.foreach(x => {
      x._2.distinct.foreach(y => {
        graph + ((x._1, y))
      })
    })

    println(graph)
    println(climb(graph))
  }

  def star(graph:Graph) = {
    search(graph)(x => x.sortBy(- _._2).take(2))
  }

  def climb(graph:Graph) = {
    search(graph)(x => List(x.max[(Int, Double)](Ordering.by(s => s._2))))
  }

  def search(graph:Graph)(p:List[(Int, Double)] => List[(Int, Double)]) = {
    def internal(node: List[Int]): List[List[(String, Double)]] = node.map(graph.outbound).flatten match {
      case Nil => Nil
      case x => {
        val temp: List[(Int, Double)] = x.map(p => (p, graph.heuristic(p)))
        val next = p(temp.distinct).map(_._1)
        next.map(graph.mapWithObjective) :: internal(next)
      }
    }
    List(graph.mapWithObjective(0)) :: internal(List(0))
  }

  def addToMapAndExplode(map: mutable.HashMap[String, List[String]], arg: List[String]) = {
    val result = explode(arg)
    map += ((to_string(arg), result.map(to_string)))
    result
  }

  def to_string(list: List[String]) = list.mkString("")

  def explode(arg: List[String]) = {
    def internal(current: List[String], first: List[String]): Result = {
      current match {
        case x :: y :: z => (first ::: (normalize(x.toInt + y.toInt) :: z)) :: (first ::: (y :: z)) :: internal(z, first ::: (x :: y :: Nil))
        case _ => Nil
      }
    }
    internal(arg, List())
  }

  def normalize(x: Int): String = (if (x > 6) x - 6 else x).toString
}
