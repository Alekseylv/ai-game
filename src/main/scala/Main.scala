import domain.{Graph, Mapping}

import scala.collection.mutable

object Main {
  type Result = List[List[String]]

  def main(args: Array[String]) {
    var hasResultWith2 = 0
    var canClimbTo2 = 0
    val permutations = Array.fill(6)(1 to 6 toList).flatten.combinations(6).map(_.permutations).flatten.toList.map(_.mkString(""))
    permutations.foreach(x => {
      println(x)
      val res = result(x)
      if (res._1.mapping.stringToInt.contains("2")) {
        hasResultWith2 += 1
        if (res._2.last.contains("2")) {
          canClimbTo2 += 1
        }
      }
    })

    println(s"total results = ${permutations.length}, nonempty results = $hasResultWith2, good results = $canClimbTo2")
//     nonempty results = 43883, good results = 38534
//     nonempty results = 43883, good results = 42710

    //      result("632145").foreach(println)


//    val s = miniMax(graph)
//    println(graph.toString(s.apply))
  }

  def result(start:String) = {
    val map = mutable.HashMap[String, List[String]]()
    var current = List(start.toList.map(_.toString))
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

    (graph, star(graph).map(withoutObjective))
  }

  def star(graph:Graph) = {
    search(graph)(x => x.sortBy(- _._2).take(3))
  }

  def climb(graph:Graph) = {
    search(graph)(x => List(x.max[(Int, Double)](Ordering.by(s => s._2))))
  }

  def miniMax(graph:Graph) = {
    val values = Array.fill(graph.V)(0)
    def vertexValue(v:Int, level:Boolean):Boolean = {
      val result = if (graph.outbound(v).isEmpty) {
        graph.mapping(v).toInt % 2 == 1
      } else { // remove toStream for non-optimized minimax
        graph.outbound(v).toStream.map(x => vertexValue(x, !level)).find(_ == level).getOrElse(!level)
      }

      values(v) = if (result) 11 else 22
      result
    }
    vertexValue(0, level = false)
    values
  }

  def withoutObjective(list: List[(String, Double)]) = list.map(_._1)

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
