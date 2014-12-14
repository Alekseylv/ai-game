import domain.{Graph, Mapping}

import scala.collection.mutable

object Main {
  type Result = List[List[String]]
  type Level = (List[(String, Double)], List[(String, Double)])

  def main(args: Array[String]) {
    //   statistics()
    //      val s = miniMax(graph)
    //      println(graph.toString(s.apply))
//    val res = result("63215")
//    println(res._1)
//    res._2.foreach(levelToString _ andThen println)

    val graph = createGraph("63215")
    val s = miniMax(graph)
    println(graph.toString(x => s.apply(x).toString))
    pathsToVictory(graph, s).foreach(println)
  }

  def levelToString(input: Level) = {
    val sorted = (input._1.sortBy(-_._2), input._2.sortBy(-_._2))
    s"${inputToString(sorted._1)("OPEN")} ${inputToString(sorted._2)("Closed")}"
  }

  def inputToString(input: List[(String, Double)])(name: String) = {
    val sanitized = input.map(x => s"${x._1}[${x._2.formatted("%1.4e")}]").mkString(", ")
    s"$name: $sanitized"
  }

  def statistics() {
    var hasResultWith2 = 0
    var canClimbTo2 = 0
    val permutations = Array.fill(6)(1 to 6 toList).flatten.combinations(6).map(_.permutations).flatten.toList.map(_.mkString(""))
    permutations.foreach(x => {
      val res = result(x)
      if (res._1.mapping.stringToInt.contains("2")) {
        hasResultWith2 += 1
        if (res._2.map(s => withoutObjective(s._1)).last.contains("2")) {
          canClimbTo2 += 1
        }
      }
    })

    println(s"total results = ${permutations.length}, nonempty results = $hasResultWith2, good results = $canClimbTo2")
    //           total results = 46656
    //  climb:   nonempty results = 43883, good results = 38534
    //  star:    nonempty results = 43883, good results = 42710
  }

  def result(start: String) = {
    val graph = createGraph(start)
    (graph, climb(graph))
  }

  def star(graph: Graph) = {
    search(graph)(x => x.sortBy(-_._2).take(3))
  }

  def climb(graph: Graph) = {
    search(graph)(x => List(x.max[(Int, Double)](Ordering.by(s => s._2))))
  }

  def createGraph(start:String) = {
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

    graph
  }

  def miniMax(graph: Graph) = {
    val values = Array.fill(graph.V)(-1)
    def vertexValue(v: Int, level: Boolean): Boolean = {
      val result = if (graph.outbound(v).isEmpty) {
        graph.mapping(v).toInt % 2 == 1
      } else {
        // remove toStream for non-optimized minimax
        graph.outbound(v).map(x => vertexValue(x, !level)).find(_ == level).getOrElse(!level)
      }

      values(v) = if (result) 1 else 0
      result
    }
    vertexValue(0, level = true)
    values
  }

  def pathsToVictory(graph:Graph, values:Array[Int]) = {
    val victor = values(0)
    def search(v:Int):List[List[Int]] = {
      if (graph.outbound(v).isEmpty) List(List(v)) else
      graph.outbound(v).filter(values(_) == victor).map(search).flatten.map(v :: _)
    }
    search(0).map(_.map(graph.mapping.apply).mkString(" -> "))
  }

  def withoutObjective(list: List[(String, Double)]) = list.map(_._1)

  def search(graph: Graph)(p: List[(Int, Double)] => List[(Int, Double)]): List[Level] = {
    def internal(node: List[Int], closed: List[(String, Double)]): List[Level] = node.map(graph.outbound).flatten match {
      case Nil => Nil
      case x => {
        val temp: List[(Int, Double)] = x.map(p => (p, graph.heuristic(p))).distinct
        val next = p(temp)
        val currentClosed = closed ::: (temp diff next).map(x => (graph.mapping(x._1), x._2))
        (next.map(x => (graph.mapping(x._1), x._2)), currentClosed) :: internal(next.map(_._1), next.map(x => (graph.mapping(x._1), x._2)) ::: currentClosed)
      }
    }
    (List(graph.mapWithObjective(0)), List()) :: internal(List(0), List(graph.mapWithObjective(0)))
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
