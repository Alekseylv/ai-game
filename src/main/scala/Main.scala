object Main {

  type Result = List[List[String]]

  def main(args:Array[String]) {

    var current = List("632145".toList.map(_.toString))
    var count = 0
    var sum = List[Int]()
    do {
        val temp = current.map(explode)
      println(printableResult2(current) + " " + printableResult(current).length)
      count += printableResult(current).length
      sum = current.zip(temp).map(x => {
        println("("+ x._1.mkString("") + ") -> (" + printableResult(x._2).mkString(", ") + "), Z.f. = " + printableResult(x._2).length)
        printableResult(x._2).length
      }) ::: sum
      current = printableResult(temp.flatten).map(x => x.toList.map(y => y.toString))
    } while(current.head.length > 1)

    println(printableResult2(current) + " " + printableResult(current).length)
    count += printableResult(current).length
    val realSum = sum.fold(0)(_ + _)
    println((sum.reverse ::: List(0,0,0,0,0,0)  mkString " + ") + " = " + realSum)
    println(count)

    println("V.z.f = " + (realSum.toDouble / count))
  }

  def printableResult(current:Result) = current.map(_.mkString("")).toSet.toList

  def printableResult2(current:Result) = "Limenis (" + printableResult(current).mkString(", ") + ")"


  def explode(arg:List[String]) = {
    def internal(current:List[String], first:List[String]):Result = {
      current match {
        case x :: y :: z => (first ::: (normalize(x.toInt + y.toInt) :: z)) :: (first ::: (y :: z)) :: internal(z, first ::: (x :: y :: Nil))
        case _ => Nil
      }
    }
    internal(arg, List())
  }


  def normalize(x:Int):String = (if (x > 6) x - 6 else x).toString

}
