import scala.annotation.tailrec
import scala.collection.mutable

object Cracking extends App{

  /**
   * 1.1
   */
  def isUniq(string: String) : Boolean ={
    val strList = string.toCharArray.toList
    val hs: mutable.HashSet[Char] = mutable.HashSet()

    for {element <- strList} {
      if(hs.contains(element)){
        return false
      }
      else {
        hs.add(element)
      }


    }

    true
  }

  def isUniqWithoutHashSet(string: String): Boolean = {
    val strList = string.toList
    for {
      elem1 <- strList
      elem2 <- strList


    } {

      if((elem1 == elem2) && strList.indexOf(elem1) != strList.lastIndexOf(elem2))  {

        return false
      }
    }
    true
  }

  /**
   * 1.2
   */

  def checkPermutation(str1 : String, str2: String): Boolean = {
    val map1 = str1.groupBy(identity).mapValues(_.length).toMap
    val map2 = str2.groupBy(identity).mapValues(_.length).toMap
    println(map1)
    println(map2)
    map1 == map2

  }

  /**
   * 1.3
   */



  def URLify(string: List[Char], trueLength: Int) : String = {
    @tailrec
    def helper(str : List[Char], count: Int, newStr: List[Char]): String = {
      str match {
        case Nil => newStr.mkString
        case x::_ => if (count == trueLength) {
          newStr.mkString
        }
        else if(x == ' ') {
          helper(str.tail, count + 1, (((newStr :+ '%') :+ '2') :+ '0'))
        }
        else {
          helper(str.tail, count + 1, newStr :+ x)
        }
      }
    }


    helper(string, 0, List())
  }

}