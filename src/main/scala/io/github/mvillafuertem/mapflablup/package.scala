package io.github.mvillafuertem

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MutableMap}

package object mapflablup {

  trait DtoOperation[A] {
    def flatten(a: A): A

    def blowUp(a: A): A
  }


  val mapOperation = new DtoOperation[Map[String, Any]] {
    /*
    - recursive function
    - carry previous key. k1.k2...kn
    - concatenate string keys with dot separator
    - case String -> Map[String, Any]
    - case String -> Not Map[String, Any]
    - retrieve entry iterator
   */
    override def flatten(map: Map[String, Any]): Map[String, Any] = {

      var resultMap = MutableMap.empty[String, Any]

      def buildKey(prefix: String, suffix: String) = {
        if (prefix.isEmpty) suffix
        else s"$prefix.$suffix"
      }

      // TODO add Map return type
      def go(map: Map[String, Any], k0: String) {
        if (map.nonEmpty) {
          val it = map.iterator
          it.foreach(
            e => e match {
              case (k1: String, value: Map[String, _]) => {
                go(value, k1)
              }
              case (k1, value) => {
                val key = buildKey(k0, k1)
                resultMap += (key -> value)
              }
            }
          )
        }
      }

      go(map, "")

      resultMap.toMap
    }

    override def blowUp(a: Map[String, Any]): Map[String, Any] = ???
  }


  /* Unsafe code (stack overflow)
   if (map.isEmpty) Map.empty
   else copyMap(map.tail) + (map.head._1 -> map.head._2)
  */
  def copyMap(map: Map[String, Any]): Map[String, Any] = {
    // safe implementation
    @tailrec
    def go(map: Map[String, Any], copy: Map[String, Any]): Map[String, Any] = {
      if (map.isEmpty) copy
      else go(map.tail, copy + (map.head._1 -> map.head._2))
    }

    go(map, Map.empty)
  }

  def keyToList(key: String): List[String] = {
    if (key.isEmpty) List()
    else {
      val list = key.split('.').toList
      if (list.isEmpty) List(key)
      else list
    }
    //
  }
}
