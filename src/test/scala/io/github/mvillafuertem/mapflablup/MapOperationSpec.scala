package io.github.mvillafuertem.mapflablup

import io.github.mvillafuertem.mapflablup.mapOperation._
import org.scalatest.{FlatSpec, Matchers}

class MapOperationSpec extends FlatSpec with Matchers {

  val emptyMap = Map.empty[String, Any]

  it should "retrieve an empty flatten map" in {
    val result: Map[String, Any] = flatten(emptyMap)

    result.isEmpty shouldBe true
  }

  it should "retrieve a non nested flatten map" in {
    val map = Map("k1" -> "v2", "k2" -> true, "k3" -> 1.0)

    val result: Map[String, Any] = flatten(map)

    result equals map shouldBe true
  }

  it should "retrieve a nested flatten map" in {
    val nestedMap = Map("k1" -> "v2", "k2" -> true)
    val map = Map("k1" -> "v1", "k2" -> nestedMap, "k3" -> 1.0)
    val expected = Map("k1" -> "v1", "k2.k1" -> "v2", "k2.k2" -> true, "k3" -> 1.0)

    val result: Map[String, Any] = flatten(map)

    result equals expected shouldBe true
  }

  it should "copy a non empty map" in {
    val map = Map("k1" -> "v1", "k2" -> "v2", "k3" -> "v3")

    val result: Map[String, Any] = copyMap(map)

    result equals map shouldBe true
  }

  it should "copy an empty map" in {
    val result: Map[String, Any] = copyMap(emptyMap)

    result.isEmpty shouldBe true
  }

  it should "return an empty string list from an empty key" in {
    val key = ""
    val keyList = keyToList(key)

    keyList.isEmpty shouldBe true
  }

  it should "return an empty string list from a single key" in {
    val key = "k1"
    val keyList = keyToList(key)

    keyList.nonEmpty shouldBe true
    keyList.head shouldBe key
  }

  it should "return an empty string list from a multiple key" in {
    val key = "k1.k2"
    val keyList = keyToList(key)

    keyList.nonEmpty shouldBe true
    keyList.size shouldBe 2
  }

}
