package scala.collection
package decorators

import scala.collection.generic.IsMap

class MapDecorator[C, M <: IsMap[C]](coll: C)(implicit val map: M) {

  /**
    * @return A Map associating all the keys from `this` and `that` with values returned by the partial function
    *         `f`, when this one is defined.
    *
    * @param other Map to merge
    * @param f Combination function
    * @param bf Builder driven by the type of `this` Map
    * @tparam W Type of values of the other Map (e.g. `Int`, `String`)
    * @tparam X Type of values of the resulting Map
    * @tparam That Type of the result (e.g. `Map[Int, (String, Option[Boolean])]`)
    */
  def mergeByKeyWith[W, X, That](other: Map[map.K, W])(f: PartialFunction[(Option[map.V], Option[W]), X])(implicit bf: BuildFrom[C, (map.K, X), That]): That = {
    val b = bf.newBuilder(coll)
    val thisMap = map(coll)
    val allKeys = thisMap.keys ++ other.keys
    val pf = f.lift
    for(key <- allKeys) {
      val res = pf(thisMap.get(key), other.get(key))
      if(res.nonEmpty) b += (key -> res.get)
    }
    b.result()
  }

  /**
    * Perform a full outer join of `this` and `that`.
    *
    * Equivalent to `mergeByKeyWith(that) { case any => any }`.
    *
    * @param bf Builder for the resulting collection
    * @tparam W Type of values of `that` (e.g. `String`)
    * @tparam That Type of the resulting collection (e.g. `Map[Int, (Option[Boolean], Option[String])]`)
    * @return A Map that associates all the keys `k` of `this` or `that` to pairs `(Some(v), Some(w))` if `this`
    *         associates `k` to `v` and `that` associates `k` to `w`, or pairs `(None, Some(w))` if `this` doesn’t
    *         contain `k`, or pairs `(Some(v), None)` if `that` doesn’t contain `k`
    */
  @`inline` final def mergeByKey[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (Option[map.V], Option[W])), That]): That =
    mergeByKeyWith(other) { case any => any }

  /** Alias for `mergeByKey` */
  @`inline` final def fullOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (Option[map.V], Option[W])), That]): That =
    mergeByKey(other)

  /**
    * Perform a left outer join of `this` and `that`.
    *
    * Equivalent to `mergeByKeyWith(that) { case (Some(v), maybeW) => (v, maybeW) }`.
    *
    * @param bf Builder for the resulting collection
    * @tparam W Type of values of `that`
    * @tparam That Type of the resulting collection
    * @return A Map that associates all the keys `k` of `this` to pairs `(v, Some(w))` if `that` associates `k` to `w`,
    *         or `(v, None)` if `that` doesn’t contain `k`
    */
  def leftOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (map.V, Option[W])), That]): That = {
    val b = bf.newBuilder(coll)
    for ((k, v) <- map(coll)) {
      b += k -> ((v, other.get(k)))
    }
    b.result()
  }

  /**
    * Perform a right outer join of `this` and `that`.
    *
    * Equivalent to `mergeByKeyWith(that) { case (maybeV, Some(w)) => (maybeV, w) }`.
    *
    * @param bf Builder for the resulting collection
    * @tparam W Type of values of `that` (e.g. `String`)
    * @tparam That Type of the resulting collection (e.g. `Map[Int, (Option[Boolean], String)]`)
    * @return A Map that associates all the keys `k` of `that` to pairs `(Some(v), w)` if `this` associates `k` to `v`,
    *         or `(None, w)` if `this` doesn’t contain `k`
    */
  def rightOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (Option[map.V], W)), That]): That = {
    val b = bf.newBuilder(coll)
    for ((k, w) <- other) {
      b += k -> ((map(coll).get(k), w))
    }
    b.result()
  }

}
