package scala.collection

import scala.collection.generic.IsMap
import scala.language.implicitConversions

package object decorators {

  implicit def MapDecorator[C](coll: C)(implicit map: IsMap[C]): MapDecorator[C, map.type] =
    new MapDecorator(coll)(map)

}
