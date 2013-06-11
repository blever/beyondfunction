import org.specs2.mutable.Specification


trait Slide2 {

  /**
   * An ordered collection of elements.
   */
  trait Collection[A] {
    /**
     * Apply an associative function to combine the collection of values to a single value.
     */
    def combine(f: (A, A) => A): A
  }

  object Collection extends Collections
  trait Collections {
    /**
     * Construct a collection from a sequence of elements.
     */
    def apply[A](x: A*): Collection[A] = new Collection[A] {
      def combine(f: (A, A) => A): A = x.reduce(f)
    }
  }


  /**
   * An associative binary operator on values of type `A`.
   */

  // TODO

}



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/**
 * Usage examples.
 */
class Slide2Spec extends Specification with Slide2 {

  "Combining integers" >> {

    val ints = Collection(5, 3, 6, 4)

    "Can compute the sum" >> {
      val sum: Int = ???
      sum must_== 18
    }.pendingUntilFixed

    "Can compute the maximum" >> {
      val biggest: Int = ???
      biggest must_== 6
    }.pendingUntilFixed
  }







//
//  "Combining strings" >> {
//
//    val beers = Collection("Pilsner", "IPA", "Stout", "Bitter")
//
//    "Can concatenate" >> {
//      beers.combine(Reduction(_ + _)) must_== "PilsnerIPAStoutBitter"
//    }
//
//    "Can get the shortest" >> {
//      val shortest = beers.combine(Reduction((s1, s2) => if (s2.length > s1.length) s1 else s2))
//      shortest must_== "IPA"
//    }
//  }
//
//
//  "Combining vector points" >> {
//
//    case class Point(x: Int, y: Int)
//    val points = Collection(Point(3, 4), Point(1, 2), Point(-2, -3))
//
//    "Can compute the sum" >> {
//      val summed = points.combine(Reduction { case ((Point(x1, y1), Point(x2, y2))) => Point(x1 + x2, y1 + y2) })
//      summed must_== Point(2, 3)
//    }
//
//    "Can compute the minimum" >> {
//      def distance(p: Point): Double = math.abs(math.sqrt((p.x * p.x) + (p.y * p.y)))
//      val minimum = points.combine(Reduction { case (p1, p2) =>
//        if (distance(p1) < distance(p2))
//          p1
//        else
//          p2
//      })
//      minimum must_== Point(1, 2)
//    }
//  }
//
//
//  "Can combine tuples" >> {
//
//    "Sum and max over pairs" >> {
//      val pairs = Collection((3, 4), (-2, 6), (1, 8), (0, -9))
//      val stats = pairs.combine(Reduction { case ((a1, b1), (a2, b2)) => ((a1 + a2), (b1 max b2)) })
//      stats must_== (2, 8)
//    }
//
//    "Sum, max and min over triples" >> {
//      val triples = Collection((3, 4, 2), (-2, 6, 3), (1, 8, 4), (0, -9, 2))
//      val stats = triples.combine(Reduction { case ((a1, b1, c1), (a2, b2, c2)) => ((a1 + a2), (b1 max b2), (c1 min c2)) })
//      stats must_== (2, 8, 2)
//    }
//  }
//
//
//  "Can compute the union of a collection of Maps" >> {
//    val maps = Collection(
//      Map("a" -> 1, "b" -> 3, "c" -> 4),
//      Map("b" -> 2, "a" -> 2),
//      Map("d" -> 4, "a" -> 3))
//    val unioned = maps.combine(Reduction { case (m1, m2) =>
//      m1 ++ m2.map{ case (k, v) => k -> (m1.get(k).map(_ + v).getOrElse(v)) }
//    })
//    unioned must_== Map("a" -> 6, "b" -> 5, "c" -> 4, "d" -> 4)
//  }
}