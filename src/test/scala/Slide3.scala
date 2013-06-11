import org.specs2.mutable.Specification
import scalaz._, Scalaz._


trait Slide3 {

  /**
   * An ordered collection of elements.
   */
  trait Collection[A] {
    /**
     * Apply an associative function to combine the collection of values to a single value.
     */
    def combine(r: Reduction[A]): A
  }

  object Collection extends Collections
  trait Collections {
    /**
     * Construct a collection from a sequence of elements.
     */
    def apply[A](x: A*): Collection[A] = new Collection[A] {
      def combine(r: Reduction[A]): A = x.reduce(r.reduce)
    }
  }


  /**
   * An associative binary operator on values of type `A`.
   */
  trait Reduction[A] {
    val reduce: (A, A) => A

    def zip[B](r: Reduction[B]): Reduction[(A, B)] =
      Reduction {
        case ((a1, b1), (a2, b2)) => (reduce(a1, a2), r.reduce(b1, b2))
      }

    def zip3[B, C](b: Reduction[B], c: Reduction[C]): Reduction[(A, B, C)] =
      Reduction {
        case ((a1, b1, c1), (a2, b2, c2)) => (reduce(a1, a2), b.reduce(b1, b2), c.reduce(c1, c2))
      }

    def xmap[B](f: A => B, g: B => A): Reduction[B] =
      Reduction((b1, b2) => f(reduce(g(b1), g(b2))))
  }

  object Reduction extends Reductions
  trait Reductions {
    /**
     * Construct a reduction from the given binary operation.
     */
    def apply[A](f: (A, A) => A): Reduction[A] = new Reduction[A] {
      val reduce = f
    }

    def maximum[A](implicit O: Order[A]): Reduction[A] =
      Reduction((a1, a2) => O max (a1, a2))

    def minimum[A](implicit O: Order[A]): Reduction[A] =
      Reduction((a1, a2) => O min (a1, a2))

    def string: Reduction[String] =
      Reduction(_ + _)

    def mapS[K, V](R: Reduction[V]): Reduction[Map[K, V]] =
      Reduction {
        (m1, m2) =>
          m1 ++ (m2 map { case (k, v) => k -> (m1.get(k).map(R.reduce(_, v)).getOrElse(v)) })
      }

    object Sum {
      def int: Reduction[Int] =
        Reduction(_ + _)
    }
  }
}



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/**
 * Usage examples.
 */
class Slide3Spec extends Specification with Slide3 {

  import Reduction._

  "Combining integers" >> {

    val ints = Collection(5, 3, 6, 4)

    "Can compute the sum" >> {
      ints.combine(Sum.int) must_== 18
    }

    "Can compute the maximum" >> {
      ints.combine(maximum) must_== 6
    }
  }


  "Combining strings" >> {

    val beers = Collection("Pilsner", "IPA", "Stout", "Bitter")

    "Can concatenate" >> {
      beers.combine(string) must_== "PilsnerIPAStoutBitter"
    }

    "Can get the shortest" >> {
      val strLenOrd: Order[String] = Order.orderBy(_.length)
      val shortest = beers.combine(minimum(strLenOrd))
      shortest must_== "IPA"
    }
  }


  "Combining vector points" >> {

    case class Point(x: Int, y: Int)
    val toPair = (p: Point) => (p.x, p.y)
    val fromPair = (t: (Int, Int)) => Point(t._1, t._2)

    val points = Collection(Point(3, 4), Point(1, 2), Point(-2, -3))

    "Can compute the sum" >> {
      val pointSum: Reduction[Point] = (Sum.int zip Sum.int).xmap(fromPair, toPair)
      val summed = points.combine(pointSum)
      summed must_== Point(2, 3)
    }

    "Can compute the minimum" >> {
      def distance(p: Point): Double = math.abs(math.sqrt((p.x * p.x) + (p.y * p.y)))
      val closest = points.combine(minimum(Order.orderBy(distance)))
      closest must_== Point(1, 2)
    }
  }


  "Can combine tuples" >> {

    "Pairs" >> {
      val pairs = Collection((3, 4), (-2, 6), (1, 8), (0, -9))
      val stats = pairs.combine(Sum.int zip maximum)
      stats must_== (2, 8)
    }

    "Triples" >> {
      val triples = Collection((3, 4, 2), (-2, 6, 3), (1, 8, 4), (0, -9, 2))
      val stats = triples.combine(Sum.int.zip3(maximum, minimum))
      stats must_== (2, 8, 2)
    }
  }


  "Can compute the union of a collection of Maps" >> {
    val maps = Collection(
      Map("a" -> 1, "b" -> 3, "c" -> 4),
      Map("b" -> 2, "a" -> 2),
      Map("d" -> 4, "a" -> 3))
    val unioned = maps.combine(mapS(Sum.int))
    unioned must_== Map("a" -> 6, "b" -> 5, "c" -> 4, "d" -> 4)
  }
}