import scalaz._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck


trait Slide4 {

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

    /**
     * Takes a pair of reductions to a reduction on pairs.
     */
    def zip[B](r: Reduction[B]): Reduction[(A, B)] =
      Reduction {
        case ((a1, b1), (a2, b2)) => (reduce(a1, a2), r.reduce(b1, b2))
      }

    /**
     * Takes three reductions to a reduction on tuple-3.
     */
    def zip3[B, C](b: Reduction[B], c: Reduction[C]): Reduction[(A, B, C)] =
      Reduction {
        case ((a1, b1, c1), (a2, b2, c2)) => (reduce(a1, a2), b.reduce(b1, b2), c.reduce(c1, c2))
      }

    /**
     * Maps a pair of functions on a reduction to produce a reduction.
     */
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

    /**
     * A reduction that produces the maximum value of its two arguments.
     */
    def maximum[A](implicit O: Order[A]): Reduction[A] =
      Reduction((a1, a2) => O max (a1, a2))

    /**
     * A reduction that produces the minimum value of its two arguments.
     */
    def minimum[A](implicit O: Order[A]): Reduction[A] =
      Reduction((a1, a2) => O min (a1, a2))

    /**
     * A reduction on strings by appending.
     */
    def string: Reduction[String] =
      Reduction(_ + _)

    /**
     * A reduction on maps by appending and reducing values with the same key using the
     * specified reduction.
     */
    def mapS[K, V](R: Reduction[V]): Reduction[Map[K, V]] =
      Reduction {
        (m1, m2) =>
          m1 ++ (m2 map { case (k, v) => k -> (m1.get(k).map(R.reduce(_, v)).getOrElse(v)) })
      }

    /**
     * Reductions that perform addition.
     */
    object Sum {
      /**
       * Reductions on integers by addition.
       */
      def int: Reduction[Int] =
        Reduction(_ + _)
    }
  }
}



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/**
 * Usage examples.
 */
class Slide4Spec extends Specification with ScalaCheck with Slide4 {

  /* ScalaCheck primer */

  "Reversing a list twice is the same as the original list" >> {
    prop { (xs: List[Int]) => xs.reverse.reverse must_== xs }
  }

  "Summing over integers is associative" >> {
    prop { (a: Int, b: Int, c: Int) => ((a + b) + c) must_== (a + (b + c)) }
  }

  "Finding the maximum integer is associative" >> {
    prop { (a: Int, b: Int, c: Int) => ((a max b) max c) must_== (a max (b max c)) }
  }


  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


  import Reduction._

  /**
   * Property that a Reduction must be associative.
   */
  def reductionLaw[T](r: Reduction[T])(implicit A: Arbitrary[T]): Prop =
    prop {
      (a: T, b: T, c: T) => r.reduce(r.reduce(a, b), c) must_== r.reduce(a, r.reduce(b, c))
    }


  "Minimum integer is associative" >> {
    reductionLaw(Reduction.minimum[Int])
  }


  "Combining integers" >> {
    "Can compute the sum" >> reductionLaw(Sum.int)
    "Can compute the maximum" >> reductionLaw(maximum[Int])
  }


  "Combining strings" >> {

    "Can concatenate" >>
      reductionLaw(string)

    "Can get the shortest" >> {
      val strLenOrd: Order[String] = Order.orderBy(_.length)
      reductionLaw(minimum(strLenOrd))
    }
  }


  "Combining vector points" >> {

    case class Point(x: Int, y: Int)

    val toPair = (p: Point) => (p.x, p.y)
    val fromPair = (t: (Int, Int)) => Point(t._1, t._2)

    implicit val genPoint: Arbitrary[Point] = Arbitrary {
      for {
        x <- arbitrary[Int]
        y <- arbitrary[Int]
      } yield Point(x, y)
    }

    "Can compute the sum" >> {
      val pointSum: Reduction[Point] = (Sum.int zip Sum.int).xmap(fromPair, toPair)
      reductionLaw(pointSum)
    }

    "Can compute the minimum" >> {
      def distance(p: Point): Double = math.abs(math.sqrt((p.x * p.x) + (p.y * p.y)))
      reductionLaw(minimum(Order.orderBy(distance)))
    }
  }


  "Can combine tuples" >> {
    "Pairs" >> reductionLaw(Sum.int zip maximum[Int])
    "Triples" >> reductionLaw(Sum.int.zip3(maximum[Int], minimum[Int]))
  }


  "Can compute the union of a collection of Maps" >> {
    reductionLaw(mapS[String, Int](Sum.int))
  }
}