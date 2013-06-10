import scalaz._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck


trait Slide4 {

  /** An ordered collection of elements. */
  trait Collection[A] {
    /* Apply an associative function to combine the collection of values to a single value. */
    def combine(f: Reduction[A]): A
  }

  object Collection {
    def apply[A](x: A*): Collection[A] = new Collection[A] {
      def combine(f: Reduction[A]): A = x.reduce(f.reduce)
    }
  }


  /* An associative binary operator on values of type A. */
  trait Reduction[A] {
    val reduce: (A, A) => A

    def xmap[B](f: A => B, g: B => A): Reduction[B] =
      Reduction((b1, b2) => f(reduce(g(b1), g(b2))))

    def zip[B](r: Reduction[B]): Reduction[(A, B)] =
      Reduction {
        case ((a1, b1), (a2, b2)) => (reduce(a1, a2), r.reduce(b1, b2))
      }

    def zip3[B, C](b: Reduction[B], c: Reduction[C]): Reduction[(A, B, C)] =
      Reduction {
        case ((a1, b1, c1), (a2, b2, c2)) => (reduce(a1, a2), b.reduce(b1, b2), c.reduce(c1, c2))
      }

    def pointwise[B]: Reduction[B => A] =
      Reduction((g, h) => b => reduce(g(b), h(b)))
  }

  object Reduction {
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



class Slide4Spec extends Specification with ScalaCheck with Slide4 {

  "Summing over integers is associative" >> {
    prop { (a: Int, b: Int, c: Int) => ((a + b) + c) must_== (a + (b + c)) }
  }

  "Finding the maximum integer is associative" >> {
    prop { (a: Int, b: Int, c: Int) => ((a max b) max c) must_== (a max (b max c)) }
  }


  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


  import Reduction._

  /* A Reduction must be associative. */
  def reductionLaw[T](r: Reduction[T])(implicit A: Arbitrary[T]): Prop =
    prop { (a: T, b: T, c: T) => r.reduce(r.reduce(a, b), c) must_== r.reduce(a, r.reduce(b, c)) }

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