import scalaz._, Scalaz._
import org.specs2.mutable.Specification


trait Slide5 {

  /**
   * An ordered collection of elements.
   */
  trait Collection[A] {
    /**
     * Apply an associative function to combine the collection of values to a single value.
     */
    def combine(implicit S: Semigroup[A]): A
  }

  object Collection extends Collections
  trait Collections {
    /**
     * Construct a collection from a sequence of elements.
     */
    def apply[A](x: A*): Collection[A] = new Collection[A] {
      def combine(implicit S: Semigroup[A]): A = x.reduce((a, b) => S.append(a, b))
    }
  }
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/**
 * Usage examples.
 */
class Slide5Spec extends Specification with Slide5 {

  "Combining integers" >> {

    "Can compute the sum" >> {
      val ints = Collection(5, 3, 6, 4)
      ints.combine(Semigroup[Int]) must_== 18
    }

    "Can compute the maximum" >> {
      import Tags._
      val ints = Collection(MaxVal(5), MaxVal(3), MaxVal(6), MaxVal(4))
      ints.combine(Semigroup.maxSemigroup[Int]) must_== MaxVal(6)
    }
  }
}
