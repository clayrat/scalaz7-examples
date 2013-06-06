package sandbox

import scalaz._

object distribMonad {

  import Scalaz._
  import scala.util.Random

  abstract class Dist[A] {
    def support: List[A]
    def gen(r: Random): (A, Random)
    def expect(f: A => Double): Double
  }

  object DistMonad extends Monad[Dist] {
    def point[A](a: => A): Dist[A] = new Dist[A] {
      def support = List(a)
      def gen(g: Random) = (a, g)
      def expect(f: (A => Double)) = f(a)
    }
    def bind[A, B](da: Dist[A])(fdb: A => Dist[B]): Dist[B] = new Dist[B] {
      def support = da.support.map(fdb(_).support).flatten
      def gen(g: Random) = da.gen(g) match { case (a, g1) => (fdb(a)).gen(g1) }
      def expect(f: (B => Double)) = da.expect { a => (fdb(a)).expect(f) }
    }
  }

}