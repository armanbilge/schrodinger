/*
 * Copyright 2021 Arman Bilge
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package schrodinger

import cats.data.StateT
import cats.{
  ~>,
  Alternative,
  Applicative,
  CommutativeMonad,
  Defer,
  FlatMap,
  Functor,
  FunctorFilter,
  Id,
  Monad,
  MonadError,
  Monoid,
  MonoidK,
  SemigroupK
}
import schrodinger.CommonDistTConstructors.FromDistPartiallyApplied
import schrodinger.DistT.{*=>, AndThen}
import schrodinger.generators.Generator

import java.io.Serializable
import scala.annotation.tailrec

final class DistT[F[_], A] private (private[schrodinger] val runF: F[Id *=> λ[S => F[(S, A)]]])
    extends Serializable {

  /**
   * Terminal operation: samples from this distribution
   */
  def sample[S: Generator](state: S)(implicit F: FlatMap[F]): F[A] =
    sampler.runA(state)

  /**
   * Terminal operation: binds this distribution to a random generator
   */
  def sampler[S: Generator](implicit F: Functor[F]): StateT[F, S, A] =
    StateT.applyF(F.map(runF)(_.apply))

  def flatMap[B, SC](fas: A => DistT[F, B])(implicit F: FlatMap[F]): DistT[F, B] =
    DistT.applyF(F.map(runF) { sfsa =>
      AndThen(sfsa).andThen(new (λ[S => F[(S, A)]] *=> λ[S => F[(S, B)]]) {
        override def apply[S: Generator](fsa: F[(S, A)]): F[(S, B)] =
          F.flatMap(fsa) {
            case (s, a) =>
              fas(a).sampler.run(s)
          }
      })
    })

  def flatMapF[B](faf: A => F[B])(implicit F: FlatMap[F]): DistT[F, B] =
    DistT.applyF(F.map(runF) { sfsa =>
      AndThen(sfsa).andThen(new (λ[S => F[(S, A)]] *=> λ[S => F[(S, B)]]) {
        override def apply[S: Generator](fsa: F[(S, A)]): F[(S, B)] =
          F.flatMap(fsa) {
            case (s, a) =>
              F.map(faf(a))((s, _))
          }
      })
    })

  def map[B](f: A => B)(implicit F: Functor[F]): DistT[F, B] =
    DistT.applyF(F.map(runF) { sfsa =>
      AndThen(sfsa).andThen(new (λ[S => F[(S, A)]] *=> λ[S => F[(S, B)]]) {
        override def apply[S: Generator](fsa: F[(S, A)]): F[(S, B)] =
          F.map(fsa) { case (s, a) => (s, f(a)) }
      })
    })

  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): DistT[G, A] =
    DistT.applyF(f(F.map(runF) { sfsa =>
      AndThen(sfsa).andThen(new (λ[S => F[(S, A)]] *=> λ[S => G[(S, A)]]) {
        override def apply[S: Generator](fsa: F[(S, A)]): G[(S, A)] = f(fsa)
      })
    }))

}

object DistT extends DistTInstances with CommonDistTConstructors0 {

  private[schrodinger] def apply[F[_], A](f: Id *=> λ[S => F[(S, A)]])(
      implicit F: Applicative[F]): DistT[F, A] =
    applyF(F.pure(f))

  private[schrodinger] def applyF[F[_], A](runF: F[Id *=> λ[S => F[(S, A)]]]): DistT[F, A] =
    new DistT(runF)

  private[schrodinger] trait *=>[-T[_], +R[_]] extends Serializable { self =>
    def apply[S: Generator](t: T[S]): R[S]

    def compose[A[_]](g: A *=> T): A *=> R = new (A *=> R) {
      override def apply[S: Generator](x: A[S]): R[S] = self(g(x))
    }

    def andThen[A[_]](g: R *=> A): T *=> A = new (T *=> A) {
      override def apply[S: Generator](x: T[S]): A[S] = g(self(x))
    }
  }

  sealed abstract private class AndThen[-T[_], +R[_]]
      extends (T *=> R)
      with Product
      with Serializable {

    override def apply[S: Generator](a: T[S]): R[S] = runLoop(a)

    override def andThen[A[_]](g: R *=> A): AndThen[T, A] =
      // Fusing calls up to a certain threshold, using the fusion
      // technique implemented for `cats.effect.IO#map`
      g match {
        case atg: AndThen[R, A] =>
          AndThen.andThen(this, atg)
        case _ =>
          this match {
            case Single(f, index) if index < AndThen.fusionMaxStackDepth =>
              Single(f.andThen(g), index + 1)
            case Concat(left, Single(f, index)) if index < AndThen.fusionMaxStackDepth =>
              Concat(left, Single(f.andThen(g), index + 1))
            case _ =>
              Concat(this, Single(g, 0))
          }
      }

    override def compose[A[_]](g: A *=> T): AndThen[A, R] =
      // Fusing calls up to a certain threshold, using the fusion
      // technique implemented for `cats.effect.IO#map`
      g match {
        case atg: AndThen[A, T] => AndThen.andThen(atg, this)
        case _ =>
          this match {
            case Single(f, index) if index < AndThen.fusionMaxStackDepth =>
              Single(f.compose(g), index + 1)
            case Concat(Single(f, index), right) if index < AndThen.fusionMaxStackDepth =>
              Concat(Single(f.compose(g), index + 1), right)
            case _ =>
              Concat(Single(g, 0), this)
          }
      }

    private def runLoop[S: Generator](start: T[S]): R[S] = {
      @tailrec
      def loop[A[_]](self: AndThen[A, R], current: A[S]): R[S] =
        self match {
          case Single(f, _) => f(current)

          case Concat(Single(f, _), right) =>
            loop(right, f(current))

          case Concat(left @ Concat(_, _), right) =>
            loop(left.rotateAccum(right), current)
        }

      loop(this, start)
    }

    // converts left-leaning to right-leaning
    final protected def rotateAccum[E[_]](_right: AndThen[R, E]): AndThen[T, E] = {
      @tailrec
      def loop[A[_]](left: AndThen[T, A], right: AndThen[A, E]): AndThen[T, E] =
        left match {
          case Concat(left1, right1) =>
            loop(left1, Concat(right1, right))
          case notConcat => Concat(notConcat, right)
        }

      loop(this, _right)
    }

  }

  final private case class Single[-A[_], +B[_]](f: A *=> B, index: Int) extends AndThen[A, B]
  final private case class Concat[-A[_], E[_], +B[_]](left: AndThen[A, E], right: AndThen[E, B])
      extends AndThen[A, B]

  private[DistT] object AndThen {

    def apply[A[_], B[_]](f: A *=> B): AndThen[A, B] =
      f match {
        case ref: AndThen[A, B] @unchecked => ref
        case _ => Single(f, 0)
      }

    val fusionMaxStackDepth = 1 //28

    def andThen[A[_], B[_], C[_]](ab: AndThen[A, B], bc: AndThen[B, C]): AndThen[A, C] =
      ab match {
        case Single(f, indexf) =>
          bc match {
            case Single(g, indexg) =>
              if (indexf + indexg < fusionMaxStackDepth)
                Single(f.andThen(g), indexf + indexg + 1)
              else Concat(ab, bc)

            case Concat(Single(g, indexg), right) if indexf + indexg < fusionMaxStackDepth =>
              Concat(Single(f.andThen(g), indexf + indexg + 1), right)

            case _ => Concat(ab, bc)
          }
        case Concat(leftf, Single(f, indexf)) =>
          bc match {
            case Single(g, indexg) =>
              if (indexf + indexg < fusionMaxStackDepth)
                Concat(leftf, Single(f.andThen(g), indexf + indexg + 1))
              else Concat(ab, bc)

            case Concat(Single(g, indexg), right) if indexf + indexg < fusionMaxStackDepth =>
              Concat(leftf, Concat(Single(f.andThen(g), indexf + indexg + 1), right))

            case _ =>
              Concat(ab, bc)
          }
        case _ => Concat(ab, bc)
      }

  }

}

private[schrodinger] trait CommonDistTConstructors {

  def pure[F[_], A](a: A)(implicit F: Applicative[F]): DistT[F, A] =
    DistT(new (Id *=> λ[S => F[(S, A)]]) {
      override def apply[S: Generator](s: S): F[(S, A)] = F.pure(s, a)
    })

  def liftF[F[_], A](fa: F[A])(implicit F: Applicative[F]): DistT[F, A] =
    DistT(new (Id *=> λ[S => F[(S, A)]]) {
      override def apply[S: Generator](s: S): F[(S, A)] = F.map(fa)((s, _))
    })

  def fromDist[F[_]]: FromDistPartiallyApplied[F] = new FromDistPartiallyApplied

  def liftK[F[_]: Applicative]: F ~> DistT[F, *] =
    new (F ~> DistT[F, *]) { def apply[A](a: F[A]): DistT[F, A] = DistT.liftF(a) }

  def liftFunctionK[F[_]: Monad, G[_]: Applicative](f: F ~> G): DistT[F, *] ~> DistT[G, *] =
    new (DistT[F, *] ~> DistT[G, *]) {
      override def apply[A](fa: DistT[F, A]): DistT[G, A] =
        DistT {
          new (Id *=> λ[S => G[(S, A)]]) {
            override def apply[S: Generator](s: S): G[(S, A)] =
              f(fa.sampler.run(s))
          }
        }
    }

}

private[schrodinger] object CommonDistTConstructors {
  final private[schrodinger] class FromDistPartiallyApplied[F[_]](
      private val dummy: Boolean = true)
      extends AnyVal {
    def apply[A](value: Dist[A])(implicit F: Applicative[F]): DistT[F, A] =
      value.mapK(λ[Id ~> F](F.pure(_)))
  }
}

private[schrodinger] trait CommonDistTConstructors0 extends CommonDistTConstructors {
  def empty[F[_], A](implicit A: Monoid[A], F: Applicative[F]): DistT[F, A] =
    pure(A.empty)
}

sealed abstract private[schrodinger] class DistTInstances extends DistTInstances0 {
  implicit def schrodingerDeferForDistT[F[_]](implicit F: Defer[F]): Defer[DistT[F, *]] =
    new Defer[DistT[F, *]] {
      def defer[A](fa: => DistT[F, A]): DistT[F, A] =
        DistT.applyF(F.defer(fa.runF))
    }

  implicit def schrodingerMonadErrorForDistT[F[_], E](
      implicit ev: MonadError[F, E]): MonadError[DistT[F, *], E] =
    new DistTMonadError[F, E] {
      implicit override def F: MonadError[F, E] = ev
    }
}

sealed abstract private[schrodinger] class DistTInstances0 extends DistTInstances1 {
  implicit def commutativeMonadForDistT[F[_]](
      implicit F0: CommutativeMonad[F]): CommutativeMonad[DistT[F, *]] =
    new DistTMonad[F] with CommutativeMonad[DistT[F, *]] { implicit def F = F0 }
}

sealed abstract private[schrodinger] class DistTInstances1 extends DistTInstances2 {
  implicit def schrodingerDelegatedFunctorFilterForDistT[F[_]](
      implicit ev1: Monad[F],
      ev2: FunctorFilter[F]): FunctorFilter[DistT[F, *]] =
    new DistTDelegatedFunctorFilter[F] {
      override def F0 = ev1
      override def F1 = ev2
    }

  implicit def schrodingerAlternativeForDistT[F[_]](
      implicit ev1: Alternative[F],
      ev2: Monad[F]): Alternative[DistT[F, *]] =
    new DistTAlternative[F] {
      implicit override def F: Monad[F] = ev2
      implicit override def F1: Alternative[F] = ev1
    }
}

sealed abstract private[schrodinger] class DistTInstances2 extends DistTInstances3 {
  implicit def schrodingerMonadForDistT[F[_]](implicit F0: Monad[F]): Monad[DistT[F, *]] =
    new DistTMonad[F] { implicit def F = F0 }

  implicit def schrodingerMonoidKForDistT[F[_]](
      implicit ev1: Monad[F],
      ev2: MonoidK[F]): MonoidK[DistT[F, *]] =
    new DistTMonoidK[F] {
      implicit override def F: Monad[F] = ev1
      implicit override def F1: MonoidK[F] = ev2
    }
}

sealed abstract private[schrodinger] class DistTInstances3 {
  implicit def schrodingerFunctorForDistT[F[_]](implicit F0: Functor[F]): Functor[DistT[F, *]] =
    new DistTFunctor[F] { implicit def F = F0 }

  implicit def schrodingerSemigroupKForDistT[F[_]](
      implicit ev1: Monad[F],
      ev2: SemigroupK[F]): SemigroupK[DistT[F, *]] =
    new DistTSemigroupK[F] {
      implicit override def F: Monad[F] = ev1
      implicit override def F1: SemigroupK[F] = ev2
    }
}

sealed private[schrodinger] trait DistTFunctor[F[_]] extends Functor[DistT[F, *]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: DistT[F, A])(f: A => B): DistT[F, B] =
    fa.map(f)

}

sealed private[schrodinger] trait DistTMonad[F[_]]
    extends DistTFunctor[F]
    with Monad[DistT[F, *]] {
  implicit def F: Monad[F]

  def pure[A](a: A): DistT[F, A] =
    DistT.pure[F, A](a)

  def flatMap[A, B](fa: DistT[F, A])(f: A => DistT[F, B]): DistT[F, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: A => DistT[F, Either[A, B]]): DistT[F, B] =
    DistT(new (Id *=> λ[S => F[(S, B)]]) {
      override def apply[S: Generator](s: S): F[(S, B)] =
        Monad[StateT[F, S, *]].tailRecM(a)(f(_).sampler[S]).run(s)
    })

}

// Unsealed so DistTMonadCancel can pick up from here
private[schrodinger] trait DistTMonadError[F[_], E]
    extends DistTMonad[F]
    with MonadError[DistT[F, *], E] {

  implicit def F: MonadError[F, E]

  override def raiseError[A](e: E): DistT[F, A] =
    DistT.liftF(F.raiseError(e))

  override def handleErrorWith[A](fa: DistT[F, A])(f: E => DistT[F, A]): DistT[F, A] =
    DistT(new (Id *=> λ[S => F[(S, A)]]) {
      override def apply[S](s: S)(implicit rng: Generator[S]): F[(S, A)] = {
        val (s1, s2) = rng.split.run(s)
        F.handleErrorWith(fa.sampler.run(s1))(f(_).sampler.run(s2))
      }
    })

}

sealed private[schrodinger] trait DistTDelegatedFunctorFilter[F[_]]
    extends FunctorFilter[DistT[F, *]] {
  implicit def F0: Monad[F]
  def F1: FunctorFilter[F]

  implicit override def functor: Functor[DistT[F, *]] =
    DistT.schrodingerFunctorForDistT(F1.functor)

  override def mapFilter[A, B](fa: DistT[F, A])(f: A => Option[B]): DistT[F, B] =
    fa.flatMapF(a => F1.mapFilter(F0.pure(a))(f))

}

sealed private[schrodinger] trait DistTSemigroupK[F[_]] extends SemigroupK[DistT[F, *]] {
  implicit def F: Monad[F]
  implicit def F1: SemigroupK[F]

  def combineK[A](x: DistT[F, A], y: DistT[F, A]): DistT[F, A] =
    DistT(new (Id *=> λ[S => F[(S, A)]]) {
      override def apply[S](s: S)(implicit rng: Generator[S]): F[(S, A)] = {
        val (s1, s2) = rng.split.run(s)
        F1.combineK(x.sampler.run(s1), y.sampler.run(s2))
      }
    })
}

sealed private[schrodinger] trait DistTMonoidK[F[_]]
    extends DistTSemigroupK[F]
    with MonoidK[DistT[F, *]] {
  implicit def F1: MonoidK[F]

  override def empty[A]: DistT[F, A] =
    DistT.liftF(F1.empty[A])
}

sealed private[schrodinger] trait DistTAlternative[F[_]]
    extends DistTMonad[F]
    with DistTSemigroupK[F]
    with Alternative[DistT[F, *]] {
  implicit def F1: Alternative[F]

  override def empty[A]: DistT[F, A] =
    DistT.liftF(F1.empty)
}
