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

import cats.{
  ~>,
  Alternative,
  Applicative,
  CommutativeMonad,
  Defer,
  FlatMap,
  Functor,
  FunctorFilter,
  Monad,
  MonadError,
  Monoid,
  MonoidK,
  SemigroupK
}
import cats.data.{Kleisli, StateT}
import schrodinger.CommonRandomTConstructors.FromRandomPartiallyApplied
import schrodinger.generators.Split

final case class RandomT[F[_], S, A] private (sampler: StateT[F, S, A]) extends Serializable {

  /**
   * Terminal operation: simulates this random variable
   */
  def simulate(state: S)(implicit F: FlatMap[F]): F[A] =
    sampler.runA(state)

  def flatMap[B](fas: A => RandomT[F, S, B])(implicit F: FlatMap[F]): RandomT[F, S, B] =
    RandomT(sampler.flatMap(fas.andThen(_.sampler)))

  def flatMapF[B](faf: A => F[B])(implicit F: FlatMap[F]): RandomT[F, S, B] =
    RandomT(sampler.flatMapF(faf))

  def map[B](f: A => B)(implicit F: Functor[F]): RandomT[F, S, B] =
    RandomT(sampler.map(f))

  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): RandomT[G, S, A] =
    RandomT(sampler.mapK(f))

}

object RandomT extends RandomTInstances with CommonRandomTConstructors0

private[schrodinger] trait CommonRandomTConstructors {

  def pure[F[_], S, A](a: A)(implicit F: Applicative[F]): RandomT[F, S, A] =
    RandomT(StateT.pure(a))

  def liftF[F[_], S, A](fa: F[A])(implicit F: Applicative[F]): RandomT[F, S, A] =
    RandomT(StateT.liftF(fa))

  def fromRandom[F[_], S]: FromRandomPartiallyApplied[F, S] = new FromRandomPartiallyApplied

  def liftK[F[_]: Applicative, S]: F ~> RandomT[F, S, *] =
    StateT.liftK[F, S].andThen(Lambda[StateT[F, S, *] ~> RandomT[F, S, *]](RandomT(_)))

}

private[schrodinger] object CommonRandomTConstructors {
  final private[schrodinger] class FromRandomPartiallyApplied[F[_], S](
      private val dummy: Boolean = true)
      extends AnyVal {
    def apply[A](value: Random[S, A])(implicit F: Applicative[F]): RandomT[F, S, A] =
      RandomT(StateT(value.sampler.runF.andThen(F.pure)))
  }
}

private[schrodinger] trait CommonRandomTConstructors0 extends CommonRandomTConstructors {
  def empty[F[_], S, A](implicit A: Monoid[A], F: Applicative[F]): RandomT[F, S, A] =
    pure(A.empty)
}

sealed abstract private[schrodinger] class RandomTInstances extends RandomTInstances0 {
  implicit def schrodingerDeferForRandomT[F[_], S](
      implicit F: Defer[F]): Defer[RandomT[F, S, *]] =
    new Defer[RandomT[F, S, *]] {
      def defer[A](fa: => RandomT[F, S, A]): RandomT[F, S, A] =
        RandomT(Defer[StateT[F, S, *]].defer(fa.sampler))
    }

  implicit def schrodingerMonadErrorForRandomT[F[_], S, E](
      implicit ev: MonadError[F, E],
      ev2: Split[F, S]): MonadError[RandomT[F, S, *], E] =
    new RandomTMonadError[F, S, E] {
      implicit override def F: MonadError[F, E] = ev
      implicit override def S: Split[F, S] = ev2
    }
}

sealed abstract private[schrodinger] class RandomTInstances0 extends RandomTInstances1 {
  implicit def schrodingerCommutativeMonadForRandomT[F[_], S](
      implicit F0: CommutativeMonad[F]): CommutativeMonad[RandomT[F, S, *]] =
    new RandomTMonad[F, S] with CommutativeMonad[RandomT[F, S, *]] { implicit def F = F0 }
}

sealed abstract private[schrodinger] class RandomTInstances1 extends RandomTInstances2 {
  implicit def schrodingerDelegatedFunctorFilterForRandomT[F[_], S](
      implicit ev1: Monad[F],
      ev2: FunctorFilter[F]): FunctorFilter[RandomT[F, S, *]] =
    new RandomTDelegatedFunctorFilter[F, S] {
      override def F0 = ev1
      override def F1 = ev2
    }

  implicit def schrodingerAlternativeForRandomT[F[_], S](
      implicit ev1: Alternative[F],
      ev2: Monad[F],
      ev3: Split[F, S]): Alternative[RandomT[F, S, *]] =
    new RandomTAlternative[F, S] {
      implicit override def F: Monad[F] = ev2
      implicit override def F1: Alternative[F] = ev1
      implicit override def S: Split[F, S] = ev3
    }
}

sealed abstract private[schrodinger] class RandomTInstances2 extends RandomTInstances3 {
  implicit def schrodingerMonadForRandomT[F[_], S](
      implicit F0: Monad[F]): Monad[RandomT[F, S, *]] =
    new RandomTMonad[F, S] { implicit def F = F0 }

  implicit def schrodingerMonoidKForRandomT[F[_], S](
      implicit ev1: Monad[F],
      ev2: MonoidK[F],
      ev3: Split[F, S]): MonoidK[RandomT[F, S, *]] =
    new RandomTMonoidK[F, S] {
      implicit override def F: Monad[F] = ev1
      implicit override def F1: MonoidK[F] = ev2
      implicit override def S: Split[F, S] = ev3
    }
}

sealed abstract private[schrodinger] class RandomTInstances3 {
  implicit def schrodingerFunctorForRandomT[F[_], S](
      implicit F0: Functor[F]): Functor[RandomT[F, S, *]] =
    new RandomTFunctor[F, S] { implicit def F = F0 }

  implicit def schrodingerSemigroupKForRandomT[F[_], S](
      implicit ev1: Monad[F],
      ev2: SemigroupK[F],
      ev3: Split[F, S]): SemigroupK[RandomT[F, S, *]] =
    new RandomTSemigroupK[F, S] {
      implicit override def F: Monad[F] = ev1
      implicit override def F1: SemigroupK[F] = ev2
      implicit override def S: Split[F, S] = ev3
    }
}

sealed private[schrodinger] trait RandomTFunctor[F[_], S] extends Functor[RandomT[F, S, *]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: RandomT[F, S, A])(f: A => B): RandomT[F, S, B] =
    fa.map(f)
}

sealed private[schrodinger] trait RandomTMonad[F[_], S]
    extends RandomTFunctor[F, S]
    with Monad[RandomT[F, S, *]] {
  implicit def F: Monad[F]

  def pure[A](a: A): RandomT[F, S, A] =
    RandomT.pure[F, S, A](a)

  def flatMap[A, B](fa: RandomT[F, S, A])(f: A => RandomT[F, S, B]): RandomT[F, S, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: A => RandomT[F, S, Either[A, B]]): RandomT[F, S, B] =
    RandomT(Monad[StateT[F, S, *]].tailRecM(a)(f.andThen(_.sampler)))
}

// Unsealed so RandomTMonadCancel can pick up from here
private[schrodinger] trait RandomTMonadError[F[_], S, E]
    extends RandomTMonad[F, S]
    with MonadError[RandomT[F, S, *], E] {

  implicit def F: MonadError[F, E]
  implicit def S: Split[F, S]

  override def raiseError[A](e: E): RandomT[F, S, A] =
    RandomT.liftF(F.raiseError(e))

  override def handleErrorWith[A](fa: RandomT[F, S, A])(
      f: E => RandomT[F, S, A]): RandomT[F, S, A] =
    RandomT(
      StateT.applyF(
        F.map(S.split.sampler.runF) { g =>
          Kleisli(g).andThen { s =>
            F.handleErrorWith(fa.sampler.run(s._1))(f(_).sampler.run(s._2))
          }.run
        }
      )
    )

}

sealed private[schrodinger] trait RandomTDelegatedFunctorFilter[F[_], S]
    extends FunctorFilter[RandomT[F, S, *]] {
  implicit def F0: Monad[F]
  implicit def F1: FunctorFilter[F]

  implicit override def functor: Functor[RandomT[F, S, *]] =
    RandomT.schrodingerFunctorForRandomT(F1.functor)

  override def mapFilter[A, B](fa: RandomT[F, S, A])(f: A => Option[B]): RandomT[F, S, B] =
    RandomT(FunctorFilter[StateT[F, S, *]].mapFilter(fa.sampler)(f))
}

sealed private[schrodinger] trait RandomTSemigroupK[F[_], S]
    extends SemigroupK[RandomT[F, S, *]] {
  implicit def F: Monad[F]
  implicit def F1: SemigroupK[F]
  implicit def S: Split[F, S]

  def combineK[A](x: RandomT[F, S, A], y: RandomT[F, S, A]): RandomT[F, S, A] =
    RandomT(
      StateT.applyF(
        F.map(S.split.sampler.runF) { g =>
          Kleisli(g).andThen { s => F1.combineK(x.sampler.run(s._1), y.sampler.run(s._2)) }.run
        }
      )
    )
}

sealed private[schrodinger] trait RandomTMonoidK[F[_], S]
    extends RandomTSemigroupK[F, S]
    with MonoidK[RandomT[F, S, *]] {
  implicit def F1: MonoidK[F]

  override def empty[A]: RandomT[F, S, A] =
    RandomT.liftF(F1.empty[A])
}

sealed private[schrodinger] trait RandomTAlternative[F[_], S]
    extends RandomTMonad[F, S]
    with RandomTSemigroupK[F, S]
    with Alternative[RandomT[F, S, *]] {
  implicit def F1: Alternative[F]

  override def empty[A]: RandomT[F, S, A] =
    RandomT.liftF(F1.empty)
}
