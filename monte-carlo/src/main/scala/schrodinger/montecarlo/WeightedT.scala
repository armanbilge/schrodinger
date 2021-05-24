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

package schrodinger.montecarlo

import cats.effect.kernel.Sync.Type
import cats.effect.kernel.{
  Async,
  CancelScope,
  Clock,
  Cont,
  Deferred,
  Fiber,
  GenConcurrent,
  GenSpawn,
  GenTemporal,
  MonadCancel,
  Outcome,
  Poll,
  Ref,
  Sync,
  Unique
}
import cats.{
  ~>,
  Alternative,
  Applicative,
  ApplicativeError,
  Apply,
  Contravariant,
  ContravariantMonoidal,
  Defer,
  Eq,
  Eval,
  Functor,
  Id,
  Invariant,
  Monad,
  MonadError,
  Monoid,
  MonoidK,
  Order,
  Parallel,
  PartialOrder,
  Semigroup,
  SemigroupK,
  Show
}
import cats.syntax.all._
import litter.{ZeroGroup, ZeroMonoid}
import schrodinger.montecarlo.Weighted.{Heavy, Weightless}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

final case class WeightedT[F[_], W, A](value: F[Weighted[W, A]]) {

  def ap[B](f: WeightedT[F, W, A => B])(
      implicit F: Apply[F],
      W0: ZeroMonoid[W],
      W1: Eq[W]): WeightedT[F, W, B] =
    WeightedT(F.map2(f.value, value) { (wf, wa) => wf.flatMap(f => wa.map(f)) })

  def apF[B](f: F[Weighted[W, A => B]])(
      implicit F: Apply[F],
      W0: ZeroMonoid[W],
      W1: Eq[W]): WeightedT[F, W, B] =
    WeightedT(F.map2(f, value) { (wf, wa) => wf.flatMap(f => wa.map(f)) })

  def map[B](f: A => B)(implicit F: Functor[F]): WeightedT[F, W, B] =
    WeightedT(F.map(value)(_.map(f)))

  def imap[B](f: A => B)(g: B => A)(implicit F: Invariant[F]): WeightedT[F, W, B] =
    WeightedT(F.imap(value)(_.map(f))(_.map(g)))

  def mapK[G[_]](f: F ~> G): WeightedT[G, W, A] =
    WeightedT[G, W, A](f(value))

  def contramap[B](f: B => A)(implicit F: Contravariant[F]): WeightedT[F, W, B] =
    WeightedT(F.contramap(value)(_.map(f)))

  def flatMap[B](f: A => WeightedT[F, W, B])(
      implicit F: Monad[F],
      W0: ZeroMonoid[W],
      W1: Eq[W]): WeightedT[F, W, B] =
    WeightedT {
      F.flatMap(value) {
        case weightless @ Weightless(_) => F.pure(weightless)
        case Heavy(wa, a) =>
          F.map(f(a).value) {
            case weightless @ Weightless(_) => weightless
            case Heavy(wb, b) => Weighted(W0.combine(wa, wb), b)
          }
      }
    }

  def flatMapF[B](f: A => F[Weighted[W, B]])(
      implicit F: Monad[F],
      W0: ZeroMonoid[W],
      W1: Eq[W]): WeightedT[F, W, B] =
    flatMap(a => WeightedT(f(a)))

  def importance(
      f: A => W)(implicit F: Applicative[F], W0: ZeroGroup[W], W1: Eq[W]): WeightedT[F, W, A] =
    WeightedT(F.map(value)(_.importance(f)))

  def importanceF(
      f: A => F[W])(implicit F: Monad[F], W0: ZeroGroup[W], W1: Eq[W]): WeightedT[F, W, A] =
    WeightedT {
      F.flatMap(value) {
        case Heavy(w, a) => F.map(f(a))(fa => Weighted(W0.remove(fa, w), a))
        case weightless @ Weightless(_) => F.pure(weightless)
      }
    }

  def show(implicit F: Show[F[Weighted[W, A]]]): String = F.show(value)

  def compare(that: WeightedT[F, W, A])(implicit Ord: Order[F[Weighted[W, A]]]): Int =
    Ord.compare(value, that.value)
}

object WeightedT extends WeightedTInstances {
  def pure[F[_], W: Monoid, A](a: A)(implicit F: Applicative[F]): WeightedT[F, W, A] =
    liftF(F.pure(a))

  def liftF[F[_], W: Monoid, A](fa: F[A])(implicit F: Functor[F]): WeightedT[F, W, A] =
    WeightedT(F.map(fa)(Weighted.pure(_)))

  def liftK[F[_]: Applicative, W: Monoid]: F ~> WeightedT[F, W, *] =
    new (F ~> WeightedT[F, W, *]) {
      def apply[A](a: F[A]): WeightedT[F, W, A] = WeightedT.liftF(a)
    }

  def liftFunctionK[F[_], G[_], A](f: F ~> G): WeightedT[F, A, *] ~> WeightedT[G, A, *] =
    new (WeightedT[F, A, *] ~> WeightedT[G, A, *]) {
      def apply[B](k: WeightedT[F, A, B]): WeightedT[G, A, B] = k.mapK(f)
    }
}

private[montecarlo] class WeightedTInstances extends WeightedTInstances0 {
  implicit def schrodingerEffectAsyncForWeightedT[F[_], W](
      implicit ev1: Async[F],
      ev2: ZeroMonoid[W],
      ev3: Eq[W]): Async[WeightedT[F, W, *]] =
    new WeightedTAsync[F, W] {
      implicit override val F: Async[F] = ev1
      implicit override val W0: ZeroMonoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
    }
}

sealed private[montecarlo] class WeightedTInstances0 extends WeightedTInstances1 {
  implicit def schrodingerEffectSyncForWeightedT[F[_], W](
      implicit ev1: Sync[F],
      ev2: ZeroMonoid[W],
      ev3: Eq[W]): Sync[WeightedT[F, W, *]] =
    new WeightedTSync[F, W] {
      implicit override val F: Sync[F] = ev1
      implicit override val W0: ZeroMonoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
      override def rootCancelScope: CancelScope = F.rootCancelScope
    }

  implicit def schrodingerEffectTemporalForWeightedT[F[_], W, E](
      implicit ev1: GenTemporal[F, E],
      ev2: ZeroMonoid[W],
      ev3: Eq[W]): GenTemporal[WeightedT[F, W, *], E] =
    new WeightedTTemporal[F, W, E] {
      implicit override val F: GenTemporal[F, E] = ev1
      implicit override val W0: ZeroMonoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
    }
}

sealed private[montecarlo] class WeightedTInstances1 extends WeightedTInstances2 {
  implicit def schrodingerEffectGenConcurrentForWeightedT[F[_], W, E](
      implicit ev1: GenConcurrent[F, E],
      ev2: ZeroMonoid[W],
      ev3: Eq[W]): GenConcurrent[WeightedT[F, W, *], E] =
    new WeightedTConcurrent[F, W, E] {
      implicit override val F: GenConcurrent[F, E] = ev1
      implicit override val W0: ZeroMonoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
    }

  implicit def schrodingerEffectClockForWeightedT[F[_], W, E](
      implicit ev1: Applicative[F],
      ev2: Clock[F],
      ev3: ZeroMonoid[W],
      ev4: Eq[W]): Clock[WeightedT[F, W, *]] =
    new WeightedTClock[F, W] {
      implicit override val F: Applicative[F] = ev1
      implicit override val C: Clock[F] = ev2
      implicit override val W0: ZeroMonoid[W] = ev3
      implicit override val W1: Eq[W] = ev4
      override def applicative: Applicative[WeightedT[F, W, *]] =
        WeightedT.schrodingerMonteCarloApplicativeForWeightedT[F, W](F, W0, W1)
    }
}

sealed private[montecarlo] class WeightedTInstances2 extends WeightedTInstances3 {
  implicit def schrodingerEffectGenSpawnForWeightedT[F[_], W, E](
      implicit ev1: GenSpawn[F, E],
      ev2: ZeroMonoid[W],
      ev3: Eq[W]): GenSpawn[WeightedT[F, W, *], E] =
    new WeightedTSpawn[F, W, E] {
      implicit override val F: GenSpawn[F, E] = ev1
      implicit override val W0: ZeroMonoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
    }
}

sealed private[montecarlo] class WeightedTInstances3 extends WeightedTInstances4 {
  implicit def schrodingerEffectMonadCancelForWeightedT[F[_], W, E](
      implicit ev1: MonadCancel[F, E],
      ev2: ZeroMonoid[W],
      ev3: Eq[W]): MonadCancel[WeightedT[F, W, *], E] =
    new WeightedTMonadCancel[F, W, E] {
      implicit override val F: MonadCancel[F, E] = ev1
      implicit override val W0: ZeroMonoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
      override def rootCancelScope: CancelScope = F.rootCancelScope
    }
}

sealed private[montecarlo] class WeightedTInstances4 extends WeightedTInstances5 {

  implicit def schrodingerMonteCarloDeferForWeightedT[F[_], W](
      implicit F: Defer[F]): Defer[WeightedT[F, W, *]] =
    new Defer[WeightedT[F, W, *]] {
      def defer[A](fa: => WeightedT[F, W, A]): WeightedT[F, W, A] =
        WeightedT(F.defer(fa.value))
    }
}

sealed private[montecarlo] class WeightedTInstances5 extends WeightedTInstances6 {
  implicit def schrodingerMonteCarloOrderForWeightedT[F[_], W, A](
      implicit Ord: Order[F[Weighted[W, A]]]): Order[WeightedT[F, W, A]] =
    new Order[WeightedT[F, W, A]] {
      def compare(x: WeightedT[F, W, A], y: WeightedT[F, W, A]): Int = x.compare(y)
    }
}

sealed private[montecarlo] class WeightedTInstances6 extends WeightedTInstances7 {
  implicit def schrodingerMonteCarloPartialOrderForWeightedT[F[_], W, A](
      implicit Ord: PartialOrder[F[Weighted[W, A]]]): PartialOrder[WeightedT[F, W, A]] =
    new PartialOrder[WeightedT[F, W, A]] {
      override def partialCompare(x: WeightedT[F, W, A], y: WeightedT[F, W, A]): Double =
        Ord.partialCompare(x.value, y.value)
    }
}

sealed private[montecarlo] class WeightedTInstances7 extends WeightedTInstances8 {

  implicit def schrodingerMonteCarloMonadErrorForWeightedT[F[_], W, E](
      implicit F0: MonadError[F, E],
      W: ZeroMonoid[W],
      eqW: Eq[W]): MonadError[WeightedT[F, W, *], E] =
    new WeightedTMonadError[F, W, E] {
      implicit val F: MonadError[F, E] = F0
      implicit val W0: ZeroMonoid[W] = W
      implicit val W1: Eq[W] = eqW
    }

  implicit def schrodingerMonteCarloParallelForWeightedT[M[_], W: ZeroMonoid: Eq](
      implicit P: Parallel[M]): Parallel.Aux[WeightedT[M, W, *], WeightedT[P.F, W, *]] =
    new Parallel[WeightedT[M, W, *]] {
      type F[x] = WeightedT[P.F, W, x]
      implicit val monadM: Monad[M] = P.monad

      def applicative: Applicative[WeightedT[P.F, W, *]] =
        schrodingerMonteCarloApplicativeForWeightedT(P.applicative, ZeroMonoid[W], Eq[W])
      def monad: Monad[WeightedT[M, W, *]] = schrodingerMonteCarloMonadForWeightedT

      def sequential: WeightedT[P.F, W, *] ~> WeightedT[M, W, *] =
        new (WeightedT[P.F, W, *] ~> WeightedT[M, W, *]) {
          def apply[A](wfw: WeightedT[P.F, W, A]): WeightedT[M, W, A] = WeightedT(
            P.sequential(wfw.value))
        }

      def parallel: WeightedT[M, W, *] ~> WeightedT[P.F, W, *] =
        new (WeightedT[M, W, *] ~> WeightedT[P.F, W, *]) {
          def apply[A](wmw: WeightedT[M, W, A]): WeightedT[P.F, W, A] = WeightedT(
            P.parallel(wmw.value))
        }
    }

  implicit def schrodingerMonteCarloEqForWeightedTId[W: Eq, A: Eq]: Eq[WeightedT[Id, W, A]] =
    schrodingerMonteCarloEqForWeightedT[Id, W, A]

  implicit def schrodingerMonteCarloShowForWeightedT[F[_], W, A](
      implicit F: Show[F[Weighted[W, A]]]): Show[WeightedT[F, W, A]] =
    new Show[WeightedT[F, W, A]] {
      override def show(f: WeightedT[F, W, A]): String = f.show
    }

  implicit def schrodingerMonteCarloMonoidForWeightedTId[W: ZeroMonoid: Eq, A: Monoid]
      : Monoid[WeightedT[Id, W, A]] =
    schrodingerMonteCarloMonoidForWeightedT[Id, W, A]
}

sealed private[montecarlo] class WeightedTInstances8 extends WeightedTInstances9 {
  implicit def schrodingerMonteCarloMonadForWeightedTId[W: ZeroMonoid: Eq]
      : Monad[WeightedT[Id, W, *]] =
    schrodingerMonteCarloMonadForWeightedT[Id, W]

  implicit def schrodingerMonteCarloEqForWeightedT[F[_], W, A](
      implicit F: Eq[F[Weighted[W, A]]]): Eq[WeightedT[F, W, A]] =
    Eq.by[WeightedT[F, W, A], F[Weighted[W, A]]](_.value)

  implicit def schrodingerMonteCarloSemigroupForWeightedTId[W: ZeroMonoid: Eq, A: Semigroup]
      : Semigroup[WeightedT[Id, W, A]] =
    schrodingerMonteCarloSemigroupForWeightedT[Id, W, A]
}

sealed private[montecarlo] class WeightedTInstances9 extends WeightedTInstances10 {
  implicit def schrodingerMonteCarloMonadForWeightedT[F[_], W](
      implicit F0: Monad[F],
      W: ZeroMonoid[W],
      eqW: Eq[W]): Monad[WeightedT[F, W, *]] =
    new WeightedTMonad[F, W] {
      implicit val F: Monad[F] = F0
      implicit val W0: ZeroMonoid[W] = W
      implicit val W1: Eq[W] = eqW
    }

  implicit def schrodingerMonteCarloMonoidForWeightedT[F[_], W, A](
      implicit W: Monoid[F[Weighted[W, A]]]): Monoid[WeightedT[F, W, A]] =
    new WeightedTMonoid[F, W, A] {
      implicit val F0: Monoid[F[Weighted[W, A]]] = W
    }
}

sealed private[montecarlo] class WeightedTInstances10 extends WeightedTInstances11 {
  implicit def schrodingerMonteCarloSemigroupForWeightedT[F[_], W, A](
      implicit W: Semigroup[F[Weighted[W, A]]]): Semigroup[WeightedT[F, W, A]] =
    new WeightedTSemigroup[F, W, A] {
      implicit val F0: Semigroup[F[Weighted[W, A]]] = W
    }
}

sealed private[montecarlo] class WeightedTInstances11 extends WeightedTInstances12 {
  implicit def schrodingerMonteCarloApplicativeErrorForWeightedT[F[_], W, E](
      implicit F0: ApplicativeError[F, E],
      W: ZeroMonoid[W],
      eqW: Eq[W]): ApplicativeError[WeightedT[F, W, *], E] =
    new WeightedTApplicativeError[F, W, E] {
      implicit val F: ApplicativeError[F, E] = F0
      implicit val W0: ZeroMonoid[W] = W
      implicit val W1: Eq[W] = eqW
    }
}

sealed private[montecarlo] class WeightedTInstances12 extends WeightedTInstances13 {
  implicit def schrodingerMonteCarloAlternativeForWeightedT[F[_], W](
      implicit F0: Alternative[F],
      W: ZeroMonoid[W],
      eqW: Eq[W]): Alternative[WeightedT[F, W, *]] =
    new WeightedTAlternative[F, W] {
      implicit val F: Alternative[F] = F0
      implicit val W0: ZeroMonoid[W] = W
      implicit val W1: Eq[W] = eqW
    }

  implicit def schrodingerMonteCarloContravariantMonoidalForWeightedT[F[_], W](
      implicit F: ContravariantMonoidal[F]): ContravariantMonoidal[WeightedT[F, W, *]] =
    new WeightedTContravariantMonoidal[F, W] {
      implicit val F0: ContravariantMonoidal[F] = F
    }
}

sealed private[montecarlo] class WeightedTInstances13 extends WeightedTInstances14 {
  implicit def schrodingerMonteCarloMonoidKForWeightedT[F[_], W](
      implicit F0: MonoidK[F]): MonoidK[WeightedT[F, W, *]] =
    new WeightedTMonoidK[F, W] {
      implicit val F: MonoidK[F] = F0
    }

  implicit def schrodingerMonteCarloContravariantForWeightedT[F[_], W](
      implicit F: Contravariant[F]): Contravariant[WeightedT[F, W, *]] =
    new WeightedTContravariant[F, W] {
      implicit val F0: Contravariant[F] = F
    }
}

sealed private[montecarlo] class WeightedTInstances14 extends WeightedTInstances15 {
  implicit def schrodingerMonteCarloSemigroupKForWeightedT[F[_], W](
      implicit F0: SemigroupK[F]): SemigroupK[WeightedT[F, W, *]] =
    new WeightedTSemigroupK[F, W] {
      implicit val F: SemigroupK[F] = F0
    }

  implicit def schrodingerMonteCarloApplicativeForWeightedT[F[_], W](
      implicit F0: Applicative[F],
      W: ZeroMonoid[W],
      eqW: Eq[W]): Applicative[WeightedT[F, W, *]] =
    new WeightedTApplicative[F, W] {
      implicit val F: Applicative[F] = F0
      implicit val W0: ZeroMonoid[W] = W
      implicit val W1: Eq[W] = eqW
    }

  implicit def schrodingerMonteCarloInvariantForWeightedT[F[_], W](
      implicit F0: Invariant[F]): Invariant[WeightedT[F, W, *]] =
    new WeightedTInvariant[F, W] { implicit val F = F0 }
}

sealed private[montecarlo] class WeightedTInstances15 {
  implicit def schrodingerMonteCarloApplyForWeightedT[F[_], W](
      implicit F0: Apply[F],
      W: ZeroMonoid[W],
      eqW: Eq[W]): Apply[WeightedT[F, W, *]] =
    new WeightedTApply[F, W] {
      implicit val F: Apply[F] = F0
      implicit val W0: ZeroMonoid[W] = W
      implicit val W1: Eq[W] = eqW
    }
}

sealed private[montecarlo] trait WeightedTFunctor[F[_], W] extends Functor[WeightedT[F, W, *]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: WeightedT[F, W, A])(f: A => B): WeightedT[F, W, B] =
    fa.map(f)
}

sealed private[montecarlo] trait WeightedTContravariant[F[_], W]
    extends Contravariant[WeightedT[F, W, *]] {
  implicit def F0: Contravariant[F]

  override def contramap[A, B](fa: WeightedT[F, W, A])(f: B => A): WeightedT[F, W, B] =
    fa.contramap(f)
}

sealed private[montecarlo] trait WeightedTInvariant[F[_], W]
    extends Invariant[WeightedT[F, W, *]] {
  implicit def F: Invariant[F]

  override def imap[A, B](fa: WeightedT[F, W, A])(f: A => B)(g: B => A): WeightedT[F, W, B] =
    fa.imap(f)(g)
}

sealed private[montecarlo] trait WeightedTApply[F[_], W]
    extends WeightedTFunctor[F, W]
    with Apply[WeightedT[F, W, *]] {
  implicit override def F: Apply[F]
  implicit def W0: ZeroMonoid[W]
  implicit def W1: Eq[W]

  def ap[A, B](f: WeightedT[F, W, A => B])(fa: WeightedT[F, W, A]): WeightedT[F, W, B] =
    fa.ap(f)

  override def map2Eval[A, B, Z](fa: WeightedT[F, W, A], fb: Eval[WeightedT[F, W, B]])(
      f: (A, B) => Z
  ): Eval[WeightedT[F, W, Z]] =
    F.map2Eval(fa.value, fb.map(_.value)) {
      case (Heavy(wa, a), Heavy(wb, b)) =>
        Weighted(W0.combine(wa, wb), f(a, b))
      case _ => Weightless[W]
    }.map(WeightedT(_)) // F may have a lazy map2Eval

  override def product[A, B](
      fa: WeightedT[F, W, A],
      fb: WeightedT[F, W, B]): WeightedT[F, W, (A, B)] =
    WeightedT(F.map(F.product(fa.value, fb.value)) {
      case (Heavy(wa, a), Heavy(wb, b)) =>
        Weighted(W0.combine(wa, wb), (a, b))
      case _ => Weightless[W]
    })
}

sealed private[montecarlo] trait WeightedTApplicative[F[_], W]
    extends WeightedTApply[F, W]
    with Applicative[WeightedT[F, W, *]] {
  implicit override def F: Applicative[F]
  implicit override def W0: ZeroMonoid[W]

  def pure[A](a: A): WeightedT[F, W, A] =
    WeightedT.pure(a)
}

sealed private[montecarlo] trait WeightedTMonad[F[_], W]
    extends WeightedTApplicative[F, W]
    with Monad[WeightedT[F, W, *]] {
  implicit override def F: Monad[F]
  implicit override def W0: ZeroMonoid[W]

  override def flatMap[A, B](fa: WeightedT[F, W, A])(
      f: A => WeightedT[F, W, B]): WeightedT[F, W, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(fn: A => WeightedT[F, W, Either[A, B]]): WeightedT[F, W, B] = {

    def step(wa: Weighted[W, A]): F[Either[Weighted[W, A], Weighted[W, B]]] = wa match {
      case weightless @ Weightless(_) => F.pure(Right(weightless))
      case Heavy(w1, a) =>
        val fwa: F[Weighted[W, Either[A, B]]] = fn(a).value
        F.map(fwa) {
          case weightless @ Weightless(_) => Right(weightless)
          case Heavy(w2, aorb) =>
            val combineW = W0.combine(w1, w2)
            aorb match {
              case Left(a) => Left(Weighted(combineW, a))
              case Right(b) => Right(Weighted(combineW, b))
            }
        }
    }

    WeightedT(F.tailRecM(Weighted.pure(a))(step))
  }
}

sealed private[montecarlo] trait WeightedTApplicativeError[F[_], W, E]
    extends ApplicativeError[WeightedT[F, W, *], E]
    with WeightedTApplicative[F, W] {
  implicit override def F: ApplicativeError[F, E]

  def raiseError[A](e: E): WeightedT[F, W, A] = WeightedT(F.raiseError[Weighted[W, A]](e))

  def handleErrorWith[A](fa: WeightedT[F, W, A])(
      f: E => WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(F.handleErrorWith(fa.value)(e => f(e).value))
}

private[montecarlo] trait WeightedTMonadError[F[_], W, E]
    extends MonadError[WeightedT[F, W, *], E]
    with WeightedTMonad[F, W]
    with WeightedTApplicativeError[F, W, E] {
  implicit override def F: MonadError[F, E]

  override def ap[A, B](f: WeightedT[F, W, A => B])(
      fa: WeightedT[F, W, A]): WeightedT[F, W, B] =
    super[WeightedTMonad].ap(f)(fa)
}

sealed private[montecarlo] trait WeightedTSemigroupK[F[_], W]
    extends SemigroupK[WeightedT[F, W, *]] {
  implicit def F: SemigroupK[F]

  def combineK[A](x: WeightedT[F, W, A], y: WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(F.combineK(x.value, y.value))

  override def combineKEval[A](
      x: WeightedT[F, W, A],
      y: Eval[WeightedT[F, W, A]]): Eval[WeightedT[F, W, A]] =
    F.combineKEval(x.value, y.map(_.value)).map(WeightedT(_))
}

sealed private[montecarlo] trait WeightedTMonoidK[F[_], W]
    extends MonoidK[WeightedT[F, W, *]]
    with WeightedTSemigroupK[F, W] {
  implicit override def F: MonoidK[F]

  def empty[A]: WeightedT[F, W, A] = WeightedT(F.empty)
}

sealed private[montecarlo] trait WeightedTAlternative[F[_], W]
    extends Alternative[WeightedT[F, W, *]]
    with WeightedTMonoidK[F, W]
    with WeightedTApplicative[F, W] {
  implicit override def F: Alternative[F]
}

sealed private[montecarlo] trait WeightedTContravariantMonoidal[F[_], W]
    extends ContravariantMonoidal[WeightedT[F, W, *]] {
  implicit def F0: ContravariantMonoidal[F]

  override def unit: WeightedT[F, W, Unit] = WeightedT(F0.trivial[Weighted[W, Unit]])

  override def contramap[A, B](fa: WeightedT[F, W, A])(f: B => A): WeightedT[F, W, B] =
    fa.contramap(f)

  override def product[A, B](
      fa: WeightedT[F, W, A],
      fb: WeightedT[F, W, B]): WeightedT[F, W, (A, B)] =
    WeightedT(
      F0.contramap(F0.product(fa.value, fb.value)) {
        case Heavy(w, (a, b)) => (Heavy(w, a), Heavy(w, b))
        case weightless @ Weightless(_) => (weightless, weightless)
      }
    )
}

sealed private[montecarlo] trait WeightedTSemigroup[F[_], W, A]
    extends Semigroup[WeightedT[F, W, A]] {
  implicit def F0: Semigroup[F[Weighted[W, A]]]

  def combine(x: WeightedT[F, W, A], y: WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(F0.combine(x.value, y.value))
}

sealed private[montecarlo] trait WeightedTMonoid[F[_], W, A]
    extends Monoid[WeightedT[F, W, A]]
    with WeightedTSemigroup[F, W, A] {
  implicit override def F0: Monoid[F[Weighted[W, A]]]

  def empty: WeightedT[F, W, A] = WeightedT(F0.empty)
}

sealed private[montecarlo] trait WeightedTMonadCancel[F[_], W, E]
    extends WeightedTMonadError[F, W, E]
    with MonadCancel[WeightedT[F, W, *], E] {
  implicit override def F: MonadCancel[F, E]

  override def uncancelable[A](
      body: Poll[WeightedT[F, W, *]] => WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(
      F.uncancelable { nat =>
        val natT =
          new Poll[WeightedT[F, W, *]] {
            def apply[B](optfa: WeightedT[F, W, B]): WeightedT[F, W, B] =
              WeightedT(nat(optfa.value))
          }
        body(natT).value
      }
    )

  // Note that this does not preserve the weight from the finalizer
  override def onCancel[A](
      fa: WeightedT[F, W, A],
      fin: WeightedT[F, W, Unit]): WeightedT[F, W, A] =
    WeightedT(F.onCancel(fa.value, fin.value.void))

  def forceR[A, B](fa: WeightedT[F, W, A])(fb: WeightedT[F, W, B]): WeightedT[F, W, B] =
    WeightedT(F.forceR(fa.value)(fb.value))

  override def canceled: WeightedT[F, W, Unit] = WeightedT.liftF(F.canceled)

}

sealed private[montecarlo] trait WeightedTSpawn[F[_], W, E]
    extends GenSpawn[WeightedT[F, W, *], E]
    with WeightedTMonadCancel[F, W, E] {
  implicit override def F: GenSpawn[F, E]

  def unique: WeightedT[F, W, Unique.Token] =
    WeightedT.liftF(F.unique)

  def start[A](fa: WeightedT[F, W, A]): WeightedT[F, W, Fiber[WeightedT[F, W, *], E, A]] =
    WeightedT.liftF(F.start(fa.value).map(liftFiber))

  def never[A]: WeightedT[F, W, A] = WeightedT.liftF(F.never)

  def cede: WeightedT[F, W, Unit] = WeightedT.liftF(F.cede)

  def racePair[A, B](fa: WeightedT[F, W, A], fb: WeightedT[F, W, B]): WeightedT[
    F,
    W,
    Either[
      (Outcome[WeightedT[F, W, *], E, A], Fiber[WeightedT[F, W, *], E, B]),
      (Fiber[WeightedT[F, W, *], E, A], Outcome[WeightedT[F, W, *], E, B])]] = {
    WeightedT.liftF(
      F.uncancelable(poll =>
        poll(F.racePair(fa.value, fb.value)).map {
          case Left((oc, fib)) => Left((liftOutcome(oc), liftFiber(fib)))
          case Right((fib, oc)) => Right((liftFiber(fib), liftOutcome(oc)))
        })
    )
  }

  override def race[A, B](
      fa: WeightedT[F, W, A],
      fb: WeightedT[F, W, B]): WeightedT[F, W, Either[A, B]] =
    WeightedT(F.race(fa.value, fb.value).map(_.bisequence))

  override def both[A, B](
      fa: WeightedT[F, W, A],
      fb: WeightedT[F, W, B]): WeightedT[F, W, (A, B)] =
    WeightedT(F.both(fa.value, fb.value).map {
      case (Heavy(w1, a), Heavy(w2, b)) => Weighted(w1 |+| w2, a -> b)
      case (weightless @ Weightless(_), _) => weightless
      case (_, weightless @ Weightless(_)) => weightless
    })

  override def raceOutcome[A, B](fa: WeightedT[F, W, A], fb: WeightedT[F, W, B]): WeightedT[
    F,
    W,
    Either[Outcome[WeightedT[F, W, *], E, A], Outcome[WeightedT[F, W, *], E, B]]] =
    WeightedT.liftF(
      F.raceOutcome(fa.value, fb.value).map(_.bimap(liftOutcome(_), liftOutcome(_))))

  override def bothOutcome[A, B](fa: WeightedT[F, W, A], fb: WeightedT[F, W, B]): WeightedT[
    F,
    W,
    (Outcome[WeightedT[F, W, *], E, A], Outcome[WeightedT[F, W, *], E, B])] =
    WeightedT.liftF(
      F.bothOutcome(fa.value, fb.value).map(_.bimap(liftOutcome(_), liftOutcome(_))))

  private def liftOutcome[A](
      oc: Outcome[F, E, Weighted[W, A]]): Outcome[WeightedT[F, W, *], E, A] =
    oc match {
      case Outcome.Canceled() => Outcome.Canceled()
      case Outcome.Errored(e) => Outcome.Errored(e)
      case Outcome.Succeeded(foa) => Outcome.Succeeded(WeightedT(foa))
    }

  private def liftFiber[A](fib: Fiber[F, E, Weighted[W, A]]): Fiber[WeightedT[F, W, *], E, A] =
    new Fiber[WeightedT[F, W, *], E, A] {
      def cancel: WeightedT[F, W, Unit] = WeightedT.liftF(fib.cancel)
      def join: WeightedT[F, W, Outcome[WeightedT[F, W, *], E, A]] =
        WeightedT.liftF(fib.join.map(liftOutcome))
    }

}

sealed private[montecarlo] trait WeightedTConcurrent[F[_], W, E]
    extends GenConcurrent[WeightedT[F, W, *], E]
    with WeightedTSpawn[F, W, E] {
  implicit override def F: GenConcurrent[F, E]

  override def ref[A](a: A): WeightedT[F, W, Ref[WeightedT[F, W, *], A]] =
    WeightedT.liftF(F.map(F.ref(a))(_.mapK(WeightedT.liftK)))

  override def deferred[A]: WeightedT[F, W, Deferred[WeightedT[F, W, *], A]] =
    WeightedT.liftF(F.map(F.deferred[A])(_.mapK(WeightedT.liftK)))

  override def racePair[A, B](fa: WeightedT[F, W, A], fb: WeightedT[F, W, B]): WeightedT[
    F,
    W,
    Either[
      (Outcome[WeightedT[F, W, *], E, A], Fiber[WeightedT[F, W, *], E, B]),
      (Fiber[WeightedT[F, W, *], E, A], Outcome[WeightedT[F, W, *], E, B])]] =
    super.racePair(fa, fb)

}

private[montecarlo] trait WeightedTClock[F[_], W] extends Clock[WeightedT[F, W, *]] {
  implicit def F: Applicative[F]
  implicit def C: Clock[F]
  implicit def W0: ZeroMonoid[W]
  implicit def W1: Eq[W]

  override def monotonic: WeightedT[F, W, FiniteDuration] =
    WeightedT.liftF(C.monotonic)

  override def realTime: WeightedT[F, W, FiniteDuration] = WeightedT.liftF(C.realTime)
}

sealed private[montecarlo] trait WeightedTTemporal[F[_], W, E]
    extends GenTemporal[WeightedT[F, W, *], E]
    with WeightedTConcurrent[F, W, E]
    with WeightedTClock[F, W] {
  implicit def F: GenTemporal[F, E]
  def C = F

  def sleep(time: FiniteDuration): WeightedT[F, W, Unit] = WeightedT.liftF(F.sleep(time))
}

sealed private[montecarlo] trait WeightedTSync[F[_], W]
    extends Sync[WeightedT[F, W, *]]
    with WeightedTMonadCancel[F, W, Throwable]
    with WeightedTClock[F, W] {
  implicit def F: Sync[F]
  def C = F

  def suspend[A](hint: Type)(thunk: => A): WeightedT[F, W, A] =
    WeightedT.liftF(F.suspend(hint)(thunk))
}

sealed private[montecarlo] trait WeightedTAsync[F[_], W]
    extends Async[WeightedT[F, W, *]]
    with WeightedTSync[F, W]
    with WeightedTTemporal[F, W, Throwable] { async =>

  implicit def F: Async[F]

  final override def C = F

  def cont[K, R](body: Cont[WeightedT[F, W, *], K, R]): WeightedT[F, W, R] =
    WeightedT(
      F.cont(
        new Cont[F, K, Weighted[W, R]] {

          override def apply[G[_]](implicit G: MonadCancel[G, Throwable])
              : (Either[Throwable, K] => Unit, G[K], F ~> G) => G[Weighted[W, R]] = {
            implicit val WG = new WeightedTMonadCancel[G, W, Throwable] {
              implicit override val F: MonadCancel[G, Throwable] = G
              implicit override val W0: ZeroMonoid[W] = async.W0
              implicit override val W1: Eq[W] = async.W1
              override def rootCancelScope: CancelScope = F.rootCancelScope
            }

            (cb, ga, nat) => {
              val natT: WeightedT[F, W, *] ~> WeightedT[G, W, *] =
                new (WeightedT[F, W, *] ~> WeightedT[G, W, *]) {

                  override def apply[A](fa: WeightedT[F, W, A]): WeightedT[G, W, A] =
                    WeightedT(nat(fa.value))

                }

              body[WeightedT[G, W, *]].apply(cb, WeightedT.liftF(ga), natT).value
            }
          }
        }
      )
    )

  def evalOn[A](fa: WeightedT[F, W, A], ec: ExecutionContext): WeightedT[F, W, A] =
    WeightedT(F.evalOn(fa.value, ec))

  def executionContext: WeightedT[F, W, ExecutionContext] = WeightedT.liftF(F.executionContext)

  override def unique: WeightedT[F, W, Unique.Token] = super[WeightedTTemporal].unique
  override def never[A]: WeightedT[F, W, A] = super[WeightedTTemporal].never
}
