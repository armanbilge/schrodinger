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

import cats.Alternative
import cats.Applicative
import cats.ApplicativeError
import cats.Apply
import cats.Contravariant
import cats.ContravariantMonoidal
import cats.Defer
import cats.Eq
import cats.Eval
import cats.Functor
import cats.Id
import cats.Invariant
import cats.Monad
import cats.MonadError
import cats.Monoid
import cats.MonoidK
import cats.Order
import cats.Parallel
import cats.PartialOrder
import cats.Semigroup
import cats.SemigroupK
import cats.Show
import cats.effect.kernel.Async
import cats.effect.kernel.CancelScope
import cats.effect.kernel.Clock
import cats.effect.kernel.Cont
import cats.effect.kernel.Deferred
import cats.effect.kernel.Fiber
import cats.effect.kernel.GenConcurrent
import cats.effect.kernel.GenSpawn
import cats.effect.kernel.GenTemporal
import cats.effect.kernel.MonadCancel
import cats.effect.kernel.Outcome
import cats.effect.kernel.Poll
import cats.effect.kernel.Ref
import cats.effect.kernel.Sync
import cats.effect.kernel.Sync.Type
import cats.effect.kernel.Unique
import cats.syntax.all.given
import cats.~>
import litter.ZeroGroup
import litter.ZeroMonoid
import schrodinger.montecarlo.Weighted.Heavy
import schrodinger.montecarlo.Weighted.Weightless

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

opaque type WeightedT[F[_], W, A] = F[Weighted[W, A]]
object WeightedT extends WeightedTInstances:
  def apply[F[_], W, A](fwa: F[Weighted[W, A]]): WeightedT[F, W, A] = fwa

  def pure[F[_], W: Monoid, A](a: A)(using F: Applicative[F]): WeightedT[F, W, A] =
    liftF(F.pure(a))

  def liftF[F[_], W: Monoid, A](fa: F[A])(using F: Functor[F]): WeightedT[F, W, A] =
    WeightedT(F.map(fa)(Weighted.pure(_)))

  def liftK[F[_]: Applicative, W: Monoid]: F ~> WeightedT[F, W, _] =
    new (F ~> WeightedT[F, W, _]):
      def apply[A](a: F[A]): WeightedT[F, W, A] = WeightedT.liftF(a)

  def liftFunctionK[F[_], G[_], A](f: F ~> G): WeightedT[F, A, _] ~> WeightedT[G, A, _] =
    new (WeightedT[F, A, _] ~> WeightedT[G, A, _]):
      def apply[B](k: WeightedT[F, A, B]): WeightedT[G, A, B] = WeightedT.mapK(k)(f)

  extension [F[_], W, A](wfa: WeightedT[F, W, A])

    def value: F[Weighted[W, A]] = wfa

    def map[B](f: A => B)(using F: Functor[F]): WeightedT[F, W, B] =
      WeightedT(F.map(value)(_.map(f)))

    def imap[B](f: A => B)(g: B => A)(using F: Invariant[F]): WeightedT[F, W, B] =
      WeightedT(F.imap(value)(_.map(f))(_.map(g)))

    def mapK[G[_]](f: F ~> G): WeightedT[G, W, A] =
      WeightedT[G, W, A](f(value))

    def contramap[B](f: B => A)(using F: Contravariant[F]): WeightedT[F, W, B] =
      WeightedT(F.contramap(value)(_.map(f)))

    def flatMap[B](f: A => WeightedT[F, W, B])(
        using F: Monad[F],
        W0: ZeroMonoid[W],
        W1: Eq[W]): WeightedT[F, W, B] =
      WeightedT {
        F.flatMap(value) {
          case weightless @ Weightless(_) => F.pure(weightless)
          case Heavy(wa, da, a) =>
            F.map(f(a).value) {
              case weightless @ Weightless(_) => weightless
              case Heavy(wb, db, b) => Weighted(wa |+| wb, da |+| db, b)
            }
        }
      }

    def flatMapF[B](f: A => F[Weighted[W, B]])(
        using F: Monad[F],
        W0: ZeroMonoid[W],
        W1: Eq[W]): WeightedT[F, W, B] =
      WeightedT.flatMap(wfa)(a => WeightedT(f(a)))

    def importance(
        f: A => W)(using F: Applicative[F], W0: ZeroGroup[W], W1: Eq[W]): WeightedT[F, W, A] =
      WeightedT(F.map(value)(_.importance(f)))

    def importanceF(
        f: A => F[W])(using F: Monad[F], W0: ZeroGroup[W], W1: Eq[W]): WeightedT[F, W, A] =
      WeightedT {
        F.flatMap(value) {
          case heavy @ Heavy(_, _, a) => F.map(f(a))(fa => heavy.importance(_ => fa))
          case weightless @ Weightless(_) => F.pure(weightless)
        }
      }

    def show(using F: Show[F[Weighted[W, A]]]): String = F.show(value)

    def compare(that: WeightedT[F, W, A])(using Ord: Order[F[Weighted[W, A]]]): Int =
      Ord.compare(value, that.value)

private[montecarlo] class WeightedTInstances extends WeightedTInstances0:
  given schrodingerEffectAsyncForWeightedT[F[_], W](
      using Async[F],
      ZeroMonoid[W],
      Eq[W]): Async[WeightedT[F, W, _]] =
    new WeightedTAsync[F, W] {}

sealed private[montecarlo] class WeightedTInstances0 extends WeightedTInstances1:
  given schrodingerEffectSyncForWeightedT[F[_], W](
      using Sync[F],
      ZeroMonoid[W],
      Eq[W]): Sync[WeightedT[F, W, _]] =
    new WeightedTSync[F, W]:
      override def rootCancelScope: CancelScope = Sync[F].rootCancelScope

  given schrodingerEffectTemporalForWeightedT[F[_], W, E](
      using GenTemporal[F, E],
      ZeroMonoid[W],
      Eq[W]): GenTemporal[WeightedT[F, W, _], E] =
    new WeightedTTemporal[F, W, E] {}

sealed private[montecarlo] class WeightedTInstances1 extends WeightedTInstances2:
  given schrodingerEffectGenConcurrentForWeightedT[F[_], W, E](
      using GenConcurrent[F, E],
      ZeroMonoid[W],
      Eq[W]): GenConcurrent[WeightedT[F, W, _], E] =
    new WeightedTConcurrent[F, W, E] {}

  given schrodingerEffectClockForWeightedT[F[_], W, E](
      using Applicative[F],
      Clock[F],
      ZeroMonoid[W],
      Eq[W]): Clock[WeightedT[F, W, _]] =
    new WeightedTClock[F, W] {}

sealed private[montecarlo] class WeightedTInstances2 extends WeightedTInstances3:
  given schrodingerEffectGenSpawnForWeightedT[F[_], W, E](
      using ev1: GenSpawn[F, E],
      ev2: ZeroMonoid[W],
      ev3: Eq[W]): GenSpawn[WeightedT[F, W, _], E] =
    new WeightedTSpawn[F, W, E] {}

sealed private[montecarlo] class WeightedTInstances3 extends WeightedTInstances4:
  given schrodingerEffectMonadCancelForWeightedT[F[_], W, E](
      using MonadCancel[F, E],
      ZeroMonoid[W],
      Eq[W]): MonadCancel[WeightedT[F, W, _], E] =
    new WeightedTMonadCancel[F, W, E]:
      override def rootCancelScope: CancelScope = MonadCancel[F].rootCancelScope

sealed private[montecarlo] class WeightedTInstances4 extends WeightedTInstances5:

  given schrodingerMonteCarloDeferForWeightedT[F[_], W](
      using F: Defer[F]): Defer[WeightedT[F, W, _]] with
    def defer[A](fa: => WeightedT[F, W, A]): WeightedT[F, W, A] =
      WeightedT(F.defer(fa.value))

sealed private[montecarlo] class WeightedTInstances5 extends WeightedTInstances6:
  given schrodingerMonteCarloOrderForWeightedT[F[_], W, A](
      using Ord: Order[F[Weighted[W, A]]]): Order[WeightedT[F, W, A]] with
    def compare(x: WeightedT[F, W, A], y: WeightedT[F, W, A]): Int = x.compare(y)

sealed private[montecarlo] class WeightedTInstances6 extends WeightedTInstances7:
  given schrodingerMonteCarloPartialOrderForWeightedT[F[_], W, A](
      using Ord: PartialOrder[F[Weighted[W, A]]]): PartialOrder[WeightedT[F, W, A]] with
    override def partialCompare(x: WeightedT[F, W, A], y: WeightedT[F, W, A]): Double =
      Ord.partialCompare(x.value, y.value)

sealed private[montecarlo] class WeightedTInstances7 extends WeightedTInstances8:

  given schrodingerMonteCarloMonadErrorForWeightedT[F[_], W, E](
      using MonadError[F, E],
      ZeroMonoid[W],
      Eq[W]): MonadError[WeightedT[F, W, _], E] =
    new WeightedTMonadError[F, W, E] {}

  given schrodingerMonteCarloParallelForWeightedT[M[_], W: ZeroMonoid: Eq](
      using P: Parallel[M]): Parallel.Aux[WeightedT[M, W, _], WeightedT[P.F, W, _]] =
    new Parallel[WeightedT[M, W, _]]:
      type F[x] = WeightedT[P.F, W, x]
      given Monad[M] = P.monad

      def applicative: Applicative[WeightedT[P.F, W, _]] =
        schrodingerMonteCarloApplicativeForWeightedT(using P.applicative, ZeroMonoid[W], Eq[W])
      def monad: Monad[WeightedT[M, W, _]] = schrodingerMonteCarloMonadForWeightedT

      def sequential: WeightedT[P.F, W, _] ~> WeightedT[M, W, _] =
        new (WeightedT[P.F, W, _] ~> WeightedT[M, W, _]) {
          def apply[A](wfw: WeightedT[P.F, W, A]): WeightedT[M, W, A] = WeightedT(
            P.sequential(wfw.value))
        }

      def parallel: WeightedT[M, W, _] ~> WeightedT[P.F, W, _] =
        new (WeightedT[M, W, _] ~> WeightedT[P.F, W, _]) {
          def apply[A](wmw: WeightedT[M, W, A]): WeightedT[P.F, W, A] = WeightedT(
            P.parallel(wmw.value))
        }

  given schrodingerMonteCarloEqForWeightedTId[W: Eq, A: Eq]: Eq[WeightedT[Id, W, A]] =
    schrodingerMonteCarloEqForWeightedT[Id, W, A]

  given schrodingerMonteCarloShowForWeightedT[F[_], W, A](
      using Show[F[Weighted[W, A]]]): Show[WeightedT[F, W, A]] with
    override def show(f: WeightedT[F, W, A]): String = f.show

  given schrodingerMonteCarloMonoidForWeightedTId[W: ZeroMonoid: Eq, A: Monoid]
      : Monoid[WeightedT[Id, W, A]] =
    schrodingerMonteCarloMonoidForWeightedT[Id, W, A]

sealed private[montecarlo] class WeightedTInstances8 extends WeightedTInstances9:
  given schrodingerMonteCarloMonadForWeightedTId[W: ZeroMonoid: Eq]
      : Monad[WeightedT[Id, W, _]] =
    given Monad[Id] = cats.catsInstancesForId
    schrodingerMonteCarloMonadForWeightedT[Id, W]

  given schrodingerMonteCarloEqForWeightedT[F[_], W, A](
      using Eq[F[Weighted[W, A]]]): Eq[WeightedT[F, W, A]] =
    Eq.by[WeightedT[F, W, A], F[Weighted[W, A]]](_.value)

  given schrodingerMonteCarloSemigroupForWeightedTId[W: ZeroMonoid: Eq, A: Semigroup]
      : Semigroup[WeightedT[Id, W, A]] =
    schrodingerMonteCarloSemigroupForWeightedT[Id, W, A]

sealed private[montecarlo] class WeightedTInstances9 extends WeightedTInstances10:
  given schrodingerMonteCarloMonadForWeightedT[F[_], W](
      using Monad[F],
      ZeroMonoid[W],
      Eq[W]): Monad[WeightedT[F, W, _]] =
    new WeightedTMonad[F, W] {}

  given schrodingerMonteCarloMonoidForWeightedT[F[_], W, A](
      using Monoid[F[Weighted[W, A]]]): Monoid[WeightedT[F, W, A]] =
    new WeightedTMonoid[F, W, A] {}

sealed private[montecarlo] class WeightedTInstances10 extends WeightedTInstances11:
  given schrodingerMonteCarloSemigroupForWeightedT[F[_], W, A](
      using Semigroup[F[Weighted[W, A]]]): Semigroup[WeightedT[F, W, A]] =
    new WeightedTSemigroup[F, W, A] {}

sealed private[montecarlo] class WeightedTInstances11 extends WeightedTInstances12:
  given schrodingerMonteCarloApplicativeErrorForWeightedT[F[_], W, E](
      using ApplicativeError[F, E],
      ZeroMonoid[W],
      Eq[W]): ApplicativeError[WeightedT[F, W, _], E] =
    new WeightedTApplicativeError[F, W, E] {}

sealed private[montecarlo] class WeightedTInstances12 extends WeightedTInstances13:
  given schrodingerMonteCarloAlternativeForWeightedT[F[_], W](
      using Alternative[F],
      ZeroMonoid[W],
      Eq[W]): Alternative[WeightedT[F, W, _]] =
    new WeightedTAlternative[F, W] {}

  given schrodingerMonteCarloContravariantMonoidalForWeightedT[F[_], W](
      using ContravariantMonoidal[F]): ContravariantMonoidal[WeightedT[F, W, _]] =
    new WeightedTContravariantMonoidal[F, W] {}

sealed private[montecarlo] class WeightedTInstances13 extends WeightedTInstances14:
  given schrodingerMonteCarloMonoidKForWeightedT[F[_], W](
      using MonoidK[F]): MonoidK[WeightedT[F, W, _]] =
    new WeightedTMonoidK[F, W] {}

  given schrodingerMonteCarloContravariantForWeightedT[F[_], W](
      using Contravariant[F]): Contravariant[WeightedT[F, W, _]] =
    new WeightedTContravariant[F, W] {}

sealed private[montecarlo] class WeightedTInstances14 extends WeightedTInstances15:
  given schrodingerMonteCarloSemigroupKForWeightedT[F[_], W](
      using SemigroupK[F]): SemigroupK[WeightedT[F, W, _]] =
    new WeightedTSemigroupK[F, W] {}

  given schrodingerMonteCarloApplicativeForWeightedT[F[_], W](
      using Applicative[F],
      ZeroMonoid[W],
      Eq[W]): Applicative[WeightedT[F, W, _]] =
    new WeightedTApplicative[F, W] {}

  given schrodingerMonteCarloInvariantForWeightedT[F[_], W](
      using Invariant[F]): Invariant[WeightedT[F, W, _]] =
    new WeightedTInvariant[F, W] {}

sealed private[montecarlo] class WeightedTInstances15:
  given schrodingerMonteCarloApplyForWeightedT[F[_], W](
      using Apply[F],
      ZeroMonoid[W],
      Eq[W]): Apply[WeightedT[F, W, _]] =
    new WeightedTApply[F, W] {}

sealed private[montecarlo] trait WeightedTFunctor[F[_], W](using Functor[F])
    extends Functor[WeightedT[F, W, _]]:

  override def map[A, B](fa: WeightedT[F, W, A])(f: A => B): WeightedT[F, W, B] =
    fa.map(f)

sealed private[montecarlo] trait WeightedTContravariant[F[_], W](using Contravariant[F])
    extends Contravariant[WeightedT[F, W, _]]:

  override def contramap[A, B](fa: WeightedT[F, W, A])(f: B => A): WeightedT[F, W, B] =
    fa.contramap(f)

sealed private[montecarlo] trait WeightedTInvariant[F[_], W](using F: Invariant[F])
    extends Invariant[WeightedT[F, W, _]]:

  override def imap[A, B](fa: WeightedT[F, W, A])(f: A => B)(g: B => A): WeightedT[F, W, B] =
    fa.imap(f)(g)

sealed private[montecarlo] trait WeightedTApply[F[_], W](
    using F: Apply[F],
    val W0: ZeroMonoid[W],
    val W1: Eq[W])
    extends WeightedTFunctor[F, W],
      Apply[WeightedT[F, W, _]]:

  def ap[A, B](f: WeightedT[F, W, A => B])(fa: WeightedT[F, W, A]): WeightedT[F, W, B] =
    WeightedT(F.map2(f.value, fa.value) { (wf, wa) => wf.flatMap(f => wa.map(f)) })

  override def map2Eval[A, B, Z](fa: WeightedT[F, W, A], fb: Eval[WeightedT[F, W, B]])(
      f: (A, B) => Z
  ): Eval[WeightedT[F, W, Z]] =
    F.map2Eval(fa.value, fb.map(_.value)) {
      case (Heavy(wa, da, a), Heavy(wb, db, b)) =>
        Weighted(wa |+| wb, da |+| db, f(a, b))
      case _ => Weightless[W]
    }.map(WeightedT(_)) // F may have a lazy map2Eval

  override def product[A, B](
      fa: WeightedT[F, W, A],
      fb: WeightedT[F, W, B]): WeightedT[F, W, (A, B)] =
    WeightedT(F.map(F.product(fa.value, fb.value)) {
      case (Heavy(wa, da, a), Heavy(wb, db, b)) =>
        Weighted(wa |+| wb, da |+| db, (a, b))
      case _ => Weightless[W]
    })

sealed private[montecarlo] trait WeightedTApplicative[F[_], W](
    using F: Applicative[F],
    W0: ZeroMonoid[W])
    extends WeightedTApply[F, W],
      Applicative[WeightedT[F, W, _]]:

  def pure[A](a: A): WeightedT[F, W, A] =
    WeightedT.pure(a)

sealed private[montecarlo] trait WeightedTMonad[F[_], W](using F: Monad[F], W0: ZeroMonoid[W])
    extends WeightedTApplicative[F, W],
      Monad[WeightedT[F, W, _]]:

  override def flatMap[A, B](fa: WeightedT[F, W, A])(
      f: A => WeightedT[F, W, B]): WeightedT[F, W, B] =
    WeightedT.flatMap(fa)(f)

  def tailRecM[A, B](a: A)(fn: A => WeightedT[F, W, Either[A, B]]): WeightedT[F, W, B] =

    def step(wa: Weighted[W, A]): F[Either[Weighted[W, A], Weighted[W, B]]] = wa match
      case weightless @ Weightless(_) => F.pure(Right(weightless))
      case Heavy(w1, d1, a) =>
        val fwa: F[Weighted[W, Either[A, B]]] = fn(a).value
        F.map(fwa) {
          case weightless @ Weightless(_) => Right(weightless)
          case Heavy(w2, d2, aorb) =>
            val w = w1 |+| w2
            val d = d1 |+| d2
            aorb match
              case Left(a) => Left(Weighted(w, d, a))
              case Right(b) => Right(Weighted(w, d, b))
        }

    WeightedT(F.tailRecM(Weighted.pure(a))(step))

sealed private[montecarlo] trait WeightedTApplicativeError[F[_], W, E](
    using F: ApplicativeError[F, E])
    extends ApplicativeError[WeightedT[F, W, _], E],
      WeightedTApplicative[F, W]:

  def raiseError[A](e: E): WeightedT[F, W, A] = WeightedT(F.raiseError[Weighted[W, A]](e))

  def handleErrorWith[A](fa: WeightedT[F, W, A])(
      f: E => WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(F.handleErrorWith(fa.value)(e => f(e).value))

private[montecarlo] trait WeightedTMonadError[F[_], W, E](using F: MonadError[F, E])
    extends MonadError[WeightedT[F, W, _], E],
      WeightedTMonad[F, W],
      WeightedTApplicativeError[F, W, E]:

  override def ap[A, B](f: WeightedT[F, W, A => B])(
      fa: WeightedT[F, W, A]): WeightedT[F, W, B] =
    super[WeightedTMonad].ap(f)(fa)

sealed private[montecarlo] trait WeightedTSemigroupK[F[_], W](using F: SemigroupK[F])
    extends SemigroupK[WeightedT[F, W, _]]:

  def combineK[A](x: WeightedT[F, W, A], y: WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(F.combineK(x.value, y.value))

  override def combineKEval[A](
      x: WeightedT[F, W, A],
      y: Eval[WeightedT[F, W, A]]): Eval[WeightedT[F, W, A]] =
    F.combineKEval(x.value, y.map(_.value)).map(WeightedT(_))

sealed private[montecarlo] trait WeightedTMonoidK[F[_], W](using F: MonoidK[F])
    extends MonoidK[WeightedT[F, W, _]],
      WeightedTSemigroupK[F, W]:

  def empty[A]: WeightedT[F, W, A] = WeightedT(F.empty)

sealed private[montecarlo] trait WeightedTAlternative[F[_], W](using F: Alternative[F])
    extends Alternative[WeightedT[F, W, _]],
      WeightedTMonoidK[F, W],
      WeightedTApplicative[F, W]

sealed private[montecarlo] trait WeightedTContravariantMonoidal[F[_], W](
    using F0: ContravariantMonoidal[F])
    extends ContravariantMonoidal[WeightedT[F, W, _]]:

  override def unit: WeightedT[F, W, Unit] = WeightedT(F0.trivial[Weighted[W, Unit]])

  override def contramap[A, B](fa: WeightedT[F, W, A])(f: B => A): WeightedT[F, W, B] =
    WeightedT.contramap(fa)(f)

  override def product[A, B](
      fa: WeightedT[F, W, A],
      fb: WeightedT[F, W, B]): WeightedT[F, W, (A, B)] =
    WeightedT(
      F0.contramap(F0.product(fa.value, fb.value)) {
        case Heavy(w, d, (a, b)) => (Heavy(w, d, a), Heavy(w, d, b))
        case weightless @ Weightless(_) => (weightless, weightless)
      }
    )

sealed private[montecarlo] trait WeightedTSemigroup[F[_], W, A](
    using Semigroup[F[Weighted[W, A]]])
    extends Semigroup[WeightedT[F, W, A]]:

  def combine(x: WeightedT[F, W, A], y: WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(x.value |+| y.value)

sealed private[montecarlo] trait WeightedTMonoid[F[_], W, A](using F: Monoid[F[Weighted[W, A]]])
    extends Monoid[WeightedT[F, W, A]],
      WeightedTSemigroup[F, W, A]:

  def empty: WeightedT[F, W, A] = WeightedT(F.empty)

sealed private[montecarlo] trait WeightedTMonadCancel[F[_], W, E](using F: MonadCancel[F, E])
    extends WeightedTMonadError[F, W, E],
      MonadCancel[WeightedT[F, W, _], E]:

  override def uncancelable[A](
      body: Poll[WeightedT[F, W, _]] => WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(
      F.uncancelable { nat =>
        val natT =
          new Poll[WeightedT[F, W, _]] {
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
    WeightedT(F.onCancel(fa.value, F.void(fin.value)))

  def forceR[A, B](fa: WeightedT[F, W, A])(fb: WeightedT[F, W, B]): WeightedT[F, W, B] =
    WeightedT(F.forceR(fa.value)(fb.value))

  override def canceled: WeightedT[F, W, Unit] = WeightedT.liftF(F.canceled)

sealed private[montecarlo] trait WeightedTSpawn[F[_], W, E](using F: GenSpawn[F, E])
    extends GenSpawn[WeightedT[F, W, _], E],
      WeightedTMonadCancel[F, W, E]:

  def unique: WeightedT[F, W, Unique.Token] =
    WeightedT.liftF(F.unique)

  def start[A](fa: WeightedT[F, W, A]): WeightedT[F, W, Fiber[WeightedT[F, W, _], E, A]] =
    WeightedT.liftF(F.map(F.start(fa.value))(liftFiber))

  def never[A]: WeightedT[F, W, A] = WeightedT.liftF(F.never)

  def cede: WeightedT[F, W, Unit] = WeightedT.liftF(F.cede)

  def racePair[A, B](fa: WeightedT[F, W, A], fb: WeightedT[F, W, B]): WeightedT[
    F,
    W,
    Either[
      (Outcome[WeightedT[F, W, _], E, A], Fiber[WeightedT[F, W, _], E, B]),
      (Fiber[WeightedT[F, W, _], E, A], Outcome[WeightedT[F, W, _], E, B])]] =
    WeightedT.liftF(
      F.uncancelable(poll =>
        F.map(poll(F.racePair(fa.value, fb.value))) {
          case Left((oc, fib)) => Left((liftOutcome(oc), liftFiber(fib)))
          case Right((fib, oc)) => Right((liftFiber(fib), liftOutcome(oc)))
        })
    )

  override def race[A, B](
      fa: WeightedT[F, W, A],
      fb: WeightedT[F, W, B]): WeightedT[F, W, Either[A, B]] =
    WeightedT(F.map(F.race(fa.value, fb.value))(_.bisequence))

  override def both[A, B](
      fa: WeightedT[F, W, A],
      fb: WeightedT[F, W, B]): WeightedT[F, W, (A, B)] =
    WeightedT(F.map(F.both(fa.value, fb.value)) {
      case (Heavy(w1, d1, a), Heavy(w2, d2, b)) => Weighted(w1 |+| w2, d1 |+| d2, a -> b)
      case (weightless @ Weightless(_), _) => weightless
      case (_, weightless @ Weightless(_)) => weightless
    })

  override def raceOutcome[A, B](fa: WeightedT[F, W, A], fb: WeightedT[F, W, B]): WeightedT[
    F,
    W,
    Either[Outcome[WeightedT[F, W, _], E, A], Outcome[WeightedT[F, W, _], E, B]]] =
    WeightedT.liftF(
      F.map(F.raceOutcome(fa.value, fb.value))(_.bimap(liftOutcome(_), liftOutcome(_))))

  override def bothOutcome[A, B](fa: WeightedT[F, W, A], fb: WeightedT[F, W, B]): WeightedT[
    F,
    W,
    (Outcome[WeightedT[F, W, _], E, A], Outcome[WeightedT[F, W, _], E, B])] =
    WeightedT.liftF(
      F.map(F.bothOutcome(fa.value, fb.value))(_.bimap(liftOutcome(_), liftOutcome(_))))

  private def liftOutcome[A](
      oc: Outcome[F, E, Weighted[W, A]]): Outcome[WeightedT[F, W, _], E, A] =
    oc match
      case Outcome.Canceled() => Outcome.Canceled()
      case Outcome.Errored(e) => Outcome.Errored(e)
      case Outcome.Succeeded(foa) => Outcome.Succeeded(WeightedT(foa))

  private def liftFiber[A](fib: Fiber[F, E, Weighted[W, A]]): Fiber[WeightedT[F, W, _], E, A] =
    new Fiber[WeightedT[F, W, _], E, A]:
      def cancel: WeightedT[F, W, Unit] = WeightedT.liftF(fib.cancel)
      def join: WeightedT[F, W, Outcome[WeightedT[F, W, _], E, A]] =
        WeightedT.liftF(F.map(fib.join)(liftOutcome))

sealed private[montecarlo] trait WeightedTConcurrent[F[_], W, E](using F: GenConcurrent[F, E])
    extends GenConcurrent[WeightedT[F, W, _], E],
      WeightedTSpawn[F, W, E]:

  override def ref[A](a: A): WeightedT[F, W, Ref[WeightedT[F, W, _], A]] =
    WeightedT.liftF(F.map(F.ref(a))(_.mapK(WeightedT.liftK)))

  override def deferred[A]: WeightedT[F, W, Deferred[WeightedT[F, W, _], A]] =
    WeightedT.liftF(F.map(F.deferred[A])(_.mapK(WeightedT.liftK)))

  override def racePair[A, B](fa: WeightedT[F, W, A], fb: WeightedT[F, W, B]): WeightedT[
    F,
    W,
    Either[
      (Outcome[WeightedT[F, W, _], E, A], Fiber[WeightedT[F, W, _], E, B]),
      (Fiber[WeightedT[F, W, _], E, A], Outcome[WeightedT[F, W, _], E, B])]] =
    super.racePair(fa, fb)

private[montecarlo] trait WeightedTClock[F[_], W](
    using F: Applicative[F],
    C: Clock[F],
    W0: ZeroMonoid[W],
    W1: Eq[W]
) extends Clock[WeightedT[F, W, _]]:

  override def applicative: Applicative[WeightedT[F, W, _]] =
    WeightedT.schrodingerMonteCarloApplicativeForWeightedT[F, W]

  override def monotonic: WeightedT[F, W, FiniteDuration] =
    WeightedT.liftF(C.monotonic)

  override def realTime: WeightedT[F, W, FiniteDuration] = WeightedT.liftF(C.realTime)

sealed private[montecarlo] trait WeightedTTemporal[F[_], W, E](using F: GenTemporal[F, E])
    extends GenTemporal[WeightedT[F, W, _], E],
      WeightedTConcurrent[F, W, E],
      WeightedTClock[F, W]:
  def C = F

  override def applicative: Applicative[WeightedT[F, W, _]] =
    WeightedT.schrodingerMonteCarloApplicativeForWeightedT[F, W]

  def sleep(time: FiniteDuration): WeightedT[F, W, Unit] = WeightedT.liftF(F.sleep(time))

sealed private[montecarlo] trait WeightedTSync[F[_], W](using F: Sync[F])
    extends Sync[WeightedT[F, W, _]],
      WeightedTMonadCancel[F, W, Throwable],
      WeightedTClock[F, W]:
  def C = F

  def suspend[A](hint: Type)(thunk: => A): WeightedT[F, W, A] =
    WeightedT.liftF(F.suspend(hint)(thunk))

sealed private[montecarlo] trait WeightedTAsync[F[_], W](using F: Async[F])
    extends Async[WeightedT[F, W, _]],
      WeightedTSync[F, W],
      WeightedTTemporal[F, W, Throwable]:

  final override def C = F

  def cont[K, R](body: Cont[WeightedT[F, W, _], K, R]): WeightedT[F, W, R] =
    WeightedT(
      F.cont(
        new Cont[F, K, Weighted[W, R]] {

          override def apply[G[_]](using G: MonadCancel[G, Throwable])
              : (Either[Throwable, K] => Unit, G[K], F ~> G) => G[Weighted[W, R]] = {
            given WeightedTMonadCancel[G, W, Throwable] with
              override def rootCancelScope: CancelScope = G.rootCancelScope

            (cb, ga, nat) => {
              val natT: WeightedT[F, W, _] ~> WeightedT[G, W, _] =
                new (WeightedT[F, W, _] ~> WeightedT[G, W, _]):
                  override def apply[A](fa: WeightedT[F, W, A]): WeightedT[G, W, A] =
                    WeightedT(nat(fa.value))

              body[WeightedT[G, W, _]].apply(cb, WeightedT.liftF(ga), natT).value
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
