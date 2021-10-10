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

import cats.Alternative
import cats.Applicative
import cats.CommutativeMonad
import cats.Defer
import cats.Eval
import cats.FlatMap
import cats.Functor
import cats.FunctorFilter
import cats.Monad
import cats.MonadError
import cats.MonoidK
import cats.Now
import cats.SemigroupK
import cats.data.AndThen
import cats.effect.kernel.*
import cats.syntax.all.*
import cats.~>
import schrodinger.kernel.PseudoRandom
import schrodinger.random.GaussianCache
import schrodinger.rng.Rng
import schrodinger.rng.SplittableRng

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

import RVT.State

opaque type RVT[F[_], S, A] = F[State[S] => F[A]]

object RVT extends RVTInstances with CommonRVTConstructors:
  private[schrodinger] def apply[F[_], S, A](simF: F[State[S] => F[A]]): RVT[F, S, A] =
    simF

  private[schrodinger] def fromSim[F[_], S, A](sim: State[S] => F[A])(
      using F: Applicative[F]): RVT[F, S, A] =
    RVT(F.pure(sim))

  final private[schrodinger] case class State[S](seed: S, extra: mutable.Map[Any, Any]):
    def unsafeSplit(using SplittableRng[S]): State[S] =
      State(seed.unsafeSplit(), mutable.Map.empty)

  extension [F[_], S, A](rvfa: RVT[F, S, A])
    private[schrodinger] def simF: F[State[S] => F[A]] = rvfa

    def simulate(seed: S)(using FlatMap[F], Rng[S]): F[A] =
      RVT.unsafeSimulate(rvfa)(State(seed.copy, mutable.Map.empty))

    private[schrodinger] def unsafeSimulate(state: State[S])(using F: FlatMap[F]): F[A] =
      F.flatMap(simF)(_(state))

    def map[B](f: A => B)(using F: Functor[F]): RVT[F, S, B] =
      RVT(F.map(simF)(AndThen(_).andThen(F.map(_)(f))))

    def flatMap[B](f: A => RVT[F, S, B])(using F: FlatMap[F]): RVT[F, S, B] =
      RVT(F.map(simF) { sim => seed =>
        AndThen(sim).andThen(sim => F.flatMap(F.flatMap(sim)(f(_).simF))(_(seed)))(seed)
      })

    def mapK[G[_]](f: F ~> G)(using F: Functor[F]): RVT[G, S, A] =
      RVT(f(F.map(simF)(_.andThen(f(_)))))

type RV[S, A] = RVT[Eval, S, A]
object RV

private[schrodinger] trait CommonRVTConstructors:
  def pure[F[_], S, A](a: A)(using F: Applicative[F]): RVT[F, S, A] =
    RVT(F.pure(Function.const(F.pure(a))(_: State[S])))

  def liftF[F[_], S, A](fa: F[A])(using F: Applicative[F]): RVT[F, S, A] =
    RVT(F.map(fa)(a => Function.const(F.pure(a))))

  def fromRV[F[_], S, A](rv: RV[S, A])(using F0: Applicative[F], F1: Defer[F]): RVT[F, S, A] =
    rv.mapK(
      new (Eval ~> F):
        override def apply[A](fa: Eval[A]): F[A] = fa match {
          case Now(a) => F0.pure(a)
          case fa => F1.defer(F0.pure(fa.value))
        }
    )

  def liftK[F[_]: Applicative, S]: F ~> RVT[F, S, _] =
    new (F ~> RVT[F, S, _]):
      override def apply[A](fa: F[A]): RVT[F, S, A] = liftF(fa)

private[schrodinger] class RVTInstances extends RVTInstances0:
  given schrodingerAsyncForRVT[F[_], S](using Async[F], SplittableRng[S]): Async[RVT[F, S, _]] =
    new RVTAsync[F, S] {}

  given schrodingerPseudoRandomForRVT[F[_], S](
      using Monad[F],
      Rng[S]): PseudoRandom.Aux[RVT[F, S, _], F, S] =
    new RVTPseudoRandom[F, S] {}

  given schrodingerGaussianCacheDoubleForRVT[F[_], S](
      using F: Monad[F]): GaussianCache[RVT[F, S, _], Double] with
    def get: RVT[F, S, Double] =
      RVT.fromSim(_.extra.get(this).fold(Double.NaN)(_.asInstanceOf[Double]).pure)
    def set(a: Double): RVT[F, S, Unit] =
      RVT.fromSim { state =>
        state.extra(this) = a
        F.unit
      }

sealed private[schrodinger] class RVTInstances0 extends RVTInstances1:
  given schrodingerSyncForRVT[F[_], S](using F: Sync[F]): Sync[RVT[F, S, _]] =
    new RVTSync[F, S]:
      override def rootCancelScope = F.rootCancelScope

  given schrodingerTemporalForRVT[F[_], S, E](
      using GenTemporal[F, E],
      SplittableRng[S]): GenTemporal[RVT[F, S, _], E] =
    new RVTTemporal[F, S, E] {}

sealed private[schrodinger] class RVTInstances1 extends RVTInstances2:
  given schrodingerGenConcurrentForRVT[F[_], S, E](
      using GenConcurrent[F, E],
      SplittableRng[S]): GenConcurrent[RVT[F, S, _], E] =
    new RVTConcurrent[F, S, E] {}

  given schrodingerClockForRVT[F[_], S, E](using Monad[F], Clock[F]): Clock[RVT[F, S, _]] =
    new RVTClock[F, S] {}

sealed private[schrodinger] class RVTInstances2 extends RVTInstances3:
  given schrodingerGenSpawnForRVT[F[_], S, E](
      using GenSpawn[F, E],
      SplittableRng[S]): GenSpawn[RVT[F, S, _], E] =
    new RVTSpawn[F, S, E] {}

sealed private[schrodinger] class RVTInstances3 extends RVTInstances4:
  given schrodingerMonadCancelForRVT[F[_], S, E](
      using MonadCancel[F, E]): MonadCancel[RVT[F, S, _], E] =
    new RVTMonadCancel[F, S, E]:
      override def rootCancelScope = MonadCancel[F, E].rootCancelScope

sealed abstract private[schrodinger] class RVTInstances4 extends RVTInstances5:
  given schrodingerDeferForRVT[F[_], S](using Defer[F]): Defer[RVT[F, S, _]] =
    new RVTDefer[F, S] {}

  given schrodingerMonadErrorForRVT[F[_], S, E](
      using MonadError[F, E]): MonadError[RVT[F, S, _], E] =
    new RVTMonadError[F, S, E] {}

sealed abstract private[schrodinger] class RVTInstances5 extends RVTInstances6:
  given schrodingerCommutativeMonadForRVT[F[_], S](
      using CommutativeMonad[F]): CommutativeMonad[RVT[F, S, _]] =
    new RVTMonad[F, S] with CommutativeMonad[RVT[F, S, _]]

sealed abstract private[schrodinger] class RVTInstances6 extends RVTInstances7:
  given schrodingerDelegatedFunctorFilterForRVT[F[_], S](
      using Monad[F],
      FunctorFilter[F]): FunctorFilter[RVT[F, S, _]] =
    new RVTFunctorFilter[F, S] {}

  given schrodingerAlternativeForRVT[F[_], S](
      using Monad[F],
      Alternative[F]): Alternative[RVT[F, S, _]] =
    given Applicative[F] = Monad[F]
    new RVTAlternative[F, S] {}

sealed abstract private[schrodinger] class RVTInstances7 extends RVTInstances8:
  given schrodingerMonadForRVT[F[_], S](using Monad[F]): Monad[RVT[F, S, _]] =
    new RVTMonad[F, S] {}

  given schrodingerMonoidKForRVT[F[_], S](using Monad[F], MonoidK[F]): MonoidK[RVT[F, S, _]] =
    new RVTMonoidK[F, S] {}

sealed abstract private[schrodinger] class RVTInstances8:
  given schrodingerFunctorForRVT[F[_], S](using Functor[F]): Functor[RVT[F, S, _]] =
    new RVTFunctor[F, S] {}

  given schrodingerSemigroupKForRVT[F[_], S](
      using Monad[F],
      SemigroupK[F]): SemigroupK[RVT[F, S, _]] =
    new RVTSemigroupK[F, S] {}

sealed private[schrodinger] trait RVTPseudoRandom[F[_], S0](using F: Monad[F], S: Rng[S0])
    extends PseudoRandom[RVT[F, S0, _]]:
  override type G[A] = F[A]
  override type S = S0

  extension [A](fa: RVT[F, S, A])
    def simulate(seed: S): F[A] =
      RVT.simulate(fa)(seed)

  override def int: RVT[F, S, Int] =
    RVT.fromSim(s => F.pure(s.seed.unsafeNextInt()))

  override def long: RVT[F, S, Long] =
    RVT.fromSim(s => F.pure(s.seed.unsafeNextLong()))

sealed private[schrodinger] trait RVTFunctor[F[_], S](using F: Functor[F])
    extends Functor[RVT[F, S, _]]:

  override def map[A, B](fa: RVT[F, S, A])(f: A => B): RVT[F, S, B] =
    RVT.map(fa)(f)

sealed private[schrodinger] trait RVTMonad[F[_], S](using F: Monad[F])
    extends RVTFunctor[F, S],
      Monad[RVT[F, S, _]]:

  def pure[A](a: A): RVT[F, S, A] =
    RVT.pure(a)

  def flatMap[A, B](fa: RVT[F, S, A])(f: A => RVT[F, S, B]): RVT[F, S, B] =
    RVT.flatMap(fa)(f)

  def tailRecM[A, B](a: A)(f: A => RVT[F, S, Either[A, B]]): RVT[F, S, B] =
    RVT.fromSim(s => F.tailRecM(a)(f.andThen(fa => F.flatMap(fa.simF)(_(s)))))

private[schrodinger] trait RVTMonadError[F[_], S, E](using F: MonadError[F, E])
    extends RVTMonad[F, S]
    with MonadError[RVT[F, S, _], E]:

  override def raiseError[A](e: E): RVT[F, S, A] =
    RVT.liftF(F.raiseError(e))

  override def handleErrorWith[A](fa: RVT[F, S, A])(f: E => RVT[F, S, A]): RVT[F, S, A] =
    RVT.fromSim(s => F.handleErrorWith(fa.unsafeSimulate(s))(f(_).unsafeSimulate(s)))

sealed private[schrodinger] trait RVTFunctorFilter[F[_], S](
    using F0: Monad[F],
    F1: FunctorFilter[F])
    extends FunctorFilter[RVT[F, S, _]]:

  def functor: Functor[RVT[F, S, _]] =
    RVT.schrodingerFunctorForRVT(using F1.functor)

  override def mapFilter[A, B](fa: RVT[F, S, A])(f: A => Option[B]): RVT[F, S, B] =
    RVT(F0.map(fa.simF)(_.andThen(F1.mapFilter(_)(f))))

sealed private[schrodinger] trait RVTDefer[F[_], S](using F: Defer[F])
    extends Defer[RVT[F, S, _]]:

  override def defer[A](fa: => RVT[F, S, A]): RVT[F, S, A] =
    RVT(F.defer(fa.simF))

sealed private[schrodinger] trait RVTSemigroupK[F[_], S](using F: Monad[F], F1: SemigroupK[F])
    extends SemigroupK[RVT[F, S, _]]:

  def combineK[A](x: RVT[F, S, A], y: RVT[F, S, A]): RVT[F, S, A] =
    RVT.fromSim(s => F1.combineK(x.unsafeSimulate(s), y.unsafeSimulate(s)))

sealed private[schrodinger] trait RVTMonoidK[F[_], S](using F0: Applicative[F], F1: MonoidK[F])
    extends RVTSemigroupK[F, S],
      MonoidK[RVT[F, S, _]]:

  override def empty[A]: RVT[F, S, A] =
    RVT.liftF(F1.empty[A])

sealed private[schrodinger] trait RVTAlternative[F[_], S](using F1: Alternative[F])
    extends RVTMonad[F, S],
      RVTSemigroupK[F, S],
      Alternative[RVT[F, S, _]]:

  override def empty[A]: RVT[F, S, A] =
    RVT.liftF(F1.empty)

sealed private[schrodinger] trait RVTMonadCancel[F[_], S, E](using F: MonadCancel[F, E])
    extends MonadCancel[RVT[F, S, _], E],
      RVTMonadError[F, S, E]:

  def canceled: RVT[F, S, Unit] =
    RVT.liftF(F.canceled)

  def forceR[A, B](fa: RVT[F, S, A])(fb: RVT[F, S, B]): RVT[F, S, B] =
    RVT.fromSim(s => F.forceR(fa.unsafeSimulate(s))(fb.unsafeSimulate(s)))

  def onCancel[A](fa: RVT[F, S, A], fin: RVT[F, S, Unit]): RVT[F, S, A] =
    RVT.fromSim(s => F.onCancel(fa.unsafeSimulate(s), fin.unsafeSimulate(s)))

  def uncancelable[A](body: Poll[RVT[F, S, _]] => RVT[F, S, A]): RVT[F, S, A] =
    RVT(
      F.uncancelable { poll =>
        val poll2 = new Poll[RVT[F, S, _]] {
          def apply[B](fb: RVT[F, S, B]): RVT[F, S, B] =
            RVT(F.map(fb.simF)(sim => (s: State[S]) => poll(sim(s))))
        }

        F.map(body(poll2).simF)(sim => (s: State[S]) => sim(s))
      }
    )

sealed private[schrodinger] trait RVTSpawn[F[_], S, E](
    using F: GenSpawn[F, E],
    S: SplittableRng[S])
    extends GenSpawn[RVT[F, S, _], E],
      RVTMonadCancel[F, S, E]:

  override def unique: RVT[F, S, Unique.Token] =
    RVT.liftF(F.unique)

  override def never[A]: RVT[F, S, A] = RVT.liftF(F.never)

  override def cede: RVT[F, S, Unit] = RVT.liftF(F.cede)

  override def start[A](fa: RVT[F, S, A]): RVT[F, S, Fiber[RVT[F, S, _], E, A]] =
    RVT.fromSim(s0 => {
      val s1 = s0.unsafeSplit
      F.map(F.start(fa.unsafeSimulate(s1)))(liftFiber)
    })

  override def racePair[A, B](fa: RVT[F, S, A], fb: RVT[F, S, B]): RVT[
    F,
    S,
    Either[
      (Outcome[RVT[F, S, _], E, A], Fiber[RVT[F, S, _], E, B]),
      (Fiber[RVT[F, S, _], E, A], Outcome[RVT[F, S, _], E, B])]] =
    RVT.fromSim(s0 => {
      val s1 = s0.unsafeSplit
      val s2 = s0.unsafeSplit
      F.map(F.racePair(fa.unsafeSimulate(s1), fb.unsafeSimulate(s2))) {
        case Left((oc, fib)) => Left((liftOutcome(oc), liftFiber(fib)))
        case Right((fib, oc)) => Right((liftFiber(fib), liftOutcome(oc)))
      }
    })

  private def liftFiber[A](fib: Fiber[F, E, A]): Fiber[RVT[F, S, _], E, A] =
    new Fiber[RVT[F, S, _], E, A]:
      def cancel: RVT[F, S, Unit] = RVT.liftF(fib.cancel)
      def join: RVT[F, S, Outcome[RVT[F, S, _], E, A]] =
        RVT.liftF(F.map(fib.join)(liftOutcome))

  private def liftOutcome[A](oc: Outcome[F, E, A]): Outcome[RVT[F, S, _], E, A] =
    oc match
      case Outcome.Canceled() => Outcome.Canceled()
      case Outcome.Errored(e) => Outcome.Errored(e)
      case Outcome.Succeeded(foa) => Outcome.Succeeded(RVT.liftF(foa))

sealed private[schrodinger] trait RVTConcurrent[F[_], S, E](using F: GenConcurrent[F, E])
    extends GenConcurrent[RVT[F, S, _], E],
      RVTSpawn[F, S, E]:

  override def ref[A](a: A): RVT[F, S, Ref[RVT[F, S, _], A]] =
    RVT.liftF(F.map(F.ref(a))(_.mapK(RVT.liftK)))

  override def deferred[A]: RVT[F, S, Deferred[RVT[F, S, _], A]] =
    RVT.liftF(F.map(F.deferred[A])(_.mapK(RVT.liftK)))

sealed private[schrodinger] trait RVTClock[F[_], S](using F: Monad[F], C: Clock[F])
    extends Clock[RVT[F, S, _]]:

  override def applicative = RVT.schrodingerMonadForRVT

  override def monotonic: RVT[F, S, FiniteDuration] =
    RVT.liftF(C.monotonic)

  override def realTime: RVT[F, S, FiniteDuration] =
    RVT.liftF(C.realTime)

sealed private[schrodinger] trait RVTTemporal[F[_], S, E](using F: GenTemporal[F, E])
    extends GenTemporal[RVT[F, S, _], E],
      RVTConcurrent[F, S, E],
      RVTClock[F, S]:
  def C = F
  override def applicative = RVT.schrodingerMonadForRVT

  def sleep(time: FiniteDuration): RVT[F, S, Unit] =
    RVT.liftF(F.sleep(time))

sealed private[schrodinger] trait RVTSync[F[_], S](using F: Sync[F])
    extends Sync[RVT[F, S, _]],
      RVTMonadCancel[F, S, Throwable],
      RVTClock[F, S]:
  def C = F

  def suspend[A](hint: Sync.Type)(thunk: => A): RVT[F, S, A] =
    RVT.liftF(F.suspend(hint)(thunk))

sealed private[schrodinger] trait RVTAsync[F[_], S](using F: Async[F])
    extends Async[RVT[F, S, _]],
      RVTSync[F, S],
      RVTTemporal[F, S, Throwable]:
  final override def C = F

  def cont[K, R](body: Cont[RVT[F, S, _], K, R]): RVT[F, S, R] =
    RVT.fromSim(gen =>
      F.cont(
        new Cont[F, K, R]:

          override def apply[G[_]](using G: MonadCancel[G, Throwable])
              : (Either[Throwable, K] => Unit, G[K], F ~> G) => G[R] = {
            given RVTMonadCancel[G, S, Throwable] with
              override def rootCancelScope: CancelScope = G.rootCancelScope

            (cb, ha, nat) => {
              val natT: RVT[F, S, _] ~> RVT[G, S, _] =
                new (RVT[F, S, _] ~> RVT[G, S, _]):
                  override def apply[A](fa: RVT[F, S, A]): RVT[G, S, A] =
                    RVT(nat(F.map(fa.simF)(sim => (s: State[S]) => nat(sim(s)))))

              G.flatMap(body[RVT[G, S, _]].apply(cb, RVT.liftF(ha), natT).simF)(_(gen))
            }
          }
      ))

  def evalOn[A](fa: RVT[F, S, A], ec: ExecutionContext): RVT[F, S, A] =
    RVT.fromSim(s => F.evalOn(fa.unsafeSimulate(s), ec))

  def executionContext: RVT[F, S, ExecutionContext] =
    RVT.liftF(F.executionContext)

  override def unique: RVT[F, S, Unique.Token] =
    super[RVTTemporal].unique
  override def never[A]: RVT[F, S, A] =
    super[RVTTemporal].never
