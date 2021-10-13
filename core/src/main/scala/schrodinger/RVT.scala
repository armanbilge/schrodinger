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
import cats.CommutativeMonad
import cats.Defer
import cats.FunctorFilter
import cats.Id
import cats.Monad
import cats.MonadError
import cats.MonoidK
import cats.SemigroupK
import cats.StackSafeMonad
import cats.effect.kernel.Async
import cats.effect.kernel.CancelScope
import cats.effect.kernel.Clock
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
import cats.effect.kernel.Unique
import cats.effect.kernel.syntax.all.*
import cats.effect.kernel.Cont as CECont
import cats.syntax.all.*
import cats.~>
import schrodinger.kernel.PseudoRandom
import schrodinger.random.GaussianCache
import schrodinger.unsafe.rng.Rng
import schrodinger.unsafe.rng.SplittableRng

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

import RVT.*

type RV[S, A] = RVT[Id, S, A]
object RV:
  def pure[S, A](a: A): RV[S, A] = RVT.pure(a)
  def int[S]: RV[S, Int] = RVT.int
  def long[S]: RV[S, Long] = RVT.long

sealed abstract class RVT[F[_], S, A]:

  final def map[B](f: A => B): RVT[F, S, B] = FlatMap(this, a => Pure(f(a)))

  final def semiflatMap[B](f: A => F[B]): RVT[F, S, B] = FlatMap(this, a => Eval(f(a)))

  final def flatMap[B](f: A => RVT[F, S, B]): RVT[F, S, B] = FlatMap(this, f)

  final def flatten[B](using f: A <:< RVT[F, S, B]): RVT[F, S, B] = flatMap(f)

  final def split(using S: SplittableRng[S]): RVT[F, S, F[A]] = Split(this, S)

  final def simulate(s: S)(using sim: Simulator[F], S: Rng[S]): F[A] =
    type G[A] = sim.G[A]
    given G: Sync[G] = sim.runtime

    val ga =
      G.delay(s.copy()).product(G.delay(mutable.Map[RVTCache[F, S, Any], Any]())).flatMap {
        (rng, cache) =>

          def go[B](rv: RVT[F[_], S, B]): G[B] = (rv: @unchecked) match

            case Pure(b) => G.pure(b)

            case Eval(fb) => sim.upgrade(fb)

            case FlatMap(rvc, f) => go(rvc).flatMap(c => go(f(c)))

            case Cont(body) =>
              sim.upgrade(
                body(
                  new (RVT[F, S, _] ~> F):
                    def apply[A](rva: RVT[F, S, A]): F[A] = sim.downgrade(go(rva))
                ))

            case NextInt() => G.delay(rng.nextInt())

            case NextLong() => G.delay(rng.nextLong())

            case Split(rvb, given SplittableRng[S]) => G.delay(rng.split()).map(rvb.simulate)

            case Dispatch(given SplittableRng[S]) => G.delay(rng.split())

            case Retrieve(key) =>
              G.delay {
                val k = key.asInstanceOf[RVTCache[F, S, Any]]
                cache.getOrElse(k, k.default).asInstanceOf[B]
              }

            case Store(key, value) =>
              G.delay {
                val k = key.asInstanceOf[RVTCache[F, S, Any]]
                cache(k) = value
              }

          go(this)
      }

    sim.downgrade(ga)

object RVT extends RVTInstances:
  def pure[F[_], S, A](a: A): RVT[F, S, A] = Pure(a)

  def eval[F[_], S, A](fa: F[A]): RVT[F, S, A] = Eval(fa)

  def evalK[F[_], S]: F ~> RVT[F, S, _] =
    new (F ~> RVT[F, S, _]):
      def apply[A](fa: F[A]): RVT[F, S, A] = RVT.eval(fa)

  def int[F[_], S]: RVT[F, S, Int] = NextInt()

  def long[F[_], S]: RVT[F, S, Long] = NextLong()

  def cont[F[_], S, A](body: RVT[F, S, _] ~> F => F[A]): RVT[F, S, A] = Cont(body)

  def defer[F[_], S, A](rva: => RVT[F, S, A]): RVT[F, S, A] =
    pure(()).flatMap(_ => rva)

  def dispatch[F[_], S](using S: SplittableRng[S]): RVT[F, S, S] = Dispatch(S)

  final private case class Pure[F[_], S, A](a: A) extends RVT[F, S, A]
  final private case class Eval[F[_], S, A](fa: F[A]) extends RVT[F, S, A]
  final private case class FlatMap[F[_], S, A, B](rva: RVT[F, S, A], f: A => RVT[F, S, B])
      extends RVT[F, S, B]

  final private case class Cont[F[_], S, A](
      body: RVT[F, S, _] ~> F => F[A]
  ) extends RVT[F, S, A]

  final private case class NextInt[F[_], S]() extends RVT[F, S, Int]
  final private case class NextLong[F[_], S]() extends RVT[F, S, Long]

  final private case class Split[F[_], S, A](rva: RVT[F, S, A], S: SplittableRng[S])
      extends RVT[F, S, F[A]]

  final private case class Dispatch[F[_], S](S: SplittableRng[S]) extends RVT[F, S, S]

  final private[schrodinger] case class Retrieve[F[_], S, A](key: RVTCache[F, S, A])
      extends RVT[F, S, A]
  final private[schrodinger] case class Store[F[_], S, A](key: RVTCache[F, S, A], value: A)
      extends RVT[F, S, Unit]

sealed private[schrodinger] trait RVTInstances extends RVTInstances8:
  given [F[_]: Async, S: SplittableRng]: Async[RVT[F, S, _]] =
    new RVTAsync[F, S] {}

sealed private[schrodinger] trait RVTInstances8 extends RVTInstances7:
  given [F[_], S: SplittableRng, E](using F: GenTemporal[F, E]): GenTemporal[RVT[F, S, _], E] =
    new RVTTemporal[F, S, E] {}

  given [F[_], S: SplittableRng](using F: Sync[F]): Sync[RVT[F, S, _]] with RVTSync[F, S] with
    override def applicative = this
    def rootCancelScope: CancelScope = F.rootCancelScope

sealed private[schrodinger] trait RVTInstances7 extends RVTInstances6:
  given [F[_], S: SplittableRng, E](
      using F: GenConcurrent[F, E]): GenConcurrent[RVT[F, S, _], E] =
    new RVTConcurrent[F, S, E] {}

  given [F[_]: Clock, S, E]: Clock[RVT[F, S, _]] =
    new RVTClock[F, S] {}

sealed private[schrodinger] trait RVTInstances6 extends RVTInstances5:
  given [F[_], S: SplittableRng, E](using F: GenSpawn[F, E]): GenSpawn[RVT[F, S, _], E] =
    new RVTSpawn[F, S, E] {}

sealed private[schrodinger] trait RVTInstances5 extends RVTInstances4:
  given [F[_], S, E](using F: MonadCancel[F, E]): MonadCancel[RVT[F, S, _], E]
    with RVTMonadCancel[F, S, E]
    with
    def rootCancelScope: CancelScope = F.rootCancelScope

sealed private[schrodinger] trait RVTInstances4 extends RVTInstances3:
  given [F[_], S, E](using MonadError[F, E]): MonadError[RVT[F, S, _], E] =
    new RVTMonadError[F, S, E] {}

sealed private[schrodinger] trait RVTInstances3 extends RVTInstances2:
  given [F[_]: CommutativeMonad, S]: CommutativeMonad[RVT[F, S, _]] = new RVTMonad[F, S]
    with CommutativeMonad[RVT[F, S, _]] {}

sealed private[schrodinger] trait RVTInstances2 extends RVTInstances1:
  given [F[_], S]: Monad[RVT[F, S, _]] = new RVTMonad[F, S] {}

sealed private[schrodinger] trait RVTInstances1 extends RVTInstances0:
  given [F[_]: MonoidK, S]: Alternative[RVT[F, S, _]] = new RVTAlternative[F, S] {}

sealed private[schrodinger] trait RVTInstances0:
  given [F[_]: SemigroupK, S]: SemigroupK[RVT[F, S, _]] = new RVTSemigroupK[F, S] {}

  given [F[_]: FunctorFilter, S]: FunctorFilter[RVT[F, S, _]] = new RVTFunctorFilter[F, S] {}

  given [F[_]: Sync, S: Rng]: PseudoRandom.Aux[RVT[F, S, _], F, S] =
    new RVTPseudoRandom[F, S] {}

  given [F[_], S]: GaussianCache[RVT[F, S, _], Double] =
    new RVTGaussianDoubleCache[F, S] {}

sealed private[schrodinger] trait RVTPseudoRandom[F0[_]: Sync, S0: Rng]
    extends PseudoRandom[RVT[F0, S0, _]]:
  type G[X] = F0[X]
  type S = S0

  def int: RVT[G, S, Int] = RVT.int
  def long: RVT[G, S, Long] = RVT.long

  extension [A](rva: RVT[G, S, A]) def simulate(seed: S): G[A] = rva.simulate(seed)

sealed private[schrodinger] trait RVTGaussianDoubleCache[F[_], S]
    extends GaussianCache[RVT[F, S, _], Double]:
  val key = RVTCache[F, S, Double](Double.NaN)
  def get: RVT[F, S, Double] = key.get
  def set(a: Double): RVT[F, S, Unit] = key.set(a)

sealed private[schrodinger] trait RVTRngDispatcher[F[_], S](using S: SplittableRng[S])
    extends RngDispatcher[RVT[F, S, _], S]:
  val rng = S
  val dispatch: RVT[F, S, S] = RVT.dispatch

sealed private[schrodinger] trait RVTMonad[F[_], S] extends StackSafeMonad[RVT[F, S, _]]:

  def pure[A](a: A): RVT[F, S, A] = RVT.pure(a)

  override def map[A, B](rva: RVT[F, S, A])(f: A => B): RVT[F, S, B] = rva.map(f)

  def flatMap[A, B](rva: RVT[F, S, A])(f: A => RVT[F, S, B]): RVT[F, S, B] =
    rva.flatMap(f)

private[schrodinger] trait RVTMonadError[F[_], S, E](using F: MonadError[F, E])
    extends RVTMonad[F, S]
    with MonadError[RVT[F, S, _], E]:

  def raiseError[A](e: E): RVT[F, S, A] = RVT.eval(F.raiseError(e))

  def handleErrorWith[A](rva: RVT[F, S, A])(f: E => RVT[F, S, A]): RVT[F, S, A] =
    RVT.cont { sim => sim(rva).handleErrorWith(e => sim(f(e))) }

sealed private[schrodinger] trait RVTFunctorFilter[F[_], S](using FunctorFilter[F])
    extends FunctorFilter[RVT[F, S, _]]:

  def functor = RVT.given_Monad_RVT

  def mapFilter[A, B](rva: RVT[F, S, A])(f: A => Option[B]): RVT[F, S, B] =
    RVT.cont(sim => sim(rva).mapFilter(f))

sealed private[schrodinger] trait RVTDefer[F[_], S](using F: Defer[F])
    extends Defer[RVT[F, S, _]]:

  def defer[A](rva: => RVT[F, S, A]): RVT[F, S, A] = RVT.defer(rva)

sealed private[schrodinger] trait RVTSemigroupK[F[_], S](using SemigroupK[F])
    extends SemigroupK[RVT[F, S, _]]:

  def combineK[A](rvx: RVT[F, S, A], rvy: RVT[F, S, A]): RVT[F, S, A] =
    RVT.cont(sim => sim(rvx) <+> sim(rvy))

sealed private[schrodinger] trait RVTAlternative[F[_], S](using F: MonoidK[F])
    extends RVTMonad[F, S],
      RVTSemigroupK[F, S],
      Alternative[RVT[F, S, _]]:

  def empty[A]: RVT[F, S, A] = RVT.eval(F.empty)

sealed private[schrodinger] trait RVTMonadCancel[F[_], S, E](using F: MonadCancel[F, E])
    extends RVTMonadError[F, S, E],
      MonadCancel[RVT[F, S, _], E]:

  val canceled: RVT[F, S, Unit] = RVT.eval(F.canceled)

  def forceR[A, B](rva: RVT[F, S, A])(rvb: RVT[F, S, B]): RVT[F, S, B] =
    RVT.cont(sim => sim(rva).forceR(sim(rvb)))

  def onCancel[A](rva: RVT[F, S, A], fin: RVT[F, S, Unit]): RVT[F, S, A] =
    RVT.cont(sim => sim(rva).onCancel(sim(fin)))

  def uncancelable[A](body: Poll[RVT[F, S, _]] => RVT[F, S, A]): RVT[F, S, A] =
    RVT.cont { sim =>
      F.uncancelable { fpoll =>
        val rvpoll = new Poll[RVT[F, S, _]] {
          def apply[B](rvb: RVT[F, S, B]): RVT[F, S, B] =
            RVT.cont(sim => fpoll(sim(rvb)))
        }
        sim(body(rvpoll))
      }
    }

sealed private[schrodinger] trait RVTSpawn[F[_], S, E](
    using F: GenSpawn[F, E],
    S: SplittableRng[S])
    extends GenSpawn[RVT[F, S, _], E],
      RVTMonadCancel[F, S, E]:

  val unique: RVT[F, S, Unique.Token] = RVT.eval(F.unique)

  def never[A]: RVT[F, S, A] = RVT.eval(F.never)

  val cede: RVT[F, S, Unit] = RVT.eval(F.cede)

  def start[A](rva: RVT[F, S, A]): RVT[F, S, Fiber[RVT[F, S, _], E, A]] =
    rva.split.semiflatMap(_.start.map(liftFiber))

  def racePair[A, B](rva: RVT[F, S, A], rvb: RVT[F, S, B]): RVT[
    F,
    S,
    Either[
      (Outcome[RVT[F, S, _], E, A], Fiber[RVT[F, S, _], E, B]),
      (Fiber[RVT[F, S, _], E, A], Outcome[RVT[F, S, _], E, B])]] =
    rva.split.product(rvb.split).semiflatMap { (fa, fb) =>
      F.racePair(fa, fb).map {
        case Left((oc, fib)) => Left((liftOutcome(oc), liftFiber(fib)))
        case Right((fib, oc)) => Right((liftFiber(fib), liftOutcome(oc)))
      }
    }

  private def liftFiber[A](fib: Fiber[F, E, A]): Fiber[RVT[F, S, _], E, A] =
    new Fiber[RVT[F, S, _], E, A]:
      def cancel: RVT[F, S, Unit] = RVT.eval(fib.cancel)
      def join: RVT[F, S, Outcome[RVT[F, S, _], E, A]] =
        RVT.eval(F.map(fib.join)(liftOutcome))

  private def liftOutcome[A](oc: Outcome[F, E, A]): Outcome[RVT[F, S, _], E, A] =
    oc match
      case Outcome.Canceled() => Outcome.Canceled()
      case Outcome.Errored(e) => Outcome.Errored(e)
      case Outcome.Succeeded(foa) => Outcome.Succeeded(RVT.eval(foa))

sealed private[schrodinger] trait RVTConcurrent[F[_], S, E](using F: GenConcurrent[F, E])
    extends RVTSpawn[F, S, E],
      GenConcurrent[RVT[F, S, _], E]:

  def ref[A](a: A): RVT[F, S, Ref[RVT[F, S, _], A]] =
    RVT.eval(F.ref[A](a).map(_.mapK(RVT.evalK)))

  def deferred[A]: RVT[F, S, Deferred[RVT[F, S, _], A]] =
    RVT.eval(F.deferred[A].map(_.mapK(RVT.evalK)))

sealed private[schrodinger] trait RVTClock[F[_], S](using F: Clock[F])
    extends Clock[RVT[F, S, _]]:

  def applicative = RVT.given_Monad_RVT

  val monotonic: RVT[F, S, FiniteDuration] = RVT.eval(F.monotonic)

  val realTime: RVT[F, S, FiniteDuration] = RVT.eval(F.realTime)

sealed private[schrodinger] trait RVTTemporal[F[_], S, E](using F: GenTemporal[F, E])
    extends RVTConcurrent[F, S, E],
      RVTClock[F, S],
      GenTemporal[RVT[F, S, _], E]:
  def sleep(time: FiniteDuration): RVT[F, S, Unit] = RVT.eval(F.sleep(time))

sealed private[schrodinger] trait RVTSync[F[_], S](using F: Sync[F])
    extends RVTMonadCancel[F, S, Throwable],
      RVTClock[F, S],
      Sync[RVT[F, S, _]]:

  def suspend[A](hint: Sync.Type)(thunk: => A): RVT[F, S, A] =
    RVT.eval(F.suspend(hint)(thunk))

sealed private[schrodinger] trait RVTAsync[F[_], S: SplittableRng](using F: Async[F])
    extends RVTSync[F, S],
      RVTTemporal[F, S, Throwable],
      Async[RVT[F, S, _]]:

  def cont[K, R](body: CECont[RVT[F, S, _], K, R]): RVT[F, S, R] =
    RVT.cont { sim =>
      F.cont {
        new CECont[F, K, R]:
          def apply[G[_]](using G: MonadCancel[G, Throwable])
              : (Either[Throwable, K] => Unit, G[K], F ~> G) => G[R] =
            (cb, gk, nat) => body[G].apply(cb, gk, sim.andThen(nat))
      }
    }

  def evalOn[A](rva: RVT[F, S, A], ec: ExecutionContext): RVT[F, S, A] =
    RVT.cont(sim => F.evalOn(sim(rva), ec))

  val executionContext: RVT[F, S, ExecutionContext] =
    RVT.eval(F.executionContext)

  override val unique: RVT[F, S, Unique.Token] = RVT.eval(F.unique)
  override def never[A]: RVT[F, S, A] = RVT.eval(F.never)
