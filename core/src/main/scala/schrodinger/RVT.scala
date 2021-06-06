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

import cats.data.AndThen
import cats.effect.kernel._
import cats.{
  ~>,
  Alternative,
  Applicative,
  CommutativeMonad,
  Defer,
  Eval,
  FlatMap,
  Functor,
  FunctorFilter,
  Monad,
  MonadError,
  MonoidK,
  Now,
  SemigroupK
}
import schrodinger.rng.{Rng, SplittableRng}
import schrodinger.kernel.PseudoRandom

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

final class RVT[F[_], S, A](private[schrodinger] val simF: F[S => F[A]]) extends Serializable {

  def simulate(seed: S)(implicit F: FlatMap[F], S: Rng[S]): F[A] =
    unsafeSimulate(S.copy(seed))

  private[schrodinger] def unsafeSimulate(seed: S)(implicit F: FlatMap[F]): F[A] =
    F.flatMap(simF)(_(seed))

  def map[B](f: A => B)(implicit F: Functor[F]): RVT[F, S, B] =
    RVT(F.map(simF)(AndThen(_).andThen(F.map(_)(f))))

  def flatMap[B](f: A => RVT[F, S, B])(implicit F: FlatMap[F]): RVT[F, S, B] =
    RVT(F.map(simF) { sim => seed =>
      AndThen(sim).andThen(sim => F.flatMap(F.flatMap(sim)(f(_).simF))(_(seed)))(seed)
    })

  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): RVT[G, S, A] =
    RVT(f(F.map(simF)(_.andThen(f(_)))))
}

object RVT extends RVTInstances with CommonRVTConstructors {
  private[schrodinger] def apply[F[_], S, A](simF: F[S => F[A]]): RVT[F, S, A] =
    new RVT(simF)

  private[schrodinger] def fromSim[F[_], S, A](sim: S => F[A])(
      implicit F: Applicative[F]): RVT[F, S, A] =
    RVT(F.pure(sim))
}

private[schrodinger] trait CommonRVTConstructors {
  def pure[F[_], S, A](a: A)(implicit F: Applicative[F]): RVT[F, S, A] =
    RVT(F.pure(Function.const(F.pure(a))(_: S)))

  def liftF[F[_], S, A](fa: F[A])(implicit F: Applicative[F]): RVT[F, S, A] =
    RVT(F.map(fa)(a => Function.const(F.pure(a))))

  def fromRV[F[_], S, A](
      rv: RV[S, A])(implicit F0: Applicative[F], F1: Defer[F]): RVT[F, S, A] =
    rv.mapK(new (Eval ~> F) {
      override def apply[A](fa: Eval[A]): F[A] = fa match {
        case Now(a) => F0.pure(a)
        case fa => F1.defer(F0.pure(fa.value))
      }
    })

  def liftK[F[_]: Applicative, S]: F ~> RVT[F, S, *] =
    new (F ~> RVT[F, S, *]) {
      override def apply[A](fa: F[A]): RVT[F, S, A] = liftF(fa)
    }
}

private[schrodinger] class RVTInstances extends RVTInstances0 {
  implicit def schrodingerAsyncForRVT[F[_], S](
      implicit ev1: Async[F],
      ev2: SplittableRng[S]): Async[RVT[F, S, *]] =
    new RVTAsync[F, S] {
      implicit override val F = ev1
      implicit override val S = ev2
    }

  implicit def schrodingerPseudoRandomForRVT[F[_], S](
      implicit ev1: Monad[F],
      ev2: Rng[S]): PseudoRandom.Aux[RVT[F, S, *], F, S] =
    new RVTPseudoRandom[F, S] {
      implicit override val F = ev1
      implicit override val S = ev2
    }
}

sealed private[schrodinger] class RVTInstances0 extends RVTInstances1 {
  implicit def schrodingerSyncForRVT[F[_], S](implicit ev1: Sync[F]): Sync[RVT[F, S, *]] =
    new RVTSync[F, S] {
      implicit override val F = ev1
      override def rootCancelScope = F.rootCancelScope
    }

  implicit def schrodingerTemporalForRVT[F[_], S, E](
      implicit ev1: GenTemporal[F, E],
      ev2: SplittableRng[S]): GenTemporal[RVT[F, S, *], E] =
    new RVTTemporal[F, S, E] {
      implicit override val F = ev1
      implicit override val S = ev2
    }
}

sealed private[schrodinger] class RVTInstances1 extends RVTInstances2 {
  implicit def schrodingerGenConcurrentForRVT[F[_], S, E](
      implicit ev1: GenConcurrent[F, E],
      ev2: SplittableRng[S]): GenConcurrent[RVT[F, S, *], E] =
    new RVTConcurrent[F, S, E] {
      implicit override val F = ev1
      implicit override val S = ev2
    }

  implicit def schrodingerClockForRVT[F[_], S, E](
      implicit ev1: Monad[F],
      ev2: Clock[F]): Clock[RVT[F, S, *]] =
    new RVTClock[F, S] {
      implicit override val F: Monad[F] = ev1
      implicit override val C = ev2
      override def applicative = RVT.schrodingerMonadForRVT[F, S](F)
    }
}

sealed private[schrodinger] class RVTInstances2 extends RVTInstances3 {
  implicit def schrodingerGenSpawnForRVT[F[_], S, E](
      implicit ev1: GenSpawn[F, E],
      ev2: SplittableRng[S]): GenSpawn[RVT[F, S, *], E] =
    new RVTSpawn[F, S, E] {
      implicit override val F = ev1
      implicit override val S = ev2
    }
}

sealed private[schrodinger] class RVTInstances3 extends RVTInstances4 {
  implicit def schrodingerMonadCancelForRVT[F[_], S, E](
      implicit ev1: MonadCancel[F, E]): MonadCancel[RVT[F, S, *], E] =
    new RVTMonadCancel[F, S, E] {
      implicit override val F = ev1
      override def rootCancelScope = F.rootCancelScope
    }
}

sealed abstract private[schrodinger] class RVTInstances4 extends RVTInstances5 {
  implicit def schrodingerDeferForRVT[F[_], S](implicit ev1: Defer[F]): Defer[RVT[F, S, *]] =
    new RVTDefer[F, S] { implicit override val F = ev1 }

  implicit def schrodingerMonadErrorForRVT[F[_], S, E](
      implicit ev1: MonadError[F, E]): MonadError[RVT[F, S, *], E] =
    new RVTMonadError[F, S, E] {
      implicit override val F = ev1
    }
}

sealed abstract private[schrodinger] class RVTInstances5 extends RVTInstances6 {
  implicit def schrodingerCommutativeMonadForRVT[F[_], S](
      implicit ev1: CommutativeMonad[F]): CommutativeMonad[RVT[F, S, *]] =
    new RVTMonad[F, S] with CommutativeMonad[RVT[F, S, *]] { implicit val F = ev1 }
}

sealed abstract private[schrodinger] class RVTInstances6 extends RVTInstances7 {
  implicit def schrodingerDelegatedFunctorFilterForRVT[F[_], S](
      implicit ev1: Monad[F],
      ev2: FunctorFilter[F]): FunctorFilter[RVT[F, S, *]] =
    new RVTFunctorFilter[F, S] {
      override val F0 = ev1
      override val F1 = ev2
    }

  implicit def schrodingerAlternativeForRVT[F[_], S](
      implicit ev1: Monad[F],
      ev2: Alternative[F]): Alternative[RVT[F, S, *]] =
    new RVTAlternative[F, S] {
      implicit override val F = ev1
      implicit override val F1 = ev2
    }
}

sealed abstract private[schrodinger] class RVTInstances7 extends RVTInstances8 {
  implicit def schrodingerMonadForRVT[F[_], S](implicit F0: Monad[F]): Monad[RVT[F, S, *]] =
    new RVTMonad[F, S] { implicit def F = F0 }

  implicit def schrodingerMonoidKForRVT[F[_], S](
      implicit ev1: Monad[F],
      ev2: MonoidK[F]): MonoidK[RVT[F, S, *]] =
    new RVTMonoidK[F, S] {
      implicit override val F = ev1
      implicit override val F1 = ev2
    }
}

sealed abstract private[schrodinger] class RVTInstances8 {
  implicit def schrodingerFunctorForRVT[F[_], S](
      implicit ev1: Functor[F]): Functor[RVT[F, S, *]] =
    new RVTFunctor[F, S] { implicit val F = ev1 }

  implicit def schrodingerSemigroupKForRVT[F[_], S](
      implicit ev1: Monad[F],
      ev2: SemigroupK[F]): SemigroupK[RVT[F, S, *]] =
    new RVTSemigroupK[F, S] {
      implicit override val F = ev1
      implicit override val F1 = ev2
    }
}

sealed private[schrodinger] trait RVTPseudoRandom[F[_], S0]
    extends PseudoRandom[RVT[F, S0, *]] {
  override type G[A] = F[A]
  override type S = S0

  implicit def F: Monad[F]
  implicit def S: Rng[S]

  override def simulate[A](fa: RVT[F, S, A])(seed: S): F[A] =
    fa.simulate(seed)

  override def int: RVT[F, S, Int] =
    RVT.fromSim((s: S) => F.pure(S.unsafeNextInt(s)))

  override def long: RVT[F, S, Long] =
    RVT.fromSim((s: S) => F.pure(S.unsafeNextLong(s)))
}

sealed private[schrodinger] trait RVTFunctor[F[_], S] extends Functor[RVT[F, S, *]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: RVT[F, S, A])(f: A => B): RVT[F, S, B] =
    fa.map(f)
}

sealed private[schrodinger] trait RVTMonad[F[_], S]
    extends RVTFunctor[F, S]
    with Monad[RVT[F, S, *]] {
  implicit def F: Monad[F]

  def pure[A](a: A): RVT[F, S, A] =
    RVT.pure(a)

  def flatMap[A, B](fa: RVT[F, S, A])(f: A => RVT[F, S, B]): RVT[F, S, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: A => RVT[F, S, Either[A, B]]): RVT[F, S, B] =
    RVT.fromSim(s => F.tailRecM(a)(f.andThen(fa => F.flatMap(fa.simF)(_(s)))))
}

private[schrodinger] trait RVTMonadError[F[_], S, E]
    extends RVTMonad[F, S]
    with MonadError[RVT[F, S, *], E] {
  implicit def F: MonadError[F, E]

  override def raiseError[A](e: E): RVT[F, S, A] =
    RVT.liftF(F.raiseError(e))

  override def handleErrorWith[A](fa: RVT[F, S, A])(f: E => RVT[F, S, A]): RVT[F, S, A] =
    RVT.fromSim(s => F.handleErrorWith(fa.unsafeSimulate(s))(f(_).unsafeSimulate(s)))
}

sealed private[schrodinger] trait RVTFunctorFilter[F[_], S]
    extends FunctorFilter[RVT[F, S, *]] {
  implicit def F0: Monad[F]
  implicit def F1: FunctorFilter[F]

  implicit override def functor: Functor[RVT[F, S, *]] =
    RVT.schrodingerFunctorForRVT(F1.functor)

  override def mapFilter[A, B](fa: RVT[F, S, A])(f: A => Option[B]): RVT[F, S, B] =
    RVT(F0.map(fa.simF)(_.andThen(F1.mapFilter(_)(f))))
}

sealed private[schrodinger] trait RVTDefer[F[_], S] extends Defer[RVT[F, S, *]] {
  implicit def F: Defer[F]

  override def defer[A](fa: => RVT[F, S, A]): RVT[F, S, A] =
    RVT(F.defer(fa.simF))
}

sealed private[schrodinger] trait RVTSemigroupK[F[_], S] extends SemigroupK[RVT[F, S, *]] {
  implicit def F: Monad[F]
  implicit def F1: SemigroupK[F]

  def combineK[A](x: RVT[F, S, A], y: RVT[F, S, A]): RVT[F, S, A] =
    RVT.fromSim(s => F1.combineK(x.unsafeSimulate(s), y.unsafeSimulate(s)))
}

sealed private[schrodinger] trait RVTMonoidK[F[_], S]
    extends RVTSemigroupK[F, S]
    with MonoidK[RVT[F, S, *]] {
  implicit def F1: MonoidK[F]

  override def empty[A]: RVT[F, S, A] =
    RVT.liftF(F1.empty[A])
}

sealed private[schrodinger] trait RVTAlternative[F[_], S]
    extends RVTMonad[F, S]
    with RVTSemigroupK[F, S]
    with Alternative[RVT[F, S, *]] {
  implicit def F1: Alternative[F]

  override def empty[A]: RVT[F, S, A] =
    RVT.liftF(F1.empty)
}

sealed private[schrodinger] trait RVTMonadCancel[F[_], S, E]
    extends MonadCancel[RVT[F, S, *], E]
    with RVTMonadError[F, S, E] {

  implicit def F: MonadCancel[F, E]

  def canceled: RVT[F, S, Unit] =
    RVT.liftF(F.canceled)

  def forceR[A, B](fa: RVT[F, S, A])(fb: RVT[F, S, B]): RVT[F, S, B] =
    RVT.fromSim(s => F.forceR(fa.unsafeSimulate(s))(fb.unsafeSimulate(s)))

  def onCancel[A](fa: RVT[F, S, A], fin: RVT[F, S, Unit]): RVT[F, S, A] =
    RVT.fromSim(s => F.onCancel(fa.unsafeSimulate(s), fin.unsafeSimulate(s)))

  def uncancelable[A](body: Poll[RVT[F, S, *]] => RVT[F, S, A]): RVT[F, S, A] =
    RVT(
      F.uncancelable { poll =>
        val poll2 = new Poll[RVT[F, S, *]] {
          def apply[B](fb: RVT[F, S, B]): RVT[F, S, B] =
            RVT(F.map(fb.simF)(sim => (s: S) => poll(sim(s))))
        }

        F.map(body(poll2).simF)(sim => (s: S) => sim(s))
      }
    )

}

sealed private[schrodinger] trait RVTSpawn[F[_], S, E]
    extends GenSpawn[RVT[F, S, *], E]
    with RVTMonadCancel[F, S, E] {
  implicit override def F: GenSpawn[F, E]
  implicit def S: SplittableRng[S]

  override def unique: RVT[F, S, Unique.Token] =
    RVT.liftF(F.unique)

  override def never[A]: RVT[F, S, A] = RVT.liftF(F.never)

  override def cede: RVT[F, S, Unit] = RVT.liftF(F.cede)

  override def start[A](fa: RVT[F, S, A]): RVT[F, S, Fiber[RVT[F, S, *], E, A]] =
    RVT.fromSim(s0 => {
      val s1 = S.unsafeSplit(s0)
      F.map(F.start(fa.unsafeSimulate(s1)))(liftFiber)
    })

  override def racePair[A, B](fa: RVT[F, S, A], fb: RVT[F, S, B]): RVT[
    F,
    S,
    Either[
      (Outcome[RVT[F, S, *], E, A], Fiber[RVT[F, S, *], E, B]),
      (Fiber[RVT[F, S, *], E, A], Outcome[RVT[F, S, *], E, B])]] =
    RVT.fromSim(s0 => {
      val s1 = S.unsafeSplit(s0)
      val s2 = S.unsafeSplit(s0)
      F.map(F.racePair(fa.unsafeSimulate(s1), fb.unsafeSimulate(s2))) {
        case Left((oc, fib)) => Left((liftOutcome(oc), liftFiber(fib)))
        case Right((fib, oc)) => Right((liftFiber(fib), liftOutcome(oc)))
      }
    })

  private def liftFiber[A](fib: Fiber[F, E, A]): Fiber[RVT[F, S, *], E, A] =
    new Fiber[RVT[F, S, *], E, A] {
      def cancel: RVT[F, S, Unit] = RVT.liftF(fib.cancel)
      def join: RVT[F, S, Outcome[RVT[F, S, *], E, A]] =
        RVT.liftF(F.map(fib.join)(liftOutcome))
    }

  private def liftOutcome[A](oc: Outcome[F, E, A]): Outcome[RVT[F, S, *], E, A] =
    oc match {
      case Outcome.Canceled() => Outcome.Canceled()
      case Outcome.Errored(e) => Outcome.Errored(e)
      case Outcome.Succeeded(foa) => Outcome.Succeeded(RVT.liftF(foa))
    }
}

sealed private[schrodinger] trait RVTConcurrent[F[_], S, E]
    extends GenConcurrent[RVT[F, S, *], E]
    with RVTSpawn[F, S, E] {
  implicit override def F: GenConcurrent[F, E]

  override def ref[A](a: A): RVT[F, S, Ref[RVT[F, S, *], A]] =
    RVT.liftF(F.map(F.ref(a))(_.mapK(RVT.liftK)))

  override def deferred[A]: RVT[F, S, Deferred[RVT[F, S, *], A]] =
    RVT.liftF(F.map(F.deferred[A])(_.mapK(RVT.liftK)))
}

sealed private[schrodinger] trait RVTClock[F[_], S] extends Clock[RVT[F, S, *]] {
  implicit def F: Applicative[F]
  implicit def C: Clock[F]

  override def monotonic: RVT[F, S, FiniteDuration] =
    RVT.liftF(C.monotonic)

  override def realTime: RVT[F, S, FiniteDuration] =
    RVT.liftF(C.realTime)
}

sealed private[schrodinger] trait RVTTemporal[F[_], S, E]
    extends GenTemporal[RVT[F, S, *], E]
    with RVTConcurrent[F, S, E]
    with RVTClock[F, S] {
  implicit def F: GenTemporal[F, E]
  def C = F

  def sleep(time: FiniteDuration): RVT[F, S, Unit] =
    RVT.liftF(F.sleep(time))
}

sealed private[schrodinger] trait RVTSync[F[_], S]
    extends Sync[RVT[F, S, *]]
    with RVTMonadCancel[F, S, Throwable]
    with RVTClock[F, S] {
  implicit def F: Sync[F]
  def C = F

  def suspend[A](hint: Sync.Type)(thunk: => A): RVT[F, S, A] =
    RVT.liftF(F.suspend(hint)(thunk))
}

sealed private[schrodinger] trait RVTAsync[F[_], S]
    extends Async[RVT[F, S, *]]
    with RVTSync[F, S]
    with RVTTemporal[F, S, Throwable] {
  async =>
  implicit def F: Async[F]
  final override def C = F

  def cont[K, R](body: Cont[RVT[F, S, *], K, R]): RVT[F, S, R] =
    RVT.fromSim(gen =>
      F.cont(
        new Cont[F, K, R] {

          override def apply[G[_]](implicit G: MonadCancel[G, Throwable])
              : (Either[Throwable, K] => Unit, G[K], F ~> G) => G[R] = {
            implicit val RG = new RVTMonadCancel[G, S, Throwable] {
              implicit override val F = G
              override def rootCancelScope: CancelScope = F.rootCancelScope
            }

            (cb, ha, nat) => {
              val natT: RVT[F, S, *] ~> RVT[G, S, *] =
                new (RVT[F, S, *] ~> RVT[G, S, *]) {
                  override def apply[A](fa: RVT[F, S, A]): RVT[G, S, A] =
                    RVT(nat(F.map(fa.simF)(sim => (s: S) => nat(sim(s)))))
                }

              G.flatMap(body[RVT[G, S, *]].apply(cb, RVT.liftF(ha), natT).simF)(_(gen))
            }
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
}
