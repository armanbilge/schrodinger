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

package schrodinger.effect.instances

import cats.effect.kernel.Sync.Type
import cats.effect.kernel._
import cats.effect.kernel.{
  Async,
  Clock,
  Cont,
  Deferred,
  GenConcurrent,
  GenSpawn,
  GenTemporal,
  MonadCancel,
  Ref,
  Sync
}
import cats.syntax.all._
import cats.{~>, Applicative, Eq}
import schrodinger.Binoid
import schrodinger.data.Weighted.{Heavy, Weightless}
import schrodinger.data.{Weighted, WeightedT, WeightedTMonadError}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

private[instances] trait WeightedTInstances extends WeightedTInstances0 {
  implicit def schrodingerEffectAsyncForWeightedT[F[_], W](
      implicit ev1: Async[F],
      ev2: Binoid[W],
      ev3: Eq[W]): Async[WeightedT[F, W, *]] =
    new WeightedTAsync[F, W] {
      implicit override val F: Async[F] = ev1
      implicit override val W0: Binoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
    }
}

sealed private[instances] trait WeightedTInstances0 extends WeightedTInstances1 {
  implicit def schrodingerEffectSyncForWeightedT[F[_], W](
      implicit ev1: Sync[F],
      ev2: Binoid[W],
      ev3: Eq[W]): Sync[WeightedT[F, W, *]] =
    new WeightedTSync[F, W] {
      implicit override val F: Sync[F] = ev1
      implicit override val W0: Binoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
      override def rootCancelScope: CancelScope = F.rootCancelScope
    }

  implicit def schrodingerEffectTemporalForWeightedT[F[_], W, E](
      implicit ev1: GenTemporal[F, E],
      ev2: Binoid[W],
      ev3: Eq[W]): GenTemporal[WeightedT[F, W, *], E] =
    new WeightedTTemporal[F, W, E] {
      implicit override val F: GenTemporal[F, E] = ev1
      implicit override val W0: Binoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
    }
}

sealed private[instances] trait WeightedTInstances1 extends WeightedTInstances2 {
  implicit def schrodingerEffectGenConcurrentForWeightedT[F[_], W, E](
      implicit ev1: GenConcurrent[F, E],
      ev2: Binoid[W],
      ev3: Eq[W]): GenConcurrent[WeightedT[F, W, *], E] =
    new WeightedTConcurrent[F, W, E] {
      implicit override val F: GenConcurrent[F, E] = ev1
      implicit override val W0: Binoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
    }

  implicit def schrodingerEffectClockForWeightedT[F[_], W, E](
      implicit ev1: Applicative[F],
      ev2: Clock[F],
      ev3: Binoid[W],
      ev4: Eq[W]): Clock[WeightedT[F, W, *]] =
    new WeightedTClock[F, W] {
      implicit override val F: Applicative[F] = ev1
      implicit override val C: Clock[F] = ev2
      implicit override val W0: Binoid[W] = ev3
      implicit override val W1: Eq[W] = ev4
      override def applicative: Applicative[WeightedT[F, W, *]] =
        WeightedT.schrodingerDataApplicativeForWeightedT[F, W](F, W0, W1)
    }
}

sealed private[instances] trait WeightedTInstances2 extends WeightedTInstances3 {
  implicit def schrodingerEffectGenSpawnForWeightedT[F[_], W, E](
      implicit ev1: GenSpawn[F, E],
      ev2: Binoid[W],
      ev3: Eq[W]): GenSpawn[WeightedT[F, W, *], E] =
    new WeightedTSpawn[F, W, E] {
      implicit override val F: GenSpawn[F, E] = ev1
      implicit override val W0: Binoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
    }
}

sealed private[instances] trait WeightedTInstances3 {
  implicit def schrodingerEffectMonadCancelForWeightedT[F[_], W, E](
      implicit ev1: MonadCancel[F, E],
      ev2: Binoid[W],
      ev3: Eq[W]): MonadCancel[WeightedT[F, W, *], E] =
    new WeightedTMonadCancel[F, W, E] {
      implicit override val F: MonadCancel[F, E] = ev1
      implicit override val W0: Binoid[W] = ev2
      implicit override val W1: Eq[W] = ev3
      override def rootCancelScope: CancelScope = F.rootCancelScope
    }
}

sealed private[instances] trait WeightedTMonadCancel[F[_], W, E]
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

sealed private[instances] trait WeightedTSpawn[F[_], W, E]
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

sealed private[instances] trait WeightedTConcurrent[F[_], W, E]
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

private[instances] trait WeightedTClock[F[_], W] extends Clock[WeightedT[F, W, *]] {
  implicit def F: Applicative[F]
  implicit def C: Clock[F]
  implicit def W0: Binoid[W]
  implicit def W1: Eq[W]

  override def monotonic: WeightedT[F, W, FiniteDuration] =
    WeightedT.liftF(C.monotonic)

  override def realTime: WeightedT[F, W, FiniteDuration] = WeightedT.liftF(C.realTime)
}

sealed private[instances] trait WeightedTTemporal[F[_], W, E]
    extends GenTemporal[WeightedT[F, W, *], E]
    with WeightedTConcurrent[F, W, E]
    with WeightedTClock[F, W] {
  implicit def F: GenTemporal[F, E]
  def C = F

  def sleep(time: FiniteDuration): WeightedT[F, W, Unit] = WeightedT.liftF(F.sleep(time))
}

sealed private[instances] trait WeightedTSync[F[_], W]
    extends Sync[WeightedT[F, W, *]]
    with WeightedTMonadCancel[F, W, Throwable]
    with WeightedTClock[F, W] {
  implicit def F: Sync[F]
  def C = F

  def suspend[A](hint: Type)(thunk: => A): WeightedT[F, W, A] =
    WeightedT.liftF(F.suspend(hint)(thunk))
}

private[effect] trait WeightedTAsync[F[_], W]
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
              implicit override val W0: Binoid[W] = async.W0
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
