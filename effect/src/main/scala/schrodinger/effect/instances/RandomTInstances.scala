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

import cats.data.{Kleisli, StateT}
import cats.effect.kernel.Sync.Type
import cats.{~>, Applicative, Functor, FunctorFilter, Id, Monad}
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
import schrodinger.generators.Split
import schrodinger.{RandomT, RandomTMonadError}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

private[instances] trait RandomTInstances extends RandomTInstances0 {
  implicit def schrodingerEffectAsyncForRandomT[F[_], S](
      implicit ev1: Async[F],
      ev2: SplitMonadCancel[S]): Async[RandomT[F, S, *]] =
    new RandomTAsync[F, S] {
      implicit override val F: Async[F] = ev1
      override implicit val S1: SplitMonadCancel[S] = ev2
    }

  implicit def schrodingerEffectFunctorFilterForRandomT[F[_], S](
      implicit ev1: Sync[RandomT[F, S, *]]): FunctorFilter[RandomT[F, S, *]] =
    new RandomTFunctorFilter[F, S] {
      override def sync = ev1
    }
}

sealed private[instances] trait RandomTInstances0 extends RandomTInstances1 {
  implicit def schrodingerEffectSyncForRandomT[F[_], S](
      implicit ev1: Sync[F],
      ev2: Split[F, S]): Sync[RandomT[F, S, *]] =
    new RandomTSync[F, S] {
      implicit override val F: Sync[F] = ev1
      override implicit val S: Split[F, S] = ev2
      override def rootCancelScope: CancelScope = F.rootCancelScope
    }

  implicit def schrodingerEffectTemporalForRandomT[F[_], S, E](
      implicit ev1: GenTemporal[F, E],
      ev2: Split[F, S]): GenTemporal[RandomT[F, S, *], E] =
    new RandomTTemporal[F, S, E] {
      implicit override val F: GenTemporal[F, E] = ev1
      override implicit val S: Split[F, S] = ev2
    }
}

sealed private[instances] trait RandomTInstances1 extends RandomTInstances2 {
  implicit def schrodingerEffectGenConcurrentForRandomT[F[_], S, E](
      implicit ev1: GenConcurrent[F, E],
      ev2: Split[F, S]): GenConcurrent[RandomT[F, S, *], E] =
    new RandomTConcurrent[F, S, E] {
      implicit override val F: GenConcurrent[F, E] = ev1
      override implicit val S: Split[F, S] = ev2
    }

  implicit def schrodingerEffectClockForRandomT[F[_], S, E](
      implicit ev1: Monad[F],
      ev2: Clock[F]): Clock[RandomT[F, S, *]] =
    new RandomTClock[F, S] {
      implicit override val F: Monad[F] = ev1
      implicit override val C: Clock[F] = ev2
      override def applicative: Applicative[RandomT[F, S, *]] =
        RandomT.schrodingerMonadForRandomT[F, S](F)
    }
}

sealed private[instances] trait RandomTInstances2 extends RandomTInstances3 {
  implicit def schrodingerEffectGenSpawnForRandomT[F[_], S, E](
      implicit ev1: GenSpawn[F, E],
      ev2: Split[F, S]): GenSpawn[RandomT[F, S, *], E] =
    new RandomTSpawn[F, S, E] {
      implicit override val F: GenSpawn[F, E] = ev1
      override implicit val S: Split[F, S] = ev2
    }
}

sealed private[instances] trait RandomTInstances3 {
  implicit def schrodingerEffectMonadCancelForRandomT[F[_], S, E](
      implicit ev1: MonadCancel[F, E],
      ev2: Split[F, S]): MonadCancel[RandomT[F, S, *], E] =
    new RandomTMonadCancel[F, S, E] {
      implicit override val F: MonadCancel[F, E] = ev1
      override implicit val S: Split[F, S] = ev2
      override def rootCancelScope: CancelScope = F.rootCancelScope
    }
}

sealed private[instances] trait RandomTMonadCancel[F[_], S, E]
    extends MonadCancel[RandomT[F, S, *], E]
    with RandomTMonadError[F, S, E] {

  implicit def F: MonadCancel[F, E]

  def canceled: RandomT[F, S, Unit] =
    RandomT.liftF(F.canceled)

  def forceR[A, B](fa: RandomT[F, S, A])(fb: RandomT[F, S, B]): RandomT[F, S, B] =
    RandomT(
      StateT.applyF(
        F.map(S.split.sampler.runF) { g =>
          Kleisli(g).andThen { s => F.forceR(fa.sampler.runA(s._1))(fb.sampler.run(s._2)) }.run
        }
      )
    )

  def onCancel[A](fa: RandomT[F, S, A], fin: RandomT[F, S, Unit]): RandomT[F, S, A] =
    RandomT(
      StateT.applyF(
        F.map(S.split.sampler.runF) { g =>
          Kleisli(g).andThen { s =>
            F.onCancel(fa.sampler.run(s._1), fin.sampler.runA(s._2))
          }.run
        }
      )
    )

  def uncancelable[A](body: Poll[RandomT[F, S, *]] => RandomT[F, S, A]): RandomT[F, S, A] =
    RandomT(
      StateT(s =>
        F.uncancelable { poll =>
          val poll2 = new Poll[RandomT[F, S, *]] {
            def apply[B](fb: RandomT[F, S, B]): RandomT[F, S, B] =
              RandomT(StateT(s => poll(fb.sampler.run(s))))
          }

          body(poll2).sampler.run(s)
        })
    )

}

sealed private[instances] trait RandomTSpawn[F[_], S, E]
    extends GenSpawn[RandomT[F, S, *], E]
    with RandomTMonadCancel[F, S, E] {
  implicit override def F: GenSpawn[F, E]

  override def unique: RandomT[F, S, Unique.Token] = RandomT.liftF(F.unique)

  override def never[A]: RandomT[F, S, A] = RandomT.liftF(F.never)

  override def cede: RandomT[F, S, Unit] = RandomT.liftF(F.cede)

  override def start[A](fa: RandomT[F, S, A]): RandomT[F, S, Fiber[RandomT[F, S, *], E, A]] =
    RandomT(
      StateT.applyF(
        F.map(S.split.sampler.runF) { g =>
          Kleisli(g).andThen { s =>
            F.product(F.pure(s._1), F.map(F.start(fa.sampler.run(s._2)))(liftFiber))
          }.run
        }
      )
    )

  override def racePair[A, B](fa: RandomT[F, S, A], fb: RandomT[F, S, B]): RandomT[
    F,
    S,
    Either[
      (Outcome[RandomT[F, S, *], E, A], Fiber[RandomT[F, S, *], E, B]),
      (Fiber[RandomT[F, S, *], E, A], Outcome[RandomT[F, S, *], E, B])]] =
    RandomT(
      StateT.applyF(
        F.map(product(S.split, S.split).sampler.runF) { g =>
          Kleisli(g).andThen { s =>
            val (s1, (s2, s3)) = s
            F.product(
              F.pure(s1),
              F.map(F.racePair(fa.sampler.run(s2), fb.sampler.run(s3))) {
                case Left((oc, fib)) => Left((liftOutcome(oc), liftFiber(fib)))
                case Right((fib, oc)) => Right((liftFiber(fib), liftOutcome(oc)))
              }
            )
          }.run
        }
      )
    )

  private def liftFiber[S, A](fib: Fiber[F, E, (S, A)]): Fiber[RandomT[F, S, *], E, A] =
    new Fiber[RandomT[F, S, *], E, A] {
      def cancel: RandomT[F, S, Unit] = RandomT.liftF(fib.cancel)
      def join: RandomT[F, S, Outcome[RandomT[F, S, *], E, A]] =
        RandomT.liftF(F.map(fib.join)(liftOutcome))
    }

  private def liftOutcome[S, A](oc: Outcome[F, E, (S, A)]): Outcome[RandomT[F, S, *], E, A] =
    oc match {
      case Outcome.Canceled() => Outcome.Canceled()
      case Outcome.Errored(e) => Outcome.Errored(e)
      case Outcome.Succeeded(foa) => Outcome.Succeeded(RandomT.liftF(F.map(foa)(_._2)))
    }
}

sealed private[instances] trait RandomTConcurrent[F[_], S, E]
    extends GenConcurrent[RandomT[F, S, *], E]
    with RandomTSpawn[F, S, E] {
  implicit override def F: GenConcurrent[F, E]

  override def ref[A](a: A): RandomT[F, S, Ref[RandomT[F, S, *], A]] =
    RandomT.liftF(F.map(F.ref(a))(_.mapK(RandomT.liftK)))

  override def deferred[A]: RandomT[F, S, Deferred[RandomT[F, S, *], A]] =
    RandomT.liftF(F.map(F.deferred[A])(_.mapK(RandomT.liftK)))
}

sealed private[instances] trait RandomTClock[F[_], S] extends Clock[RandomT[F, S, *]] {
  implicit def F: Applicative[F]
  implicit def C: Clock[F]

  override def monotonic: RandomT[F, S, FiniteDuration] =
    RandomT.liftF(C.monotonic)

  override def realTime: RandomT[F, S, FiniteDuration] =
    RandomT.liftF(C.realTime)
}

sealed private[instances] trait RandomTTemporal[F[_], S, E]
    extends GenTemporal[RandomT[F, S, *], E]
    with RandomTConcurrent[F, S, E]
    with RandomTClock[F, S] {
  implicit def F: GenTemporal[F, E]
  def C = F

  def sleep(time: FiniteDuration): RandomT[F, S, Unit] =
    RandomT.liftF(F.sleep(time))
}

sealed private[instances] trait RandomTSync[F[_], S]
    extends Sync[RandomT[F, S, *]]
    with RandomTMonadCancel[F, S, Throwable]
    with RandomTClock[F, S] {
  implicit def F: Sync[F]
  def C = F

  def suspend[A](hint: Type)(thunk: => A): RandomT[F, S, A] =
    RandomT.liftF(F.suspend(hint)(thunk))
}

trait SplitMonadCancel[S] extends Serializable {
  def split[F[_], E](implicit F: MonadCancel[F, E]): Split[F, S]
}

object SplitMonadCancel {
  implicit def schrodingerEffectSplitMonadCancelFromSplitId[S](
      implicit S: Split[Id, S]): SplitMonadCancel[S] =
    new SplitMonadCancel[S] {
      override def split[F[_], E](implicit F: MonadCancel[F, E]): Split[F, S] =
        new Split[F, S] {
          override def split: RandomT[F, S, S] = RandomT.fromRandom(S.split)
        }
    }
}

private[effect] trait RandomTAsync[F[_], S]
    extends Async[RandomT[F, S, *]]
    with RandomTSync[F, S]
    with RandomTTemporal[F, S, Throwable] {
  async =>
  implicit def F: Async[F]
  implicit override def S: Split[F, S] = S1.split[F, Throwable]
  implicit def S1: SplitMonadCancel[S]
  final override def C = F

  def cont[K, R](body: Cont[RandomT[F, S, *], K, R]): RandomT[F, S, R] =
    RandomT(
      StateT(s =>
        F.cont(
          new Cont[F, K, (S, R)] {

            override def apply[G[_]](implicit G: MonadCancel[G, Throwable])
                : (Either[Throwable, K] => Unit, G[K], F ~> G) => G[(S, R)] = {
              implicit val RG = new RandomTMonadCancel[G, S, Throwable] {
                implicit override val F: MonadCancel[G, Throwable] = G
                override implicit val S: Split[G, S] = S1.split[G, Throwable](G)
                override def rootCancelScope: CancelScope = F.rootCancelScope
              }

              (cb, ga, nat) => {
                val natT: RandomT[F, S, *] ~> RandomT[G, S, *] =
                  new (RandomT[F, S, *] ~> RandomT[G, S, *]) {
                    override def apply[A](fa: RandomT[F, S, A]): RandomT[G, S, A] =
                      RandomT(StateT(s => nat(fa.sampler.run(s))))
                  }

                body[RandomT[G, S, *]].apply(cb, RandomT.liftF(ga), natT).sampler.run(s)
              }
            }
          }
        ))
    )

  def evalOn[A](fa: RandomT[F, S, A], ec: ExecutionContext): RandomT[F, S, A] =
    RandomT(StateT(s => F.evalOn(fa.sampler.run(s), ec)))

  def executionContext: RandomT[F, S, ExecutionContext] = RandomT.liftF(F.executionContext)

  override def unique: RandomT[F, S, Unique.Token] = super[RandomTTemporal].unique
  override def never[A]: RandomT[F, S, A] = super[RandomTTemporal].never
}

// TODO I think this belongs here, because mapFilter can potentially hang and thus sampling is unsafe?
sealed private[effect] trait RandomTFunctorFilter[F[_], S]
    extends FunctorFilter[RandomT[F, S, *]] {

  def sync: Sync[RandomT[F, S, *]]

  override def functor: Functor[RandomT[F, S, *]] = sync

  override def mapFilter[A, B](fa: RandomT[F, S, A])(f: A => Option[B]): RandomT[F, S, B] =
    sync.untilDefinedM(sync.map(fa)(f))

}
