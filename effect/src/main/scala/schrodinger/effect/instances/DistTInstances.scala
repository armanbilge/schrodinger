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

import cats.data.StateT
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
import schrodinger.DistT.*=>
import schrodinger.generators.Generator
import schrodinger.{DistT, DistTMonadError}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

private[instances] trait DistTInstances extends DistTInstances0 {
  implicit def schrodingerEffectAsyncForDistT[F[_]](
      implicit ev1: Async[F]): Async[DistT[F, *]] =
    new DistTAsync[F] {
      implicit override val F: Async[F] = ev1
    }

  implicit def schrodingerEffectFunctorFilterForDistT[F[_]](
      implicit ev1: Sync[DistT[F, *]]): FunctorFilter[DistT[F, *]] =
    new DistTFunctorFilter[F] {
      override def sync = ev1
    }
}

sealed private[instances] trait DistTInstances0 extends DistTInstances1 {
  implicit def schrodingerEffectSyncForDistT[F[_]](implicit ev1: Sync[F]): Sync[DistT[F, *]] =
    new DistTSync[F] {
      implicit override val F: Sync[F] = ev1
      override def rootCancelScope: CancelScope = F.rootCancelScope
    }

  implicit def schrodingerEffectTemporalForDistT[F[_], E](
      implicit ev1: GenTemporal[F, E]): GenTemporal[DistT[F, *], E] =
    new DistTTemporal[F, E] {
      implicit override val F: GenTemporal[F, E] = ev1
    }
}

sealed private[instances] trait DistTInstances1 extends DistTInstances2 {
  implicit def schrodingerEffectGenConcurrentForDistT[F[_], E](
      implicit ev1: GenConcurrent[F, E]): GenConcurrent[DistT[F, *], E] =
    new DistTConcurrent[F, E] {
      implicit override val F: GenConcurrent[F, E] = ev1
    }

  implicit def schrodingerEffectClockForDistT[F[_], E](
      implicit ev1: Monad[F],
      ev2: Clock[F]): Clock[DistT[F, *]] =
    new DistTClock[F] {
      implicit override val F: Monad[F] = ev1
      implicit override val C: Clock[F] = ev2
      override def applicative: Applicative[DistT[F, *]] =
        DistT.schrodingerMonadForDistT[F](F)
    }
}

sealed private[instances] trait DistTInstances2 extends DistTInstances3 {
  implicit def schrodingerEffectGenSpawnForDistT[F[_], E](
      implicit ev1: GenSpawn[F, E]): GenSpawn[DistT[F, *], E] =
    new DistTSpawn[F, E] {
      implicit override val F: GenSpawn[F, E] = ev1
    }
}

sealed private[instances] trait DistTInstances3 {
  implicit def schrodingerEffectMonadCancelForDistT[F[_], E](
      implicit ev1: MonadCancel[F, E]): MonadCancel[DistT[F, *], E] =
    new DistTMonadCancel[F, E] {
      implicit override val F: MonadCancel[F, E] = ev1
      override def rootCancelScope: CancelScope = F.rootCancelScope
    }
}

sealed private[instances] trait DistTMonadCancel[F[_], E]
    extends MonadCancel[DistT[F, *], E]
    with DistTMonadError[F, E] {

  implicit def F: MonadCancel[F, E]

  def canceled: DistT[F, Unit] =
    DistT.liftF(F.canceled)

  def forceR[A, B](fa: DistT[F, A])(fb: DistT[F, B]): DistT[F, B] =
    DistT {
      new (Id *=> λ[S => F[(S, B)]]) {
        override def apply[S](s: S)(implicit rng: Generator[S]): F[(S, B)] = {
          val (s1, s2) = rng.split.run(s)
          F.forceR(fa.sampler.runA(s1))(fb.sampler.run(s2))
        }
      }
    }

  def onCancel[A](fa: DistT[F, A], fin: DistT[F, Unit]): DistT[F, A] =
    DistT {
      new (Id *=> λ[S => F[(S, A)]]) {
        override def apply[S](s: S)(implicit rng: Generator[S]): F[(S, A)] = {
          val (s1, s2) = rng.split.run(s)
          F.onCancel(fa.sampler.run(s1), fin.sampler.runA(s2))
        }
      }
    }

  def uncancelable[A](body: Poll[DistT[F, *]] => DistT[F, A]): DistT[F, A] =
    DistT[F, A] {
      new (Id *=> λ[S => F[(S, A)]]) {
        override def apply[S: Generator](s: S): F[(S, A)] = {
          F.uncancelable { poll =>
            val poll2 = new Poll[DistT[F, *]] {
              def apply[B](fb: DistT[F, B]): DistT[F, B] =
                DistT {
                  new (Id *=> λ[S => F[(S, B)]]) {
                    override def apply[S1: Generator](s: S1): F[(S1, B)] =
                      poll(fb.sampler[S1].run(s))
                  }
                }
            }

            body(poll2).sampler.run(s)
          }
        }
      }
    }
}

sealed private[instances] trait DistTSpawn[F[_], E]
    extends GenSpawn[DistT[F, *], E]
    with DistTMonadCancel[F, E] {
  implicit override def F: GenSpawn[F, E]

  override def unique: DistT[F, Unique.Token] = DistT.liftF(F.unique)

  override def never[A]: DistT[F, A] = DistT.liftF(F.never)

  override def cede: DistT[F, Unit] = DistT.liftF(F.cede)

  override def start[A](fa: DistT[F, A]): DistT[F, Fiber[DistT[F, *], E, A]] =
    DistT {
      new (Id *=> λ[S => F[(S, Fiber[DistT[F, *], E, A])]]) {
        override def apply[S](s: S)(
            implicit rng: Generator[S]): F[(S, Fiber[DistT[F, *], E, A])] = {
          val (s1, s2) = rng.split.run(s)
          F.product(F.pure(s1), F.map(F.start(fa.sampler.run(s2)))(liftFiber))
        }
      }
    }

  override def racePair[A, B](fa: DistT[F, A], fb: DistT[F, B]): DistT[
    F,
    Either[
      (Outcome[DistT[F, *], E, A], Fiber[DistT[F, *], E, B]),
      (Fiber[DistT[F, *], E, A], Outcome[DistT[F, *], E, B])]] =
    DistT {
      new (Id *=> λ[S => F[
        (
            S,
            Either[
              (Outcome[DistT[F, *], E, A], Fiber[DistT[F, *], E, B]),
              (Fiber[DistT[F, *], E, A], Outcome[DistT[F, *], E, B])])
      ]]) {
        override def apply[S](s: S)(implicit rng: Generator[S]): F[
          (
              S,
              Either[
                (Outcome[DistT[F, *], E, A], Fiber[DistT[F, *], E, B]),
                (Fiber[DistT[F, *], E, A], Outcome[DistT[F, *], E, B])])
        ] = {
          val (s1, (s2, s3)) = Monad[StateT[Id, S, *]].product(rng.split, rng.split).run(s)
          F.product(
            F.pure(s1),
            F.map(F.racePair(fa.sampler.run(s2), fb.sampler.run(s3))) {
              case Left((oc, fib)) => Left((liftOutcome(oc), liftFiber(fib)))
              case Right((fib, oc)) => Right((liftFiber(fib), liftOutcome(oc)))
            }
          )
        }
      }
    }

  private def liftFiber[S, A](fib: Fiber[F, E, (S, A)]): Fiber[DistT[F, *], E, A] =
    new Fiber[DistT[F, *], E, A] {
      def cancel: DistT[F, Unit] = DistT.liftF(fib.cancel)
      def join: DistT[F, Outcome[DistT[F, *], E, A]] =
        DistT.liftF(F.map(fib.join)(liftOutcome))
    }

  private def liftOutcome[S, A](oc: Outcome[F, E, (S, A)]): Outcome[DistT[F, *], E, A] =
    oc match {
      case Outcome.Canceled() => Outcome.Canceled()
      case Outcome.Errored(e) => Outcome.Errored(e)
      case Outcome.Succeeded(foa) => Outcome.Succeeded(DistT.liftF(F.map(foa)(_._2)))
    }
}

sealed private[instances] trait DistTConcurrent[F[_], E]
    extends GenConcurrent[DistT[F, *], E]
    with DistTSpawn[F, E] {
  implicit override def F: GenConcurrent[F, E]

  override def ref[A](a: A): DistT[F, Ref[DistT[F, *], A]] =
    DistT.liftF(F.map(F.ref(a))(_.mapK(DistT.liftK)))

  override def deferred[A]: DistT[F, Deferred[DistT[F, *], A]] =
    DistT.liftF(F.map(F.deferred[A])(_.mapK(DistT.liftK)))
}

sealed private[instances] trait DistTClock[F[_]] extends Clock[DistT[F, *]] {
  implicit def F: Applicative[F]
  implicit def C: Clock[F]

  override def monotonic: DistT[F, FiniteDuration] =
    DistT.liftF(C.monotonic)

  override def realTime: DistT[F, FiniteDuration] =
    DistT.liftF(C.realTime)
}

sealed private[instances] trait DistTTemporal[F[_], E]
    extends GenTemporal[DistT[F, *], E]
    with DistTConcurrent[F, E]
    with DistTClock[F] {
  implicit def F: GenTemporal[F, E]
  def C = F

  def sleep(time: FiniteDuration): DistT[F, Unit] =
    DistT.liftF(F.sleep(time))
}

sealed private[instances] trait DistTSync[F[_]]
    extends Sync[DistT[F, *]]
    with DistTMonadCancel[F, Throwable]
    with DistTClock[F] {
  implicit def F: Sync[F]
  def C = F

  def suspend[A](hint: Type)(thunk: => A): DistT[F, A] =
    DistT.liftF(F.suspend(hint)(thunk))
}

private[effect] trait DistTAsync[F[_]]
    extends Async[DistT[F, *]]
    with DistTSync[F]
    with DistTTemporal[F, Throwable] {
  async =>
  implicit def F: Async[F]
  final override def C = F

  def cont[K, R](body: Cont[DistT[F, *], K, R]): DistT[F, R] =
    DistT {
      new (Id *=> λ[S => F[(S, R)]]) {
        override def apply[S: Generator](s: S): F[(S, R)] = {
          F.cont(
            new Cont[F, K, (S, R)] {

              override def apply[G[_]](implicit G: MonadCancel[G, Throwable])
                  : (Either[Throwable, K] => Unit, G[K], F ~> G) => G[(S, R)] = {
                implicit val DG = new DistTMonadCancel[G, Throwable] {
                  implicit override val F: MonadCancel[G, Throwable] = G
                  override def rootCancelScope: CancelScope = F.rootCancelScope
                }

                (cb, ga, nat) => {
                  val natT: DistT[F, *] ~> DistT[G, *] =
                    new (DistT[F, *] ~> DistT[G, *]) {

                      override def apply[A](fa: DistT[F, A]): DistT[G, A] =
                        DistT {
                          new (Id *=> λ[S => G[(S, A)]]) {
                            override def apply[S1: Generator](s: S1): G[(S1, A)] =
                              nat(fa.sampler[S1].run(s))
                          }
                        }

                    }

                  body[DistT[G, *]].apply(cb, DistT.liftF(ga), natT).sampler.run(s)
                }
              }
            }
          )
        }
      }
    }

  def evalOn[A](fa: DistT[F, A], ec: ExecutionContext): DistT[F, A] =
    DistT {
      new (Id *=> λ[S => F[(S, A)]]) {
        override def apply[S: Generator](s: S): F[(S, A)] =
          F.evalOn(fa.sampler.run(s), ec)
      }
    }

  def executionContext: DistT[F, ExecutionContext] = DistT.liftF(F.executionContext)

  override def unique: DistT[F, Unique.Token] = super[DistTTemporal].unique
  override def never[A]: DistT[F, A] = super[DistTTemporal].never
}

// TODO I think this belongs here, because mapFilter can potentially hang and thus sampling is unsafe?
sealed private[effect] trait DistTFunctorFilter[F[_]] extends FunctorFilter[DistT[F, *]] {

  def sync: Sync[DistT[F, *]]

  override def functor: Functor[DistT[F, *]] = sync

  override def mapFilter[A, B](fa: DistT[F, A])(f: A => Option[B]): DistT[F, B] =
    sync.untilDefinedM(sync.map(fa)(f))

}
