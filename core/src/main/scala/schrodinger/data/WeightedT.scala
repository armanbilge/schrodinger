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

package schrodinger.data

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
import schrodinger.data.Weighted.{Heavy, Weightless}
import schrodinger.{Binoid, Group0}

final case class WeightedT[F[_], W, A](value: F[Weighted[W, A]]) {

  def ap[B](f: WeightedT[F, W, A => B])(
      implicit F: Apply[F],
      W0: Binoid[W],
      W1: Eq[W]): WeightedT[F, W, B] =
    WeightedT(F.map2(f.value, value) { (wf, wa) => wf.flatMap(f => wa.map(f)) })

  def apF[B](f: F[Weighted[W, A => B]])(
      implicit F: Apply[F],
      W0: Binoid[W],
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
      W0: Binoid[W],
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
      W0: Binoid[W],
      W1: Eq[W]): WeightedT[F, W, B] =
    flatMap(a => WeightedT(f(a)))

  def importance(
      f: A => W)(implicit F: Applicative[F], W0: Group0[W], W1: Eq[W]): WeightedT[F, W, A] =
    WeightedT(F.map(value)(_.importance(f)))

  def importanceF(
      f: A => F[W])(implicit F: Monad[F], W0: Group0[W], W1: Eq[W]): WeightedT[F, W, A] =
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

sealed abstract private[data] class WeightedTInstances extends WeightedTInstances0 {

  implicit def schrodingerDataDeferForWeightedT[F[_], W](
      implicit F: Defer[F]): Defer[WeightedT[F, W, *]] =
    new Defer[WeightedT[F, W, *]] {
      def defer[A](fa: => WeightedT[F, W, A]): WeightedT[F, W, A] =
        WeightedT(F.defer(fa.value))
    }
}

sealed abstract private[data] class WeightedTInstances0 extends WeightedTInstances1 {
  implicit def schrodingerDataOrderForWeightedT[F[_], W, A](
      implicit Ord: Order[F[Weighted[W, A]]]): Order[WeightedT[F, W, A]] =
    new Order[WeightedT[F, W, A]] {
      def compare(x: WeightedT[F, W, A], y: WeightedT[F, W, A]): Int = x.compare(y)
    }
}

sealed abstract private[data] class WeightedTInstances1 extends WeightedTInstances2 {
  implicit def schrodingerDataPartialOrderForWeightedT[F[_], W, A](
      implicit Ord: PartialOrder[F[Weighted[W, A]]]): PartialOrder[WeightedT[F, W, A]] =
    new PartialOrder[WeightedT[F, W, A]] {
      override def partialCompare(x: WeightedT[F, W, A], y: WeightedT[F, W, A]): Double =
        Ord.partialCompare(x.value, y.value)
    }
}

sealed abstract private[data] class WeightedTInstances2 extends WeightedTInstances3 {

  implicit def schrodingerDataMonadErrorForWeightedT[F[_], W, E](
      implicit F0: MonadError[F, E],
      W: Binoid[W],
      eqW: Eq[W]): MonadError[WeightedT[F, W, *], E] =
    new WeightedTMonadError[F, W, E] {
      implicit val F: MonadError[F, E] = F0
      implicit val W0: Binoid[W] = W
      implicit val W1: Eq[W] = eqW
    }

  implicit def schrodingerDataParallelForWeightedT[M[_], W: Binoid: Eq](
      implicit P: Parallel[M]): Parallel.Aux[WeightedT[M, W, *], WeightedT[P.F, W, *]] =
    new Parallel[WeightedT[M, W, *]] {
      type F[x] = WeightedT[P.F, W, x]
      implicit val monadM: Monad[M] = P.monad

      def applicative: Applicative[WeightedT[P.F, W, *]] =
        schrodingerDataApplicativeForWeightedT(P.applicative, Binoid[W], Eq[W])
      def monad: Monad[WeightedT[M, W, *]] = schrodingerDataMonadForWeightedT

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

  implicit def schrodingerDataEqForWeightedTId[W: Eq, A: Eq]: Eq[WeightedT[Id, W, A]] =
    schrodingerDataEqForWeightedT[Id, W, A]

  implicit def schrodingerDataShowForWeightedT[F[_], W, A](
      implicit F: Show[F[Weighted[W, A]]]): Show[WeightedT[F, W, A]] =
    new Show[WeightedT[F, W, A]] {
      override def show(f: WeightedT[F, W, A]): String = f.show
    }

  implicit def schrodingerDataMonoidForWeightedTId[W: Binoid: Eq, A: Monoid]
      : Monoid[WeightedT[Id, W, A]] =
    schrodingerDataMonoidForWeightedT[Id, W, A]
}

sealed abstract private[data] class WeightedTInstances3 extends WeightedTInstances4 {
  implicit def schrodingerDataMonadForWeightedTId[W: Binoid: Eq]: Monad[WeightedT[Id, W, *]] =
    schrodingerDataMonadForWeightedT[Id, W]

  implicit def schrodingerDataEqForWeightedT[F[_], W, A](
      implicit F: Eq[F[Weighted[W, A]]]): Eq[WeightedT[F, W, A]] =
    Eq.by[WeightedT[F, W, A], F[Weighted[W, A]]](_.value)

  implicit def schrodingerDataSemigroupForWeightedTId[W: Binoid: Eq, A: Semigroup]
      : Semigroup[WeightedT[Id, W, A]] =
    schrodingerDataSemigroupForWeightedT[Id, W, A]
}

sealed abstract private[data] class WeightedTInstances4 extends WeightedTInstances5 {
  implicit def schrodingerDataMonadForWeightedT[F[_], W](
      implicit F0: Monad[F],
      W: Binoid[W],
      eqW: Eq[W]): Monad[WeightedT[F, W, *]] =
    new WeightedTMonad[F, W] {
      implicit val F: Monad[F] = F0
      implicit val W0: Binoid[W] = W
      implicit val W1: Eq[W] = eqW
    }

  implicit def schrodingerDataMonoidForWeightedT[F[_], W, A](
      implicit W: Monoid[F[Weighted[W, A]]]): Monoid[WeightedT[F, W, A]] =
    new WeightedTMonoid[F, W, A] {
      implicit val F0: Monoid[F[Weighted[W, A]]] = W
    }
}

sealed abstract private[data] class WeightedTInstances5 extends WeightedTInstances6 {}

sealed abstract private[data] class WeightedTInstances6 extends WeightedTInstances7 {
  implicit def schrodingerDataSemigroupForWeightedT[F[_], W, A](
      implicit W: Semigroup[F[Weighted[W, A]]]): Semigroup[WeightedT[F, W, A]] =
    new WeightedTSemigroup[F, W, A] {
      implicit val F0: Semigroup[F[Weighted[W, A]]] = W
    }
}

sealed abstract private[data] class WeightedTInstances7 extends WeightedTInstances8 {
  implicit def schrodingerDataApplicativeErrorForWeightedT[F[_], W, E](
      implicit F0: ApplicativeError[F, E],
      W: Binoid[W],
      eqW: Eq[W]): ApplicativeError[WeightedT[F, W, *], E] =
    new WeightedTApplicativeError[F, W, E] {
      implicit val F: ApplicativeError[F, E] = F0
      implicit val W0: Binoid[W] = W
      implicit val W1: Eq[W] = eqW
    }
}

sealed abstract private[data] class WeightedTInstances8 extends WeightedTInstances9 {
  implicit def schrodingerDataAlternativeForWeightedT[F[_], W](
      implicit F0: Alternative[F],
      W: Binoid[W],
      eqW: Eq[W]): Alternative[WeightedT[F, W, *]] =
    new WeightedTAlternative[F, W] {
      implicit val F: Alternative[F] = F0
      implicit val W0: Binoid[W] = W
      implicit val W1: Eq[W] = eqW
    }

  implicit def schrodingerDataContravariantMonoidalForWeightedT[F[_], W](
      implicit F: ContravariantMonoidal[F]): ContravariantMonoidal[WeightedT[F, W, *]] =
    new WeightedTContravariantMonoidal[F, W] {
      implicit val F0: ContravariantMonoidal[F] = F
    }
}

sealed abstract private[data] class WeightedTInstances9 extends WeightedTInstances10 {
  implicit def schrodingerDataMonoidKForWeightedT[F[_], W](
      implicit F0: MonoidK[F]): MonoidK[WeightedT[F, W, *]] =
    new WeightedTMonoidK[F, W] {
      implicit val F: MonoidK[F] = F0
    }

  implicit def schrodingerDataContravariantForWeightedT[F[_], W](
      implicit F: Contravariant[F]): Contravariant[WeightedT[F, W, *]] =
    new WeightedTContravariant[F, W] {
      implicit val F0: Contravariant[F] = F
    }
}

sealed abstract private[data] class WeightedTInstances10 extends WeightedTInstances11 {
  implicit def schrodingerDataSemigroupKForWeightedT[F[_], W](
      implicit F0: SemigroupK[F]): SemigroupK[WeightedT[F, W, *]] =
    new WeightedTSemigroupK[F, W] {
      implicit val F: SemigroupK[F] = F0
    }

  implicit def schrodingerDataApplicativeForWeightedT[F[_], W](
      implicit F0: Applicative[F],
      W: Binoid[W],
      eqW: Eq[W]): Applicative[WeightedT[F, W, *]] =
    new WeightedTApplicative[F, W] {
      implicit val F: Applicative[F] = F0
      implicit val W0: Binoid[W] = W
      implicit val W1: Eq[W] = eqW
    }

  implicit def schrodingerDataInvariantForWeightedT[F[_], W](
      implicit F0: Invariant[F]): Invariant[WeightedT[F, W, *]] =
    new WeightedTInvariant[F, W] { implicit val F = F0 }
}

sealed abstract private[data] class WeightedTInstances11 {
  implicit def schrodingerDataApplyForWeightedT[F[_], W](
      implicit F0: Apply[F],
      W: Binoid[W],
      eqW: Eq[W]): Apply[WeightedT[F, W, *]] =
    new WeightedTApply[F, W] {
      implicit val F: Apply[F] = F0
      implicit val W0: Binoid[W] = W
      implicit val W1: Eq[W] = eqW
    }
}

sealed private[data] trait WeightedTFunctor[F[_], W] extends Functor[WeightedT[F, W, *]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: WeightedT[F, W, A])(f: A => B): WeightedT[F, W, B] =
    fa.map(f)
}

sealed private[data] trait WeightedTContravariant[F[_], W]
    extends Contravariant[WeightedT[F, W, *]] {
  implicit def F0: Contravariant[F]

  override def contramap[A, B](fa: WeightedT[F, W, A])(f: B => A): WeightedT[F, W, B] =
    fa.contramap(f)
}

sealed private[data] trait WeightedTInvariant[F[_], W] extends Invariant[WeightedT[F, W, *]] {
  implicit def F: Invariant[F]

  override def imap[A, B](fa: WeightedT[F, W, A])(f: A => B)(g: B => A): WeightedT[F, W, B] =
    fa.imap(f)(g)
}

sealed private[data] trait WeightedTApply[F[_], W]
    extends WeightedTFunctor[F, W]
    with Apply[WeightedT[F, W, *]] {
  implicit override def F: Apply[F]
  implicit def W0: Binoid[W]
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

sealed private[data] trait WeightedTApplicative[F[_], W]
    extends WeightedTApply[F, W]
    with Applicative[WeightedT[F, W, *]] {
  implicit override def F: Applicative[F]
  implicit override def W0: Binoid[W]

  def pure[A](a: A): WeightedT[F, W, A] =
    WeightedT.pure(a)
}

sealed private[data] trait WeightedTMonad[F[_], W]
    extends WeightedTApplicative[F, W]
    with Monad[WeightedT[F, W, *]] {
  implicit override def F: Monad[F]
  implicit override def W0: Binoid[W]

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

sealed private[data] trait WeightedTApplicativeError[F[_], W, E]
    extends ApplicativeError[WeightedT[F, W, *], E]
    with WeightedTApplicative[F, W] {
  implicit override def F: ApplicativeError[F, E]

  def raiseError[A](e: E): WeightedT[F, W, A] = WeightedT(F.raiseError[Weighted[W, A]](e))

  def handleErrorWith[A](fa: WeightedT[F, W, A])(
      f: E => WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(F.handleErrorWith(fa.value)(e => f(e).value))
}

private[schrodinger] trait WeightedTMonadError[F[_], W, E]
    extends MonadError[WeightedT[F, W, *], E]
    with WeightedTMonad[F, W]
    with WeightedTApplicativeError[F, W, E] {
  implicit override def F: MonadError[F, E]

  override def ap[A, B](f: WeightedT[F, W, A => B])(
      fa: WeightedT[F, W, A]): WeightedT[F, W, B] =
    super[WeightedTMonad].ap(f)(fa)
}

sealed private[data] trait WeightedTSemigroupK[F[_], W] extends SemigroupK[WeightedT[F, W, *]] {
  implicit def F: SemigroupK[F]

  def combineK[A](x: WeightedT[F, W, A], y: WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(F.combineK(x.value, y.value))

  override def combineKEval[A](
      x: WeightedT[F, W, A],
      y: Eval[WeightedT[F, W, A]]): Eval[WeightedT[F, W, A]] =
    F.combineKEval(x.value, y.map(_.value)).map(WeightedT(_))
}

sealed private[data] trait WeightedTMonoidK[F[_], W]
    extends MonoidK[WeightedT[F, W, *]]
    with WeightedTSemigroupK[F, W] {
  implicit override def F: MonoidK[F]

  def empty[A]: WeightedT[F, W, A] = WeightedT(F.empty)
}

sealed private[data] trait WeightedTAlternative[F[_], W]
    extends Alternative[WeightedT[F, W, *]]
    with WeightedTMonoidK[F, W]
    with WeightedTApplicative[F, W] {
  implicit override def F: Alternative[F]
}

sealed private[data] trait WeightedTContravariantMonoidal[F[_], W]
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

sealed private[data] trait WeightedTSemigroup[F[_], W, A]
    extends Semigroup[WeightedT[F, W, A]] {
  implicit def F0: Semigroup[F[Weighted[W, A]]]

  def combine(x: WeightedT[F, W, A], y: WeightedT[F, W, A]): WeightedT[F, W, A] =
    WeightedT(F0.combine(x.value, y.value))
}

sealed private[data] trait WeightedTMonoid[F[_], W, A]
    extends Monoid[WeightedT[F, W, A]]
    with WeightedTSemigroup[F, W, A] {
  implicit override def F0: Monoid[F[Weighted[W, A]]]

  def empty: WeightedT[F, W, A] = WeightedT(F0.empty)
}
