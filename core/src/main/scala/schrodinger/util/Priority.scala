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

package schrodinger.util

/**
 * Priority is a type class for prioritized implicit search.
 *
 * This type class will attempt to provide an implicit instance of `P`
 * (the preferred type). If that type is not available it will
 * fallback to `F` (the fallback type). If neither type is available
 * then a `Priority[P, F]` instance will not be available.
 *
 * This type can be useful for problems where multiple algorithms can
 * be used, depending on the type classes available.
 */
sealed abstract class Priority[+P, +F] {

  import Priority.{Fallback, Preferred}

  def fold[B](f1: P => B)(f2: F => B): B =
    this match {
      case Preferred(x) => f1(x)
      case Fallback(y) => f2(y)
    }

  def join[U >: P with F]: U =
    fold(_.asInstanceOf[U])(_.asInstanceOf[U])

  def bimap[P2, F2](f1: P => P2)(f2: F => F2): Priority[P2, F2] =
    this match {
      case Preferred(x) => Preferred(f1(x))
      case Fallback(y) => Fallback(f2(y))
    }

  def toEither: Either[P, F] =
    fold[Either[P, F]](p => Left(p))(f => Right(f))

  def isPreferred: Boolean =
    fold(_ => true)(_ => false)

  def isFallback: Boolean =
    fold(_ => false)(_ => true)

  def getPreferred: Option[P] =
    fold[Option[P]](p => Some(p))(_ => None)

  def getFallback: Option[F] =
    fold[Option[F]](_ => None)(f => Some(f))
}

object Priority extends FindPreferred {

  case class Preferred[P](get: P) extends Priority[P, Nothing]
  case class Fallback[F](get: F) extends Priority[Nothing, F]

  def apply[P, F](implicit ev: Priority[P, F]): Priority[P, F] = ev
}

private[schrodinger] class FindPreferred extends FindFallback {
  implicit def preferred[P](implicit ev: P): Priority[P, Nothing] =
    Priority.Preferred(ev)
}

private[schrodinger] class FindFallback {
  implicit def fallback[F](implicit ev: F): Priority[Nothing, F] =
    Priority.Fallback(ev)
}
