# Schrodinger

[![Latest version](https://index.scala-lang.org/armanbilge/schrodinger/schrodinger/latest.svg?color=orange)](https://index.scala-lang.org/armanbilge/schrodinger/schrodinger)

Schrodinger is an (early-stage) project for probabilistic programming built on the [Cats](https://github.com/typelevel/cats) ecosystem.
At its heart is the `RandomT` monad transformer which enables you to safely inject randomness into your program by describing it as a random variable while also interleaving with other effects, such as [Cats Effect](https://github.com/typelevel/cats-effect)'s `IO` monad.
The goal is that this construct will facilitate the implementation of high-performance, concurrent Monte Carlo algorithms for simulation and inference.

Notably, via the use of "splittable" RNGs (versus a synchronized global or thread-locals), every random number generated can be deterministically traced back to the initial seed irrespective of execution order or thread assignment at runtime.
This substantially improves the reproducibility of concurrent programs.

## Usage

```scala
libraryDependencies += "com.armanbilge" %% "schrodinger" % "0.1.0"
// (optional) provides additional instances for cats-effect typeclasses
libraryDependencies += "com.armanbilge" %% "schrodinger-effect" % "0.1.0"
```

## Datatypes

* `schrodinger.RandomT`: a monad transformer for a random variable that describes a probabilistic program.
  It is a light wrapper for Cat's [`StateT`](https://typelevel.org/cats/datatypes/state.html) with its own typeclass instances.
  Hints (see also the example below):
    1. (Flat)Map the "primitive" random variables provided in `schrodinger.random.*` to create `RandomT` instances for your own datatypes.
    2. Use typeclasses to defer binding the RNG state type `S` to a specific implementation until the (terminal) simulation step.
* `schrodinger.data.Weighted`: represents a "weighted" sample from a probability distribution.
  It is similar to Cat's [`Writer`](https://typelevel.org/cats/datatypes/writer.html) datatype with the additional feature that it short-circuits computation (like `Option`) if the weight goes to zero.
  This is useful for (nested) [importance sampling](https://en.wikipedia.org/wiki/Importance_sampling) where samples from an auxiliary distribution are reweighted to sample from a target distribution.

## Example: Schrödinger's cat

In the [example project](https://github.com/armanbilge/schrodinger/blob/main/example/src/main/scala/schrodinger/ThoughtExperiment.scala) we reimagine the famous [thought experiment](https://en.wikipedia.org/wiki/Schr%C3%B6dinger%27s_cat) with the help of Cats Effect.
The experiment is modeled with three concurrent components:

* an atom that decays after a random exponentially-distributed time and is sensed by the Geiger counter
* a relay that is triggered by the Geiger counter to release poison and kill the cat :(
* an observer who starts the experiment, waits 1 second, and then checks on the cat

Although the only randomness in the program is the atom's decay, we build the entire program as a `RandomT[F[_], S, *]` and defer simulating it until "right before the end of the world" at which point we provide the initial seed for the RNG.

```scala
sealed trait Cat
case object LiveCat extends Cat
case object DeadCat extends Cat

val decayRate = math.log(2)

def decayingAtom[F[_]: Async, S: SplitMonadCancel](
                                                    geigerCounter: CountDownLatch[RandomT[F, S, *]])(implicit E: ExponentialDouble[F, S]) =
  for {
    decayAfter <- Exponential[F, S](decayRate)
    _ <- Async[RandomT[F, S, *]].sleep(decayAfter.seconds)
    _ <- geigerCounter.release
  } yield ()

def poisonRelay[F[_]: Monad](geigerCounter: CountDownLatch[F], cat: Ref[F, Cat]) =
  for {
    _ <- geigerCounter.await
    _ <- cat.set(DeadCat)
  } yield ()

def experiment[F[_]: Async, S: SplitMonadCancel](implicit E: ExponentialDouble[F, S]) =
  for {
    cat <- Ref.of[RandomT[F, S, *], Cat](LiveCat)
    geigerCounter <- CountDownLatch[RandomT[F, S, *]](1)
    // spawning fibers splits the RNG deterministically
    _ <- poisonRelay[RandomT[F, S, *]](geigerCounter, cat).start
    _ <- decayingAtom[F, S](geigerCounter).start
    _ <- Async[RandomT[F, S, *]].sleep(1.second)
    // 50% probability that we will observe a live cat
    observation <- cat.get
  } yield observation

val seed1 = SplitMix.initialState(0x2b992ddfa23249d6L, 0x4034650f1c98bd69L)
val seed2 = SplitMix.initialState(0x86d98163ff1fe751L, 0x8316a8fe31a2228eL)

override def run: IO[Unit] = for {
  observation1 <- experiment[IO, SplitMix].simulate(seed1)
  _ <- IO.println(s"Experiment 1: observing a $observation1")
  observation2 <- experiment[IO, SplitMix].simulate(seed2)
  _ <- IO.println(s"Experiment 2: observing a $observation2")
  _ <- IO.println("No cats were harmed in the thinking of this experiment :)")
} yield ()
```

Run from the command line with  `sbt "example/runMain schrodinger.ThoughtExperiment"`.
In spite of its concurrency, this program is virtually guaranteed to produce:

```
Experiment 1: observing a LiveCat
Experiment 2: observing a DeadCat
No cats were harmed in the thinking of this experiment :)
```
