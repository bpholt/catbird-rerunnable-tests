package tests

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.tagless._
import cats.tagless.implicits._
import com.twitter.util.{Await, Future}
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import io.catbird.util._
import com.twitter.conversions.DurationOps._

import scala.collection.mutable

class RerunnableTraverseSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
  implicit val twitterTimer: com.twitter.util.Timer = com.twitter.finagle.util.DefaultTimer
  implicit val CS: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)

  def expectedActionsForInput(s: String): Chain[String] =
    Chain(s"op1($s)", s"op2($s)")

  def f[F[_] : Monad](alg: Alg[F]): String => F[String] = s =>
    for {
      o1 <- alg.op1(s)
      o2 <- alg.op2(s)
    } yield o1 |+| o2

  implicit def arbChain[T : Arbitrary]: Arbitrary[Chain[T]] =
    Arbitrary(Arbitrary.arbitrary[List[T]].map(Chain.fromSeq))

  property("IO ordering should be op1, op2, op1, op2…") {
    forAll { input: Chain[String] =>
      val expected = input.flatMap(expectedActionsForInput)

      val output =
        for {
          actions <- Ref.of[IO, Chain[String]](Chain.empty)
          alg = Alg.ioAlg(actions)
          traverseOut <- input.traverse(f(alg))
          actionsOut <- actions.get
        } yield (actionsOut, traverseOut)

      val (actions, traverseOut) = output.unsafeRunSync()

      actions.toList should contain allElementsOf expected.toList
      actions should be(expected)
      traverseOut should have size input.size
    }
  }

  property("IO parTraverse should have all the expected actions in an unspecified order") {
    forAll { input: Chain[String] =>
      val expected = input.flatMap(expectedActionsForInput)

      val output =
        for {
          actions <- Ref.of[IO, Chain[String]](Chain.empty)
          alg = Alg.ioAlg(actions)
          traverseOut <- input.parTraverse(f(alg))
          actionsOut <- actions.get
        } yield (actionsOut, traverseOut)

      val (actions, traverseOut) = output.unsafeRunSync()

      actions.toList should contain allElementsOf expected.toList
      traverseOut should have size input.size
    }
  }

  property("Future ordering should be op1, op2, op1, op2…") {
    forAll { input: Chain[String] =>
      val expected = input.flatMap(expectedActionsForInput)

      val actions: mutable.MutableList[String] = mutable.MutableList.empty
      val alg = Alg.futureAlg(actions)
      val output: Future[Chain[String]] = input.traverse(f(alg))
      val traverseOut = Await.result(output)

      actions.toList should contain allElementsOf expected.toList
      actions.toList should be(expected.toList)
      traverseOut should have size input.size
    }
  }

  property("Future parTraverse should have all the expected actions in an unspecified order") {
    forAll { input: Chain[String] =>
      val expected = input.flatMap(expectedActionsForInput)

      val actions: mutable.MutableList[String] = mutable.MutableList.empty
      val alg = Alg.futureAlg(actions)
      val output: Future[Chain[String]] = input.parTraverse(f(alg))
      val traverseOut = Await.result(output)

      actions.toList should contain allElementsOf expected.toList
      traverseOut should have size input.size
    }
  }

  property("Rerunnable ordering should be op1, op2, op1, op2…") {
    forAll { input: Chain[String] =>
      val expected = input.flatMap(expectedActionsForInput)

      val actions: mutable.MutableList[String] = mutable.MutableList.empty
      val alg = Alg.rerunnableAlg(actions)
      val output: Rerunnable[Chain[String]] = input.traverse(f(alg))
      val traverseOut = Await.result(output.run)

      actions.toList should contain allElementsOf expected.toList
      actions.toList should be(expected.toList)
      traverseOut should have size input.size
    }
  }

  property("Rerunnable ordering should be op1, op2, op1, op2… when mapK'd from Future") {
    forAll { input: Chain[String] =>
      val expected = input.flatMap(expectedActionsForInput)

      val actions: mutable.MutableList[String] = mutable.MutableList.empty
      val alg: Alg[Rerunnable] = Alg.futureAlg(actions).mapK(λ[Future ~> Rerunnable](Rerunnable.fromFuture(_)))
      val output: Rerunnable[Chain[String]] = input.traverse(f(alg))
      val traverseOut = Await.result(output.run)

      actions.toList should contain allElementsOf expected.toList
      actions.toList should be(expected.toList)
      traverseOut should have size input.size
    }
  }

  property("Rerunnable parTraverse should have all the expected actions in an unspecified order") {
    forAll { input: Chain[String] =>
      val expected = input.flatMap(expectedActionsForInput)

      val actions: mutable.MutableList[String] = mutable.MutableList.empty
      val alg = Alg.rerunnableAlg(actions)
      val output: Rerunnable[Chain[String]] = input.parTraverse(f(alg))
      val traverseOut = Await.result(output.run)

      actions.toList should contain allElementsOf expected.toList
      traverseOut should have size input.size
    }
  }

  property("Rerunnable parTraverse should have all the expected actions in an unspecified order when mapK'd from Future") {
    forAll { input: Chain[String] =>
      val expected = input.flatMap(expectedActionsForInput)

      val actions: mutable.MutableList[String] = mutable.MutableList.empty
      val alg: Alg[Rerunnable] = Alg.futureAlg(actions).mapK(λ[Future ~> Rerunnable](Rerunnable.fromFuture(_)))
      val output: Rerunnable[Chain[String]] = input.parTraverse(f(alg))
      val traverseOut = Await.result(output.run)

      actions.toList should contain allElementsOf expected.toList
      traverseOut should have size input.size
    }
  }
}

trait Alg[F[_]] {
  def op1(s: String): F[String]
  def op2(s: String): F[String]
}

object Alg {
  implicit val algFunctorK: FunctorK[Alg] = Derive.functorK[Alg]

  def ioAlg(actions: Ref[IO, Chain[String]]): Alg[IO] =
    new Alg[IO] {
      override def op1(s: String): IO[String] =
        actions.update(_ append s"op1($s)").as(s"op1($s)")

      override def op2(s: String): IO[String] =
        actions.update(_ append s"op2($s)").as(s"op2($s)")
    }

  def futureAlg(actions: mutable.MutableList[String])
               (implicit T: com.twitter.util.Timer): Alg[Future] =
    new Alg[Future] {
      override def op1(s: String): Future[String] =
        Future.sleep(10.millis) *>
          Future.value(actions += s"op1($s)").as(s"op1($s)")

      override def op2(s: String): Future[String] =
        Future.sleep(10.millis) >>
          Future.value(actions += s"op2($s)").as(s"op2($s)")
    }

  def rerunnableAlg(actions: mutable.MutableList[String])
                   (implicit T: com.twitter.util.Timer): Alg[Rerunnable] =
    new Alg[Rerunnable] {
      override def op1(s: String): Rerunnable[String] =
        Rerunnable.fromFuture(Future.sleep(10.millis)) >>
          Rerunnable(actions += s"op1($s)").as(s"op1($s)")

      override def op2(s: String): Rerunnable[String] =
        Rerunnable.fromFuture(Future.sleep(10.millis)) >>
          Rerunnable(actions += s"op2($s)").as(s"op2($s)")
    }
}
