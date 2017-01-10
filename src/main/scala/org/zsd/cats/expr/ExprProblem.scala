package org.zsd.cats.expr

import cats.Functor
import cats.data.Coproduct
import cats.data.Coproduct._
import cats.derived._
import cats.free.:<:
import functor._
import legacy._

import scala.language.higherKinds

object ExprProblem extends App {

  case class Fix[F[_]](in: F[Fix[F]])

  case class Val[E](i: Int)
  case class Add[E](left: E, right: E)

  trait :+:[F[_], G[_]] {
    type L[X] = Coproduct[F, G, X]
  }

  val addExample: Fix[(Val :+: Add)#L] = Fix[(Val :+: Add)#L](rightc(Add(Fix[(Val :+: Add)#L](leftc(Val(118))), Fix[(Val :+: Add)#L](leftc(Val(1219))))))

  def foldFix[F[_], A](f: F[A] => A)(expr: Fix[F])(implicit F: Functor[F]): A = {
    f(F.map(expr.in)(foldFix(f)))
  }

  trait Eval[F[_]] {
    def evalAlgebra(f: F[Int])(implicit F: Functor[F]): Int
  }

  implicit def valEval: Eval[Val] = new Eval[Val] {
    override def evalAlgebra(f: Val[Int])(implicit F: Functor[Val]): Int = f.i
  }

  //noinspection MutatorLikeMethodIsParameterless
  implicit def addEval: Eval[Add] = new Eval[Add] {
    override def evalAlgebra(f: Add[Int])(implicit F: Functor[Add]): Int = f.left + f.right
  }

  implicit def coproductEval[F[_]: Functor, G[_]: Functor](implicit FE: Eval[F], GE: Eval[G]): Eval[(F :+: G)#L] = new Eval[(F :+: G)#L] {
    override def evalAlgebra(f: (F :+: G)#L[Int])(implicit F: Functor[(F :+: G)#L]): Int = f match {
      case Coproduct(Left(left)) => FE.evalAlgebra(left)
      case Coproduct(Right(right)) => GE.evalAlgebra(right)
    }
  }

  def eval[F[_]: Functor](e: Fix[F])(implicit F: Eval[F]): Int = foldFix(F.evalAlgebra _)(e)

  val result: Int = eval[(Val :+: Add)#L](addExample)
  println(result)

  def injectFix[G[_], F[_]](g: G[Fix[F]])(implicit I: (G :<: F)): Fix[F] = Fix(I.inj(g))

  def lit[F[_]](i: Int)(implicit I: Val :<: F): Fix[F] = injectFix[Val, F](Val(i))

  def add[F[_]](left: Fix[F], right: Fix[F])(implicit I: Add :<: F): Fix[F] = injectFix[Add, F](Add(left, right))

  type SimpleAlg[X] = (Add :+: Val)#L[X]

  def elite: Fix[SimpleAlg] = add[SimpleAlg](lit[SimpleAlg](30000), add[SimpleAlg](lit[SimpleAlg](1330), lit[SimpleAlg](7)))

  case class Mul[E](left: E, right: E)

  def mul[F[_]](left: Fix[F], right: Fix[F])(implicit I: Mul :<: F): Fix[F] = injectFix[Mul, F](Mul(left, right))

  implicit def mulEval: Eval[Mul] = new Eval[Mul] {
    override def evalAlgebra(f: Mul[Int])(implicit F: Functor[Mul]): Int = f.left * f.right
  }

  type AdvAlg[X] = (Add :+: (Mul :+: Val)#L[?])#L[X]

//  def meaning: Fix[AdvAlg] = add[AdvAlg](mul[AdvAlg](lit[AdvAlg](6), lit[AdvAlg](4)), lit[AdvAlg](18))

//  println(eval[AdvAlg](meaning))
}
