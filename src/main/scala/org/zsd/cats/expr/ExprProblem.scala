package org.zsd.cats.expr

import cats.Functor

import scala.language.higherKinds

object ExprProblem extends App {

  case class Expr[F[_]](in: F[Expr[F]])

  case class Val[E](i: Int)
  type IntExpr = Expr[Val]

  case class Add[E](left: E, right: E)
  type AddExpr = Expr[Add]

  sealed trait Coproduct[F[_], G[_], E]
  case class Inl[F[_], G[_], E](left: F[E]) extends Coproduct[F, G, E]
  case class Inr[F[_], G[_], E](right: G[E]) extends Coproduct[F, G, E]

  trait :+:[F[_], G[_]] {
    type L[X] = ExprProblem.Coproduct[F, G, X]
  }

  val addExample: Expr[(Val :+: Add)#L] = Expr[(Val :+: Add)#L](Inr(Add(Expr[(Val :+: Add)#L](Inl(Val(118))), Expr[(Val :+: Add)#L](Inl(Val(1219))))))

  implicit def valFunctor: Functor[Val] = new Functor[Val] {
    override def map[A, B](fa: Val[A])(f: (A) => B) = Val(fa.i)
  }

  //noinspection MutatorLikeMethodIsParameterless
  implicit def addFunctor: Functor[Add] = new Functor[Add] {
    override def map[A, B](fa: Add[A])(f: (A) => B) = Add(f(fa.left), f(fa.right))
  }

  implicit def coproductFunctor[F[_], G[_]](implicit F: Functor[F], G: Functor[G]): Functor[Coproduct[F, G, ?]] = new Functor[ExprProblem.Coproduct[F, G, ?]] {
    override def map[A, B](fa: Coproduct[F, G, A])(f: (A) => B): Coproduct[F, G, B] = fa match {
      case Inl(left) => Inl(F.map(left)(f))
      case Inr(right) => Inr(G.map(right)(f))
    }
  }

  def foldExpr[F[_], A](f: F[A] => A)(expr: Expr[F])(implicit F: Functor[F]): A = {
    f(F.map(expr.in)(foldExpr(f)))
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
      case Inl(left) => FE.evalAlgebra(left)
      case Inr(right) => GE.evalAlgebra(right)
    }
  }

  def eval[F[_]: Functor](e: Expr[F])(implicit F: Eval[F]): Int = foldExpr(F.evalAlgebra _)(e)

  val result: Int = eval[(Val :+: Add)#L](addExample)
  println(result)
}
