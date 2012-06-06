package com.wheaties.squealer.sql

sealed trait Result[+F, +S] {
  def flatMap[FF >: F, SS](f: S => Result[FF, SS]): Result[FF, SS]
  def map[SS](f: S => SS): Result[F, SS]
}

case class Failure[+F, +S](x: List[F]) extends Result[F, S] {
  def flatMap[FF >: F, SS](f: S => Result[FF, SS]) = Failure[FF, SS](x)
  def map[SS](f: S => SS) = Failure[F, SS](x)
}

object Failure{
  def apply[F, S](x: F):Failure[F, S] = Failure(List(x))
}

case class Partial[+F, +S](x: S, y: List[F]) extends Result[F, S]{
  def flatMap[FF >: F, SS](f: S => Result[FF, SS]) = f(x) match{
    case Success(suc) => Partial(suc, y)
    case Failure(fail) => Failure(fail ::: y)
    case Partial(out, fail) => Partial(out, fail ::: y)
  }
  def map[SS](f: S => SS) = Partial[F, SS](f(x), y)
}

object Partial{
  def apply[F, S](x: S, y: F):Partial[F, S] = Partial(x, List(y))
}

case class Success[+F, +S](x: S) extends Result[F, S] {
  def flatMap[FF >: F, SS](f: S => Result[FF, SS]) = f(x)
  def map[SS](f: S => SS) = Success(f(x))
}