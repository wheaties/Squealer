package com.wheaties.squealer.generator

sealed trait Result[+F, +S] {
  def flatMap[FF >: F, SS](f: S => Result[FF, SS]): Result[FF, SS]
  def map[SS](f: S => SS): Result[F, SS]
}

case class Failure[+F, +S](x: F) extends Result[F, S] {
  def flatMap[FF >: F, SS](f: S => Result[FF, SS]) = Failure[FF, SS](x)
  def map[SS](f: S => SS) = Failure[F, SS](x)
}

case class Success[+F, +S](x: S) extends Result[F, S] {
  def flatMap[FF >: F, SS](f: S => Result[FF, SS]) = f(x)
  def map[SS](f: S => SS) = Success(f(x))
}