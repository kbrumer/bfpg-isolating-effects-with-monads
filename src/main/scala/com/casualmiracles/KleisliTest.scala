package com.casualmiracles

import scalaz.Scalaz._
import scalaz.{KleisliFunctions, KleisliInstances}

/**
 * from:
 * http://www.casualmiracles.com/2012/07/02/a-small-example-of-kleisli-arrows/
 *
 */

object KleisliTest extends App with KleisliInstances with KleisliFunctions {

  // Some methods that take simple types and return higher-kinded types
  def str(x: Int): Option[String] = Some(x.toString)
  def toInt(x: String): Option[Int] = Some(x.toInt)
  def double(x: Int): Option[Double] = Some(x * 2)

  // Lets compose those functions Ye Olde Way
  def oldSchool(i: Int) =
    for {
      x <- str(i)
      y <- toInt(x)
      z <- double(y)
    } yield z

  // Kleisli!
  val funky = kleisli(str) >=> kleisli(toInt) >=> kleisli(double)

  println(oldSchool(1)) // Some(2.0)
  println(funky(1))     // Some(2.0)




}