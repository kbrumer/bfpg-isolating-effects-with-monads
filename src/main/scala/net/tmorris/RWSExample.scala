package net.tmorris

import scalaz.Scalaz._
import scalaz._

object RWSExample extends App {
  case class Config(port: Int)

  def log[R, S](msg: String): ReaderWriterState[R, List[String], S, Unit] =
    ReaderWriterState {
      case (r, s) => (msg.format(r, s) :: Nil, (), s)
    }

  def invokeService: ReaderWriterState[Config, List[String], Int, Int] =
    ReaderWriterState {
      case (cfg, invocationCount) => (
        List("Invoking service with port " + cfg.port),
        scala.util.Random.nextInt(100),
        invocationCount + 1
        )
    }

  val program: RWS[Config, List[String], Int, Int] = for {
    _   <- log("Start - r: %s, s: %s")
    res <- invokeService
    _   <- log("Between - r: %s, s: %s")
    _   <- invokeService
    _   <- log("Done - r: %s, s: %s")
  } yield res

  val (logMessages, result, invocationCount) = program.run(Config(80), 0)
  println("Result: " + result)
  println("Service invocations: " + invocationCount)
  println("Log: %n%s".format(logMessages.mkString("\t", "%n\t".format(), "")))
}