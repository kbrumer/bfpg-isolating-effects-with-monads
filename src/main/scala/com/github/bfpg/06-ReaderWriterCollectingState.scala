package com.github.bfpg

/*
 * Removing the mutable stack is something that is different and a bit trickier
 * to deal with compared to the read-only / write-only issues that we cleaned up
 * before. This is because each step of the computation depends on the state of
 * the previous step so we have to thread it through and make it readable and
 * writable.
 *
 * To do this, we're going to use a fold to recurse the stream and keep track
 * of the state and results that we're collecting.
 *
 * This is really crappy though. We're having to juggle the state each iteration
 * regardless of whether we're changing the stack or not
 */

import scala.collection.immutable.Stack
import scala.io.Source
import scala.xml.pull._
import scalaz._
import scalaz.std.list._
import scalaz.syntax.monad._
import scalaz.syntax.writer._

object ReaderWriterCollectingState {

  //YAY!
  //val config = Config( "  " )
  //val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  //var foundElems = mutable.Stack.empty[String]

  case class Config( indentSeq: String )
  type PpWriter[+A] = Writer[List[String],A]
  type PpReaderWriter[+A] = ReaderT[PpWriter,Config,A]
  type PpReaderWriterState[+A] = PpReaderWriter[(Stack[String],A)]
  type PpProgram = PpReaderWriterState[List[String]]

  def indented( level: Int, text: String ):PpReaderWriter[String] = Kleisli{
    (conf) => ((conf.indentSeq * level) + text).point[PpWriter]
  }

  def verifyNewElement(
    foundElems: Stack[String]
    ,event: XMLEvent
  ): PpReaderWriter[Unit] = Kleisli{ _ =>
     (foundElems.headOption,event) match {
       case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
         s"WARN: Msg should only contain text, contains: <$l>"
       ).tell
       case _ => Nil.tell
     }
  }

  def indentEvent(
    foundElems: Stack[String]
    , event: XMLEvent
  ):PpReaderWriterState[String] = event match {
    case EvElemStart( _ , l , _ , _ ) => {
      val out = indented( foundElems.size , s"<$l>" )
      out.map( ( foundElems.push( l ) , _ ) )
    }
    case EvElemEnd( _ , l ) => {
      val newStack = foundElems.pop
      indented( newStack.size , s"</$l>" ).map( ( newStack , _ ) )
    }
    case EvText(t) => indented( foundElems.size , t ).map( ( foundElems , _ ) )
  }

  def main(filename: String) = {
    val readerWriter = getStream( filename ).foldLeft[PpProgram](
      Kleisli{ _ => (Stack.empty , Nil).point[PpWriter] }
    )( (rw,event) => {
      for{
        s1 <- rw
        s2 <- indentEvent( s1._1 , event )
        _  <- verifyNewElement( s1._1 , event )
      } yield (s2._1,s2._2::s1._2)
    } )

    val (errors,(_,lines)) = readerWriter.run( Config("  ") ).run
    errors.foreach( System.err.println _ )
    lines.reverse.foreach( println _ )

  }

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

}

object ReaderWriterCollectingStateApp extends App {
  ReaderWriterCollectingState.main("test.xml")
}