/*
 * Here we hide that currying up in the reader monad, where instead of currying
 * we return a Reader[String,String] that will return a indented string once we
 * pass in the indentSeq
 *
 * When readers are sequenced together, the underlaying code chains the config
 * from the 1st reader to the second, so we get back a reader that we only have
 * to inject with the config once. We're really just building a little program
 * out of readers that will accept the conf and produce the final output.
 *
 * Sequence takes a list of readers and make a reader that returns a list.
 *
 * We've still got that dirty ugly mutable error list though, so lets tackle
 * that first.
 */

import scala.xml.pull._
import scala.io.Source
import scala.collection.mutable
import scalaz._
import std.list._
import syntax.traverse._

object ReaderMonad {

  case class Config( indentSeq: String )
  //val config = Config( "  " )
  val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  var foundElems: mutable.Stack[String] = mutable.Stack.empty[String]

  type PpReader[+A] = Reader[Config,A]

  def getIndentSeq(): PpReader[String] = Reader { (conf) => conf.indentSeq }

  def indented( level: Int, text: String ):PpReader[String] =
    getIndentSeq().map( indentSeq => ( indentSeq * level) + text )

  def verifyNewElement( event: XMLEvent ) = {
    (foundElems.headOption,event) match {
      case (Some("msg"),EvElemStart( _ , l , _ , _ )) => errors += (
        s"WARN: Msg should only contain text, contains: <$l>"
      )
      case _ => ()
    }
  }

  def main(filename: String) = {
    val readers: List[PpReader[String]] = for ( event <- getStream( filename ).toList ) yield {
      verifyNewElement(event)
      event match {
        case EvElemStart( _ , l , _ , _ ) => {
          val out = indented( foundElems.size , s"<$l>" )
          foundElems.push( l )
          out
        }
        case EvElemEnd( _ , l ) => {
          foundElems.pop()
          indented( foundElems.size , s"</$l>" )
        }
        case EvText(t) => indented( foundElems.size , t )
      }
    }

    errors.foreach( System.err.println _ )
    readers.sequenceU.apply( Config( "  " ) ).toList.foreach( println _ )

  }

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

}

object ReaderMonadApp extends App {
  ReaderMonad.main("test.xml")
}