package com.understandingcats.sqlite

import java.sql.{Connection, ResultSet, SQLException, Statement}

import scala.collection.mutable.ListBuffer
import scala.util.Try

object LoanPattern {

  def withResultSet[R](connection: Connection)(query: String)(f: ResultSet => R): R = {
    var statement: Statement = null
    var rs: ResultSet = null
    try {
      statement = connection.createStatement()
      rs = statement.executeQuery(query)
      f(rs)
    } catch {
      case sqle: SQLException =>
        System.err.print("Error executing statement", sqle)
        throw sqle
    } finally {
      Try {
        if (rs != null) {
          rs.close()
        }
      }
      Try {
        if (statement != null) {
          statement.close()
        }
      }
    }
  }

  def withConnection[R](f: Connection => R): R = {
    var connection: Connection = null
    try {
      connection = ConnectionProvider.conn
      f(connection)
    } catch {
      case sqle: SQLException =>
        System.err.print("Error in connection", sqle)
        throw sqle
    } finally {
      Try {
        if (connection != null) {
          connection.close()
        }
      }
    }
  }

}


object LoanPatternApp extends App {
  import com.understandingcats.sqlite.LoanPattern._
  val xs =
    withConnection{ conn =>
      withResultSet(conn)("SELECT * FROM widget"){ rs =>
        val rows = new ListBuffer[String]
        val rsmd = rs.getMetaData
        val columnCount = rsmd.getColumnCount
        while (rs.next()) {
          var row = ""
          var i = 1
          while (i <= columnCount) {
            val cname = rsmd.getColumnName(i)
            row = s"$row $cname => ${rs.getObject(i)}, "
            i += 1
          }
          rows += row
        }
        rows.toList
      }
    }

  println("LoanPatternApp xs ==> \n" + xs.mkString("\n"))


}