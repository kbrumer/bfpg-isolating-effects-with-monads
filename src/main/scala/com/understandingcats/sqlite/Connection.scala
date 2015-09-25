package com.understandingcats.sqlite

import java.sql.{Connection, Statement}

import org.sqlite.{SQLiteConfig, SQLiteDataSource}

import scala.collection.mutable.ListBuffer

// old school connection
object ConnectionProvider {

  val config: SQLiteConfig = {
    val cfg = new SQLiteConfig()
    cfg.enableFullSync(true)
    cfg.setReadOnly(false)
    cfg
  }

  val conn: Connection = {
    Class.forName("org.sqlite.JDBC")
    val ds = new SQLiteDataSource(config)
    ds.setUrl("jdbc:sqlite:db/dev.db")
    val c = ds.getConnection
    c.setAutoCommit(false)
    c
  }

  def getResultSet(sql: String): List[String] = {
    val c: Connection = conn
    var stmt: Statement = null
    val rows = new ListBuffer[String]
    try {
      stmt = c.createStatement()
      val rs = stmt.executeQuery(sql)
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
      rs.close()
      stmt.close()
      c.close()
      rows.toList
    } catch {
      case ex: Exception => rows.toList
    }
  }

}


object ConnectionTest extends App {
  val xs = ConnectionProvider.getResultSet("SELECT * FROM widget")
  println("ConnectionTest xs ==> \n" + xs.mkString("\n"))
}