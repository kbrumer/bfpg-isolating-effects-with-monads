package net.debasishg.snippet.domainpatterns

import java.util.Date
import net.debasishg.snippet.domainpatterns.specifications.{ShippingOrder, LineItem, ItemA, Customer}

import scalaz._
import scalaz.\/._

import scala.language.higherKinds

/**
 *
 * from:
 * https://github.com/debasishg/scala-snippets/tree/master/src/main/scala
 *
 */
object specifications {
  type ValidationStatus[S] = \/[String, S]

  // type ReaderTStatus[-A, +S] = ReaderT[ValidationStatus[S], A, S]

  // type ReaderTStatus[-A, +S] = Kleisli[ValidationStatus, A, S]

  object ReaderTStatus extends KleisliFunctions with KleisliInstances {
    def apply[ValidationStatus[+_], A, S](f: A => ValidationStatus[S]): Kleisli[ValidationStatus, A, S] = kleisli(f)
  }

//  object ReaderTStatus extends KleisliInstances with KleisliFunctions {
//    def apply[M[_], A, S](f: A => ValidationStatus[S]) = kleisli(f)
//  }

  sealed trait Item {
    def itemCode: String
  }
  case class ItemA(itemCode: String, desc: Option[String], minPurchaseUnit: Int) extends Item
  case class ItemB(itemCode: String, desc: Option[String], nutritionInfo: String) extends Item

  case class LineItem(item: Item, quantity: Int)

  case class ShippingOrder(orderNo: String, orderDate: Date, customer: Customer, lineItems: List[LineItem])

  case class Customer(custId: String, name: String, category: Int)

//  def isReadyForFulfilment(order: ShippingOrder) = {
//    val kleisli: Kleisli[ValidationStatus, ShippingOrder, Boolean] = for {
//      _ <- validate
//      _ <- approve
//      _ <- checkCustomerStatus(order.customer)
//      c <- checkInventory
//    } yield c
//    // val s = kleisli
//    // s(order)
//  }
//
//  private def validate = ReaderTStatus[ShippingOrder, Boolean] { order =>
//    if (order.lineItems.isEmpty) left(s"Validation failed for order $order") else right(true)
//  }
//
//  private def approve = ReaderTStatus[ShippingOrder, Boolean] { order =>
//    println("approved")
//    right(true)
//  }
//
//  private def checkCustomerStatus(customer: Customer) = ReaderTStatus[ShippingOrder, Boolean] { order =>
//    right(true)
//  }
//
//  private def checkInventory = ReaderTStatus[ShippingOrder, Boolean] { order =>
//    println("inventory checked")
//    right(true)
//  }
}

object SpecificationsApp extends App {
  val customer = Customer("asdf", "Fred Flintstone", 1)
  val now = new Date()
  val item1 = ItemA("s123", Some("stones"), 1)
  val item2 = ItemA("b123", Some("dino bones"), 1)
  val lineItems : List[LineItem] = LineItem(item1, 3) :: LineItem(item2, 6) :: Nil

  val order = ShippingOrder("1234", now, customer, lineItems)

  println("Done!")

}