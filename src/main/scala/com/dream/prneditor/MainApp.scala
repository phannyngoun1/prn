package com.dream.prneditor

import scala.io.Source

object MainApp {


  case class lineReading(
    index: Int,
    lineLabel: String,  // header, item, footer, unknown, item_header
    lineText: String,
  )

  case class Invoice(
    invoiceNo: String,
    invoiceDate: String,
    soNo: String,
    billTo: String,
    cCode: String,
    vat: String,
    contact: String,
    paymentTo: String,
    accountName: String,
    accountNo: String,
    bankName: String,
    grandTotal: BigDecimal,
    exchangeRate: Double,
    currency: String = "KH",
    items: List[InvItem]
  )

  case class InvItem(
    seq: Int,
    itemCode: String,
    itemDesc: String,
    qty: Int,
    amount: BigDecimal
  )

  def main(args: Array[String]): Unit = {

    val excludeString = List("Authorized Signature", "___________________________________________________________________")

    val headerPattern = List("Bill To:", "Invoice  No.:", "Invoice  Dt.:", "S.O.No.     :", "C.Code :", "Contract No.:", "VAT    :", " Contract Dt.:", "Contact:", "Terms:")
    val footerPattern = List("or Bank Transfer payable to :", "Account Name:", "Account No  :", "Bank Name   :")
    val itemHeaderPattern = List("Line Item Code", "Item Description", "No.of Spot", "US$ Amount")

    def readString(str: String) = {

    }

    val invoidList =

    for (line <- Source.fromFile("E:\\test\\PI0004.PRN").getLines
      .map(_.trim)
      .filter(
          item => item.length > 0 &&
            !excludeString.exists(_.equalsIgnoreCase(item)) &&
            !item.contains("_________________")
      ).zipWithIndex
      .map {
        case (str, index) if headerPattern.filter(exp => str.contains(exp)).length> 0 => lineReading(index, "header", str)
        case (str, index) if footerPattern.filter(exp => str.contains(exp)).length> 0 => lineReading(index, "footer", str)
        case (str, index) if itemHeaderPattern.filter(exp => str.contains(exp)).length> 0 => lineReading(index, "item_header", str)
        case (str, index) => lineReading(index, "unknown", str)
      }
    ) {
      println(s"${line.index}, ${line.lineLabel}, ${line.lineText} ")
    }
  }

}
