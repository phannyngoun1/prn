package com.dream.prneditor

import scala.io.Source

object MainApp {
  case class LineReading(
    index: Int,
    invoiceNo: String,
    lineLabel: String,  // header, item, footer, unknown, item_header
    lineText: String
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

  case class Field(
    name: String,
    detectName: String,
    fromSingleLine: Boolean = true,
    lineLabelIdentify: String ="None"
  )

  case class InvoiceField(
    fields: List[FieldValue],
    lineITemFields: List[InvoiceItemField]
  )

  case class InvoiceItemField(
    seq: Int,
    list: List[FieldValue]
  )

  case class FieldValue(
    field: Field,
    value: String
  )

  val billTo = Field("BillTo" , "Bill To:", false, "BILL_TO")
  val invoiceNo = Field("invoiceNo" , "Invoice  No.:")
  val invoiceDt = Field("invoiceDt" , "Invoice  Dt.:")
  val soNo = Field("so" , "S.O.No.     :")
  val cCode = Field("cCode" , "C.Code :")
  val vatIn = Field("vatTin", "VAT TIN:")

  val contractNo = Field("contractNo" , "Contract No.:")
  val vat = Field("vat" , "VAT    :")
  val contractDt = Field("contractDt" , "Contract Dt.:")
  val contract = Field("contract" , "Contact:")
  val term = Field("contract" , "Terms:")
  val bank =  Field("bankNo" , "or Bank Transfer payable to :")
  val accountName = Field("accountName" , "Account Name:")
  val accountNo = Field("accountNo" , "Account No  :")
  val bankName = Field("bankName" , "Bank Name   :", false, "BANK_NAME")

  val lineItem = Field("lineItem", "")
  val lineItemDesc = Field("lineItemDesc", "")
  val qty = Field("qty", "")
  val amount = Field("amount", "")

  val total = Field("total", "Total  :")
  val vatAmount = Field("vatAmount", "VAT 10%:")
  val grandTotal = Field("grandTotal", "GRAND TOTAL:")
  val exchangeRate = Field("exchangeRate", "Exchange Rate/USD:")
  val pageNo = Field("pageNo", "Page No.:")

  val filedList = List (
    billTo, invoiceNo, invoiceDt, soNo, cCode, contractNo, vat, vatIn,
    contractDt,contract, term, bank, accountName, accountNo, bankName,
    total, vatAmount, grandTotal, exchangeRate, pageNo

  )

  def findInvNo(value: String, lastInvNo: String) = {
    val invoiceNoPos = value.indexOf("Invoice  No.:")

    if(invoiceNoPos> 0)
      value.substring(invoiceNoPos + "Invoice  No.:".length ).trim
    else lastInvNo
  }

  def findLineItem(value: String, isLineItem: Boolean)  = {

    if(value.contains("Line Item Code   Item Description              No.of Spot   US$ Amount")) true
    else if(isLineItem  && value.contains(total.detectName)) false
    else isLineItem

  }

  def findLineLabel(value: String, genLabelLine: String , labelLine: String, startField: Field, endField: List[Field] ): String = {
    if(value.contains(startField.detectName)) labelLine
    else  if( endField.filter( item =>  value.contains(item.detectName) ).length> 0  ) "NONE"
    else  genLabelLine
  }

  def insertSeparator (value: String) = {

    var newVal = value
    filedList.foreach( f =>
      newVal = newVal.replaceAll(f.detectName, s"####${f.detectName}")
    )
    newVal + "####"
  }


  def readField(field: Field, value: String): Option[FieldValue] = {

    value.split("####")
      .filter(_.contains(field.detectName))
      .headOption.map( v => FieldValue(field, v.replace(field.detectName,"").trim))
      .headOption
  }


  def readMultiLineCombined(field: Field, values: List[String]): Option[FieldValue]  = {

    if(values.isEmpty) None
    else {
      val value = values.map(_.split("####").filter(!_.trim.isEmpty).headOption.map(_.replace(field.detectName, "") ).map(_.trim).getOrElse("")).mkString(" ")
      Some(FieldValue(field, value))
    }
  }

  def readLineItem( lineReading: LineReading ) = {

    val fields = lineReading.lineText
      .split("  ")
      .map(_.trim)
      .filter(!_.isEmpty)
      .zipWithIndex
      .map(item =>  {
        val field = item._2 match {
          case 0 => lineItem
          case 1 => lineItemDesc
          case _ => qty
        }
        FieldValue(field, item._1)
      }).toList.reverse

    val fi = fields.head.copy(field = amount) :: fields.tail

    InvoiceItemField(lineReading.index, fi )

  }


  def main(args: Array[String]): Unit = {

    val excludeString = List("Authorized Signature", "_______________________________")

    var lastInvNo ="None"
    var isLineItem = false
    var labelLine = "NONE"

    val reader = Source.fromFile("E:\\test\\PI0004.PRN").getLines
      .map(_.trim)
      .filter(
        item => item.length > 0 && excludeString.filter(exStr =>item.contains(exStr)).length ==0

      ).zipWithIndex
      .map(f => {
        lastInvNo = findInvNo(f._1, lastInvNo)
        isLineItem = findLineItem(f._1, isLineItem)

        labelLine  = findLineLabel(f._1, labelLine, "BILL_TO", billTo, List(cCode) )

        labelLine  = findLineLabel(f._1, labelLine, "BANK_NAME", bankName, List(vatIn, Field("","VAT INVOICE")))

        labelLine  = findLineLabel(f._1, labelLine, "LINE_ITEM", Field("", "Line Item Code   Item Description              No.of Spot   US$ Amount"), List(total) )

        LineReading(f._2, lastInvNo, labelLine ,  insertSeparator(f._1))

      }).toList.groupBy(_.invoiceNo)
      .map(line => {
        println(s"Invoice ----------------------------------")
          line._2.map( item => {
            println(s"### ${item}")
            filedList
              .filter(_.fromSingleLine)
              .map(field =>  readField(field, item.lineText))
              .filter(_.isDefined)
              .map(item => println( s"===> ${item}"))

          })
        line
      })
      .map( item => {
        val items = item._2.map( item => filedList.filter(_.fromSingleLine)
          .map(field =>  readField(field, item.lineText)).filter(_.isDefined)).map(_.map(_.get)).reduce(_ ::: _)

        val multiLineField = filedList.filter(!_.fromSingleLine).map( field => {
          readMultiLineCombined(field, item._2.filter(_.lineLabel.equalsIgnoreCase(field.lineLabelIdentify)).map(_.lineText))
        }).filter(_.isDefined).map(_.get)

        val itemLines = item._2
          .filter(_.lineLabel.equalsIgnoreCase("LINE_ITEM"))
          .filter(!_.lineText.contains("Line Item Code   Item Description              No.of Spot   US$ Amount"))
          .map(readLineItem)
        InvoiceField(items ::: multiLineField , itemLines)
      })
      .filter(_.fields.exists( filed =>   (filed.field.name.equalsIgnoreCase("invoiceNo"))) )

    for (line <- reader ) {
      println(
        s""""
          |Invoice No: ${line.fields.filter(_.field == invoiceNo).headOption.map(_.value).getOrElse("N/A")}
          |
          |
          |
          |
          |
        """.stripMargin)
    }
  }

}
