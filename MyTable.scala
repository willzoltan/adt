package TABLE

import scala.swing._
import javax.swing.table._

class MyTableModel( var rowData: Array[Array[String]], val columnNames: Seq[String] ) extends AbstractTableModel {
  override def getColumnName( column: Int) = columnNames(column).toString
  def getRowCount() = rowData.length
  def getColumnCount() = columnNames.length
  def getValueAt( row: Int, col: Int): String = rowData(row)(col).asInstanceOf[String]
  override def isCellEditable( row: Int, column: Int) = false
  def addRow( data: Array[String]) {
    rowData ++= Array(data.asInstanceOf[Array[String]])
  }
}
 
class MyTable(info:Array[Array[String]], columnheaders:List[String], row_num:Int, col_num:Int, find:Array[Int] ) extends SimpleSwingApplication {
  val top = new MainFrame {
    title = "MyTable"
    preferredSize  = new Dimension( 300,300 )
    val tableModel = new MyTableModel(info, columnheaders) 
    val table      = new Table( row_num, col_num ) { model = tableModel } 
    var str:String = ""
    for (x <- find) {
      str = str ++ x.toString ++ " "
    }
    contents = new BorderPanel {
      import BorderPanel.Position._
      add(new ScrollPane(table), Center)
      add(new Button(s"String abc matches on line $str "), South)
    }

  }

}