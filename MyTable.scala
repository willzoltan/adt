//package TABLE

import scala.swing._
import scala.swing.event._
import javax.swing.table._
import javax.swing.JFileChooser
import IO.File

class MyTableModel(var rowData: Array[Array[String]], val columnNames: Seq[String]) extends AbstractTableModel {
  override def getColumnName(column: Int) = columnNames(column).toString
  def getRowCount() = rowData.length
  def getColumnCount() = columnNames.length
  def getValueAt(row: Int, col: Int): String = rowData(row)(col).asInstanceOf[String]
  override def isCellEditable(row: Int, column: Int) = false
  def addRow(data: Array[String]) {
    rowData ++= Array(data.asInstanceOf[Array[String]])
  }
}

/* We will eventually remove the parameters from this class definition.
 * The main method will be moved here. */
object MyTable extends SimpleSwingApplication {

  val top = new MainFrame {

  	title = "MyTable - Kai and Andy 24th April"

    val xsize = 800; val ysize = 400
    val topPreferredSize  = new Dimension(xsize, ysize)
    preferredSize = topPreferredSize

     var row_num1 = 0; var row_num2 = 0
     var col_num1 = 0; var col_num2 = 0
     var info1 = new Array[Array[String]](row_num1)
     var info2 = new Array[Array[String]](row_num2)
     var columnheaders1: List[String] =  List()
     var columnheaders2: List[String] =  List()

    val tableModel1 = new MyTableModel(info1, columnheaders1) 
    val tableModel2 = new MyTableModel(info2, columnheaders2) 
    val table1      = new Table(row_num1, col_num1) { model = tableModel1 } 
    val table2      = new Table(row_num2, col_num2) { model = tableModel2 }

		/* Need to define these as null Files. NOTE: THIS WILL NOT COMPILE YET */
//    var file1 = new File
//    var file2 = new File

 		/* Contents of the MainFrame */
 		
    menuBar = new MenuBar {
      contents += new Menu("File") {
      //  contents += new MenuItem( Action("Exit") { sys.quit(0) } )
      }
    }

    val btnFile1 = new Button {
    text = "Find File 1"
    enabled = true
    }
    val btnFile2 = new Button {
    text = "Find File 2"
    enabled = true
    }
    
    val northFlow = new FlowPanel {
      contents += btnFile1
      contents += btnFile2
    }

		val scrTable1 = new ScrollPane(table1) {
      preferredSize = new Dimension (xsize/2, 200)
    	minimumSize = new Dimension(xsize/3, 200)
      horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
      verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
		}
		val scrTable2 = new ScrollPane(table2) {
      preferredSize = new Dimension (xsize/2, 200)
    	minimumSize = new Dimension(xsize/3, 200)
      horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
      verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
		}       
    val tableSplit = new SplitPane(Orientation.Vertical)
    tableSplit.leftComponent = scrTable1
    tableSplit.rightComponent = scrTable2

		val sldMatch = new Slider {
			min = 50
			max = 100
			majorTickSpacing = 10
			paintTicks = true
			paintLabels = true
		}
		val sldType = new Slider {
			min = 1
			max = 5
			majorTickSpacing = 1
			paintTicks = true
			paintLabels = true
			snapToTicks = true
		}
    val btnMatch = new Button("Generate Matches")
    
    val southFlow = new FlowPanel {
    	contents += sldMatch
    	contents += sldType
    	contents += btnMatch
    }

		val mainLayout = new BorderPanel {
			import BorderPanel.Position._
			add(northFlow, North)
			add(tableSplit, Center)
			add(southFlow, South)
		}

		contents = mainLayout

		/* Methods */
        
    listenTo(btnFile1)
    listenTo(btnFile2)
    listenTo(btnMatch)

    var file1:java.io.File = null
    var file2:java.io.File = null


    reactions += {
      case ButtonClicked(component) =>
      	if (component == btnFile1) {
      		fileSelector(1) match {
						case Some(file) => file1 = file
                               val csv = new File(file1.toString)
                               info1 = csv.returnArray(csv.rowCount, csv.columnCount)
                               columnheaders1 = csv.columnNames.toList
                               row_num1 = csv.rowCount
                               col_num1 = csv.columnCount
                               val newTableModel = new MyTableModel(info1, columnheaders1)
                               table1.model = newTableModel

      		}
				}
      	else if (component == btnFile2) {
      		fileSelector(2) match {
						case Some(file) => file2 = file
                               val csv = new File(file2.toString)
                               info2 = csv.returnArray(csv.rowCount, csv.columnCount)
                               columnheaders2 = csv.columnNames.toList
                               row_num2 = csv.rowCount
                               col_num2 = csv.columnCount
                               val newTableModel = new MyTableModel(info2, columnheaders2)
                               table2.model = newTableModel
      		}
				}               
				else if (component == btnMatch) {
					matchWindow()
				}                   
    }

    def fileSelector(id: Int): Option[java.io.File] = {
      val chooser = new JFileChooser()
      chooser.setCurrentDirectory(new java.io.File("."))
      chooser.setDialogTitle(s"Select File $id")
      chooser.setAcceptAllFileFilterUsed(false)
      if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
        System.out.println("getCurrentDirectory(): " + chooser.getCurrentDirectory())
        System.out.println("getSelectedFile(): " + chooser.getSelectedFile())
        return Some(chooser.getSelectedFile())
      } else {
        System.out.println("No Selection"); return None
      }			
    }

	/* To be completed: pop-up window triggered by clicking Generate Matches */
    def matchWindow() {

      //val matchfile = new Machine(info1, info2, sldMatch.value, sldType.value)


      val matchFrame = new Frame {
        preferredSize = new Dimension (400, 400)
        visible = true
        val matchtable1 = new Table(row_num1, col_num1) { model = table1.model }
        val scrTable1 = new ScrollPane(matchtable1) {
          preferredSize = new Dimension (xsize/2, 200)
          minimumSize = new Dimension(xsize/3, 200)
          horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
          verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
       }
        val matchtable2 = new Table(row_num2, col_num2) { model = table2.model }
       val scrTable2 = new ScrollPane(matchtable2) {
          preferredSize = new Dimension (xsize/2, 200)
          minimumSize = new Dimension(xsize/3, 200)
          horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
          verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
       }       
       val tableSplit = new SplitPane(Orientation.Vertical)
          tableSplit.leftComponent = scrTable1
          tableSplit.rightComponent = scrTable2
        contents = tableSplit
      }
    }
   
  }

}

