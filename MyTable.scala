//package TABLE

import scala.swing._
import scala.swing.event._
import javax.swing.table._
import javax.swing.JFileChooser
import IO.File

class MyTableModel(var rowData: Array[Array[String]], val columnNames: Seq[String]) extends AbstractTableModel {
  override def getColumnName(column: Int) = columnNames(column).toString
  def getRow(i: Int): Array[String] = rowData(i)
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
     var columnheaders1: List[String] =  List()
     var columnheaders2: List[String] =  List()
     var tableData1 = new Array[Array[String]](0)
     var tableData2 = new Array[Array[String]](0)

    val tableModel1 = new MyTableModel(new Array[Array[String]](row_num1), columnheaders1) 
    val tableModel2 = new MyTableModel(new Array[Array[String]](row_num2), columnheaders2) 
    val table1      = new Table(row_num1, col_num1) { model = tableModel1 } 
    val table2      = new Table(row_num2, col_num2) { model = tableModel2 }

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
    	minimumSize = new Dimension(xsize/3, 200)
      horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
      verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
		}
		val scrTable2 = new ScrollPane(table2) {
    	minimumSize = new Dimension(xsize/3, 200)
      horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
      verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
		}       
    val tableSplit = new SplitPane(Orientation.Vertical)
    tableSplit.leftComponent = scrTable1
    tableSplit.rightComponent = scrTable2

		val sldMatch = new Slider {
			min = 0
			max = 100
			preferredSize = new Dimension(300, 50)
			majorTickSpacing = 10
			paintTicks = true
			paintLabels = true
		}
		
		val cmbType = new ComboBox (List("Equality of all columns","Equality on 1 column","N percent matching columns"))
    val btnMatch = new Button("Generate Matches")
    
    val southFlow = new FlowPanel {
    	contents += sldMatch
    	contents += cmbType
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

    var file1: IO.File = null
    var file2: IO.File = null
    
    
    
    reactions += {
      case ButtonClicked(component) =>
      	if (component == btnFile1) {
      		fileSelector(1) match {
						case Some(file) => {
                               file1 = new File(file.toString)
                               columnheaders1 = file1.columnNames.toList
                               row_num1 = file1.rowCount
                               col_num1 = file1.columnCount
                               tableData1 = file1.returnArray(file1.rowCount, file1.columnCount)
                               val newTableModel = new MyTableModel(file1.returnArray(file1.rowCount, file1.columnCount), columnheaders1)
                               table1.model = newTableModel
                               }
            case None => {}

      		}
				}
      	else if (component == btnFile2) {
      		fileSelector(2) match {
						case Some(file) =>  {
                               file2 = new File(file.toString)
                               columnheaders2 = file2.columnNames.toList
                               row_num1 = file2.rowCount
                               col_num1 = file2.columnCount
                               tableData2 = file2.returnArray(file2.rowCount, file2.columnCount)
                               val newTableModel = new MyTableModel(file2.returnArray(file2.rowCount, file2.columnCount), columnheaders2)
                               table2.model = newTableModel
                               }
            case None => {}
      		}
				}               
				else if (component == btnMatch) {
					if (file1 != null && file2 != null) matchWindow() else println("No files")
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
    
      //TODO: implement a drop down list instead of cmbType with an option for each type of match.
      cmbType.selection.item match {
        case "Equality of all columns" => println("EqAll works"); val matchfile = new EqualityMachine(file1, file2)
        case "Equality on 1 column" => val matchfile = new SingleEqualityMachine(file1, file2)
        case "N percent matching columns" =>  val matchfile = new PercentageEqualityMachine(file1, file2,sldMatch.value)
      }
      
      val matchfile = new EqualityMachine(file1, file2)
      
//      val rowsWithMatches: List[Int] = matchfile.rowsWithMatches
      val arrayOfMatches: Array[List[Int]] = matchfile.arrayOfMatches
      val rowsWithMatches = List(0,2)
      arrayOfMatches(0) = List(4,5)
      arrayOfMatches(2) = List(1,2,3)
      for (i <- arrayOfMatches) println(i)
      
      // TODO: Implement the left hand side table to load in the rows that have matches (stored in rowsWithMatches as list of indicies). And clicking on row(i) brings up the matches on the right hand side.
  
  /* We want them to append the row numbers to the start of every column. */
  
  		var notMatched: List[Int] = List()
  		for (i <- 0 until arrayOfMatches.length) {
  			if (arrayOfMatches(i) == Nil) notMatched ::= i
  		}
  		
  		val arraySize = arrayOfMatches.map(x => x.length).sum
  		println("arrayOfMatches size: " + arraySize)
  		var newTableModel1 = new Array[Array[String]](arraySize + notMatched.length)
  		var oldRow = 0
  		var newRow = 0
  		for (i <- 0 until rowsWithMatches.length) {
  			oldRow = rowsWithMatches(i)
  			newTableModel1(newRow) = tableData1(oldRow)
  			val num = arrayOfMatches(oldRow).length
  			for (k <- 1 to num - 1) {
  				newRow += 1
  				val blankRow = new Array[String](col_num1) 
  				blankRow.map(x => "-")
  				blankRow(0) = oldRow.toString
  				newTableModel1(newRow) = blankRow
  			}
  			newRow += 1
  		}
  		
  		
  		var newTableModel2 = new Array[Array[String]](arraySize)
  		var rownums2: List[Int] = List()
  		for (i <- arrayOfMatches) rownums2 = rownums2 ::: i
  		println(rownums2)
  		for (j <- 0 until rownums2.length) {
  			newTableModel2(j) = tableData2(rownums2(j))
  		}

  		val newTable2 = new Table { model = new MyTableModel(newTableModel2, columnheaders2) }
  		
  		var curRow = rownums2.length
  		for (i <- 0 until notMatched.length) {
  			newTableModel1(curRow+i) = tableData1(notMatched(i))
  		}
  		
  		val newTable1 = new Table { model = new MyTableModel(newTableModel1, columnheaders1) }
  		
  		
  		
  		
  		
  		
  		
  		
  		
  		
  		
  		
  		
  		
  		
  

      val matchFrame = new Frame {
        preferredSize = new Dimension (xsize, ysize)
        visible = true
        val matchtable1 = new Table(arraySize, col_num1) { model = newTable1.model }
        val scrTable1 = new ScrollPane(matchtable1) {
          preferredSize = new Dimension (xsize/2, 200)
          minimumSize = new Dimension(xsize/3, 200)
          horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
          verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
        }
        
        val matchtable2 = new Table(arraySize, col_num2) { model = newTable2.model }
        
        val scrTable2 = new ScrollPane(matchtable2){
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

