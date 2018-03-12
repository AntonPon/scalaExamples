import scala.annotation.tailrec

// basic recursion
def Rpascal(column:Int, row:Int ): Int = {
  if (row == 1 || column == 1 || column > row)
    1
  else {
    Rpascal(column-1, row-1) + Rpascal(column, row-1)
  }

}


// tail recursion
def pascal(column: Int, row: Int): Int ={

  def rowCounter(prewRow: List[Int]): List[Int] ={
    var row = List[Int](1)
    for (i <- 1 to prewRow.length-1){
      val element = prewRow(i-1)+prewRow(i)
      row = row:+ element
    }
    row = row :+ 1
    row
  }

  @tailrec
  def recursion(column: Int, row: Int, i: Int, prewFrom: List[Int]): Int = {
    if (i == row){
      prewFrom(column-1)
    }else{
      recursion(column, row, i+1, rowCounter(prewFrom))
    }
  }
  if(column == 0|| column>row||row<=){
    1
  }else{
    recursion(column, row, 1, List[Int](1, 1))
  }
}

//println(tailPascal(5, 13000))

def tailRec(column: Int, row:Int): Int = {
  def tailpRec(i: Int, i1: Int, i2: Int, column: Int, row: Int, value1: Int): Int={
    if(column == 1){
      i/(i1 * i2)
    }
    else {
      println(column, value1, row, i1, i2)
      if (value1 == 1 && row == 1) {
        tailpRec(column * i, i1, i2, column - 1, 1, 1)
      } else if (value1 == 1) {
        tailpRec(column * i, i1 * row, i2, column - 1, row - 1, 1)
      } else if (row == 1) {
        tailpRec(column * i, i1, value1 * i2, column - 1, 1, value1 - 1)
      }
      else {
        tailpRec(column * i, row * i1, value1 * i2, column - 1, row - 1, value1 - 1)
      }
    }

  }
  if(row == 0 ) 1
  else if(row == 1 ) 1
  else tailpRec(1, 1, 1, column, row+1, column-row-1)

}

pascal(5, 10)
Rpascal(5, 10)