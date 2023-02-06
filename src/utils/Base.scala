package base
import base.WideCell.extractValue
import org.apache.poi.hssf.usermodel.{HSSFCell, HSSFRow, HSSFSheet}
import org.apache.poi.ss.util.CellRangeAddress

import scala.util.matching.Regex

object Base {
  val name_pat: Regex = "[a-zA-Z][a-zA-Z0-9_]*".r
  val hex_pat: Regex = "([0-9]+)'h([0-9a-fA-F]+)".r
  val bin_pat: Regex = "([0-9]*)'b([0-1]+)".r
  val int_pat: Regex = "[0-9]+".r
  val bit_pat: Regex = "([0-9]+)x".r
  val array_pat: Regex = "([0-9]+|string|int)x([0-9$]+)".r
  val hex2bin_map = Map('f' -> "1111", 'e' -> "1110", 'd' -> "1101", 'c' -> "1100", 'b' -> "1011", 'a' -> "1010",
    '9' -> "1001", '8' -> "1000", '7' -> "0111", '6' -> "0110", '5' -> "0101", '4' -> "0100", '3' -> "0011",
    '2' -> "0010", '1' -> "0001", '0' -> "0000")
  def hex2bin(hex:String):String = {
    hex match {
      case hex_pat(width, num) =>
        val bin = num.map(hex2bin_map(_)).mkString("")
        s"${width}'b${bin}'"
      case oth => oth
    }
  }
  def slice_bin(bin:String):Vector[String] = {
    bin match {
      case bin_pat(width, body) =>
        val wd = width.toInt
        (if (bin.length < wd) "0" * (wd - bin.length) + bin else bin.takeRight(wd)).map {
          case '0' => "1'b0"
          case '1' => "1'b1"
        }.toVector
      case _ => throw new Exception(s"unexpectied string in slice_bin : ${bin}")
    }
  }
  def read_row(row: HSSFRow, start: Int = 0, offset: Int = -1): Vector[String] = {
    val length = if (offset < 0) row.getPhysicalNumberOfCells else offset
    val cells = (start until start + length).map(i => row.getCell(i) match {
      case null => ""
      case other => other.toString.stripSuffix(".0")
    }
    ).toVector.filter(_ != "")
    cells
  }
  def read_cell(row: HSSFRow)(idx:Int):String = {
    row.getCell(idx) match {
      case null => ""
      case oth => oth.toString.stripSuffix(".0")
    }
  }
  def read_len(row: HSSFRow)(start:Int, len:Int):Vector[String] = {
    val rd_cel = read_cell(row)(_)
    (start until start + len).map(rd_cel(_)).toVector
  }
  def read_until(row: HSSFRow)(start:Int, end:String):Vector[String] = {
    def rd_cel = read_cell(row) _
    def go(idx:Int, cur:Vector[String]):Vector[String] = {
      val cur_cel = rd_cel(idx)
      if(cur_cel == end) return  cur
      else go(idx+1, cur :+ cur_cel)
    }
    go(start, Vector())
  }
  //  def cal_cell(sheet:HSSFSheet, position:(Int, Int)):String  //
  //  // 1. case/casex condition-val pairs ... endcase
  //  // 2. if(condition) get_val()  else get_val
  //  // 3. function result (or directly vals),surrounded by begin/end or {}, last expression val is the expected result
  //  // parse left to right till val_width + condition_width, row by row,
  //  def read_cell(sheet:HSSFSheet, position:(Int, Int)):String //read getOrElse("")
  //  def translate(arr:Array[String]):Array[String] //fulfill abbr/translate special tag
  //  def concatenation(arr:Array[String]):String// surround items with {}
  //  def merge(arr:Array[String]):String //if (serial items:num decrease, non_num same) -> merge to one item (must contain [])
  def merge(vec: Vector[Any]): Vector[Any] = {
    vec.foldLeft(Vector[Any]()){(cur_vec:Vector[Any], cur_val:Any) =>
      if(cur_vec.isEmpty) Vector[Any](cur_val)
      else {
        (cur_vec.last, cur_val) match {
          case (a:Bit, b:Bit) => cur_vec.init :+ (a ++ b)
          case (a:Idx, b:Idx) => {
            if(a.src != b.src) cur_vec :+ cur_val
            else if(a.right-1 != b.right) cur_vec :+ cur_val
            else cur_vec :+ Idx(a.src, a.left, b.right)
          }
          case _ => cur_vec :+ cur_val
        }
      }
    }
  }
}
case class Comb(str:String){
  override def toString: String = str
}
case class WideCell(rowNumber:Int, leftCloumn:Int, rightCloumn:Int, liberal: String, value:Any, keyword:String, width:Int, cp:WideCell=null){
  def expand:Vector[WideCell] = {
//    keyword match {
//      case "expr" =>
//    }
    val copy_pat = "(\\d+)(?::(\\d+))?".r
    val replace_pat = "\\\\\\{(.+)}%\\((.+)\\)".r
    val replace_pat(org, idxes) = this.liberal
    val idx_arr = idxes.split(",").map{_.trim()}.toVector
    val idx_int_arr_tmp = idx_arr.map {
      case copy_pat(left, right) if right != null => (left.toInt, right.toInt, if(left.toInt <= right.toInt) 1 else -1)
      case copy_pat(left, _) => (left.toInt, left.toInt, 1)
    }
    val copy_num = idx_int_arr_tmp.map{ case (i, i1, i2) => (i - i1).abs+1}.max
    val tmp: Vector[Vector[Int]] = idx_int_arr_tmp.map{case(i, i1, i2) => if(i == i1) Vector.fill(copy_num)(i) else (i to i1 by i2).toVector}
    val tmp2 = tmp.transpose
    val target = tmp2.map{vars => vars.zipWithIndex.foldLeft(org){(str:String, ele:(Int, Int)) => {
      str.replace("$"+ele._2.toString, ele._1.toString)
    }}}.map("{"+_+"}")
    val expanded_cells = target.map(exp => new WideCell(rowNumber, leftCloumn, rightCloumn, exp, extractValue(exp), "expr", this.width))
    expanded_cells
  }
  def widthExpand(left:Int, right:Int): WideCell = {
    new WideCell(this.rowNumber, left, right, this.liberal, this.value, this.keyword, right-left+1)
  }
  def valueReplace(liberal: String, value: Any, keyword: String): WideCell = {
    new WideCell(this.rowNumber, this.leftCloumn, this.rightCloumn, liberal, value, keyword, this.width)
  }
  def completeSrc(idx:Idx): WideCell = {
    import WideCell.seg_pat
    val seg_pat(width) = this.liberal
    val completed = idx.take(width.toInt)
    valueReplace(completed.toString, completed, "idx")
  }


  def valueWidth:Int = {
    value match {
      case v:Int => Bit(v).width
      case _:String => 1
      case v:Bit => v.width
      case v:Idx => v.width
//      case _:Comb => null
    }
  }
  override def toString: String = s"${keyword}:${liberal}, @row:${rowNumber}, column:${(leftCloumn, rightCloumn)}"
}
object WideCell{
  val bit_pat: Regex = "\\d*'(?:h\\h+|d\\d+|b\\b+)".r
  val str_pat: Regex = "\"[^\"]*\"".r
  val int_pat: Regex = "(\\d+)(?:\\.0+)?".r
  val idx_pat: Regex = "(\\w+)(?:\\[(\\d+):?(\\d+)?])?".r
  val seg_pat: Regex = "(\\d+)x".r
  val ext_pat: Regex = ".+#\\d.*%(?:\\((.+)\\))?".r
  def extractLine(rowNum: Int, start:Int, sheet: HSSFSheet):Vector[WideCell] = {
    val wideCells = lineReader(sheet.getRow(rowNum))(start, "//")
    val mergedCelss = this.merge(wideCells, sheet)
    mergedCelss
  }
  def extractLine(row: HSSFRow, start:Int):Vector[WideCell] = {
    val wideCells = lineReader(row)(start, "//")
    val mergedCelss = this.merge(wideCells, row.getSheet)
    mergedCelss
  }
  def extractKeyword(str:String):String = {
    str match {
      case bit_pat() => "bit"
      case str_pat() => "string"
      case int_pat(_) => "int"
      case idx_pat(_, _, _) => "index"
      case seg_pat(_) => "seg"
      case _ => "expr"
    }
  }
  def liberalWrap(str: String):String = {
    str match {
      case int_pat(v) => v
      case bit_pat() => Bit(str).toBin
      case oth => oth
    }
  }
  def extractValue(str: String):Any = {
    str match {
      case int_pat(v) => v
      case bit_pat() => Bit(str)
      case str_pat() => str
      case idx_pat(_, _, _) => Idx(str)
      case seg_pat(_) => str
      case _ => Comb(str)
    }
  }
  def completeSrc(cells:Vector[WideCell], src: String): Vector[WideCell] = {
    val segs = cells.filter(cell => cell.keyword == "seg")
    val width_array: Vector[Int] = segs.map(_.liberal).map{case seg_pat(width) => width.toInt}
    val overall_width = width_array.sum
    val idx_src = Idx(src)
    val trans: Vector[Idx] = idx_src.slice(width_array)
    def go(idx:Int, cur: Vector[WideCell], left:Vector[Idx]):Vector[WideCell] = {
      if(idx == cells.length || left.isEmpty) cur
      else cells(idx) match {
        case i if i.keyword == "seg" => go(idx+1, cur:+ cells(idx).completeSrc(left.head), left.tail)
        case _ => go(idx+1, cur :+ cells(idx), left)
      }
    }
    go(0, Vector[WideCell](), trans)
  }

//  def expandComb(org:Vector[WideCell]):Vector[WideCell] = {
//
//  }
//  def bind(src:Vector[WideCell], tar:Vector[WideCell]):Vector[WideCell] = {

//}

  private def lineReader(row: HSSFRow)(start:Int, until:String): Vector[WideCell] = {
    def go(idx:Int, cur:Vector[WideCell]):Vector[WideCell] = {
      val cur_cel: HSSFCell = row.getCell(idx)
      val cell_content = liberalWrap(if(cur_cel == null) "" else cur_cel.toString)
      val cur_wideCell = WideCell(row.getRowNum, idx, idx, cell_content, extractValue(cell_content), extractKeyword(cell_content), 1)
      if (cell_content == until || idx >= row.getLastCellNum) cur
      else go(idx + 1, cur :+ cur_wideCell)
    }
    go(start, Vector())
  }
  def getMergedCell(wideCell: WideCell, sheet: HSSFSheet): Option[CellRangeAddress] = {
    val merged_cells = (0 until sheet.getNumMergedRegions).map(i => sheet.getMergedRegion(i))
    val possible: Option[CellRangeAddress] = merged_cells.find{ i =>
      (i.getFirstRow == wideCell.rowNumber) && (i.getLastRow == wideCell.rowNumber) && (i.getFirstColumn <= wideCell.leftCloumn) && (i.getLastColumn >= wideCell.rightCloumn)}
    possible
  }
  def merge(cells: Vector[WideCell], sheet: HSSFSheet):Vector[WideCell] = {
    val strip_empty_merge = cells.filter{i => !(i.liberal == "" && getMergedCell(i, sheet).nonEmpty)}
    val cell_extend: Vector[WideCell] = strip_empty_merge map { i =>
      val tmp_merged_cell = getMergedCell(i, sheet)
      if(tmp_merged_cell.nonEmpty) i.widthExpand(tmp_merged_cell.get.getFirstColumn, tmp_merged_cell.get.getLastColumn)
      else i
    }
    cell_extend.filter(i => i.liberal != "")
  }
}
class Bit(val value: Vector[Boolean]=Vector(false), val width:Int=0){
//  val width = if(width_tmp <= 0) value_tmp.length else width_tmp
//  val value = if(width >= value_tmp.length) Vector.fill(width-value_tmp.length)(false) ++ value_tmp else value_tmp.takeRight(width)
  def toBin: String = s"$width'b" + value.map(if(_) "1" else "0").mkString("")
  def toInt: Int = value.reverse.zipWithIndex
    .map{case (bit, weight) => if(bit) math.pow(2, weight) else 0}.sum.toInt
  def toDec: String = s"$width'd" + toInt.toString
  def toHex: String = s"$width'h" + toInt.toHexString
  override def toString: String = toHex
  def or(tar: Bit): Bit = if (width != tar.width) throw new Exception(s"width mismatch between $width and ${tar.width}") else Bit((this.value zip tar.value).map { case (a, b) => a | b }, width)
  def |(tar: Bit): Bit = or(tar)
  def and(tar: Bit): Bit = if (width != tar.width) throw new Exception("width mismatch") else Bit((this.value zip tar.value).map { case (a, b) => a & b }, width)
  def &(tar:Bit) = and(tar)
  def not: Bit = Bit(this.value.map(!_), width)
  def orR: Bit = Bit(Vector(this.value.reduce(_ | _)), 1)
  def andR: Bit = Bit(Vector(this.value.reduce(_ & _)), 1)
  def norR: Bit = Bit(Vector(this.value.reduce(_ ^ _)), 1)
  def xnorR: Bit = Bit(Vector(!this.value.reduce(_ ^ _)), 1)
  def ++ (tar: Bit): Bit = {
    Bit(value ++ tar.value, width+tar.width)
  }
}

object Bit{
  val bin_pat: Regex = "(\\d*)'b([0-1]+)".r
  val hex_pat: Regex = "(\\d*)'h([0-1a-fA-F]+)".r
  val dec_pat: Regex = "(\\d*)'d(\\d+)".r
  def binString2BooleanVector(string: String): Vector[Boolean] = string.toCharArray.toVector.map{case '0' => false; case '1' => true}
  def apply(string: String): Bit = {
    string match {
      case bin_pat(width, value) if width.nonEmpty => Bit(binString2BooleanVector(value), width.toInt)
      case bin_pat(_, value) => Bit(binString2BooleanVector(value))
      case hex_pat(width, value) if width.nonEmpty => Bit(binString2BooleanVector(BigInt(value, 16).toString(2)), width.toInt)
      case hex_pat(_, value) => Bit(binString2BooleanVector(BigInt(value, 16).toString(2)))
      case dec_pat(_, value) => Bit(binString2BooleanVector(BigInt(value, 10).toString(2)))
      case _ => throw new Exception(s"unexpected $string")
    }
  }
  def apply(v: Int): Bit = {
    Bit(binString2BooleanVector(v.toBinaryString))
  }
  def apply(v: Boolean):Bit = Bit(Vector(v))
  def apply(value_tmp: Vector[Boolean]=Vector(false), width_tmp:Int=0):Bit = {
      val width = if(width_tmp <= 0) value_tmp.length else width_tmp
      val value = if(width >= value_tmp.length) Vector.fill(width-value_tmp.length)(false) ++ value_tmp else value_tmp.takeRight(width)
      new Bit(value, width)
  }
}
case class Idx(src:String, left:Int, right: Int){
  val width:Int = if(left == 0) 0 else left-right+1
  override def toString: String = {
    if(left == 0) src
    else s"${src}[${left}:${right}]"
  }
  def take(width: Int):Idx = {
    if (left-width+1<0) this else Idx(src, left, left-width+1)
  }
  def sub(start:Int, end:Int): Idx = {
    if(left == 0) Idx(src, start, end)
    else if(start-end+1>width) throw new Exception(s"attempt to take sub Idx ${(start, end)} out of range${width}")
    else {
      Idx(src, start+right, end+right)
    }
  }
  def slice(widths:Vector[Int]):Vector[Idx] = {
    if(widths.sum != left + right + 1) throw new Exception(s"attempt to slice ${this.toString} into ${widths.sum}, width mismatch")
    else {
      val right_points = widths.scanLeft(widths.sum)((a, b) => a - b)
      val left_points = (right_points zip widths).map{case (a, b) => a+b-1}
      val segments = (left_points zip right_points).map{case (a, b) => Idx(this.src, a, b)}
      segments
    }
  }
}
object Idx{
  def apply(str:String):Idx = {
    import WideCell.idx_pat
    val idx_pat(v, left, right) = str
    val left_adj: Int = if(left.isEmpty) 0 else left.toInt
    val right_adj: Int = if(right.isEmpty) left_adj else right.toInt
    new Idx(v, left_adj, right_adj)
  }
  def merge(idxes: Vector[Idx]) = idxes.foldLeft(Vector[Idx]())(append)
  def append(cur_vec:Vector[Idx], cur_val:Idx): Vector[Idx] ={
      if(cur_vec.isEmpty) Vector(cur_val)
      else if(cur_vec.last.src != cur_val.src) cur_vec :+ cur_val
      else if(cur_vec.last.right-1 == cur_val.left) cur_vec.init :+ Idx(cur_val.src, cur_vec.last.left, cur_val.right)
      else cur_vec :+ cur_val
    }
}

