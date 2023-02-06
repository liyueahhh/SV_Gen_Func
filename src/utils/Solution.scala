////import SV.{Var, sv_var}
//import base.Base._
////import org.apache.poi.xssf.usermodel.{XSSFRow, XSSFWorkbook}
//
//import java.io.FileInputStream
//import base.Base._
//import base.WideCell.extractLine
//
//import scala.annotation.tailrec
//import scala.util.matching.Regex
//
//object Solution extends App{
//  trait sv_expr
//  def idx2bitItem(arr:Array[String], name:String):Array[String] = {
//    arr.map {
//      case i@int_pat() => s"$name[$i]"
//      case i => i
//    }
//  }
//
//
//
//  def bit_merge(arr:Array[String]):Array[String] = {
//    val bit_item_pattern = "([a-zA-z_]+)\\[([0-9]+)\\]([a-zA-z_]+)?".r
//    val const_pattern = "([0-9]+)?'?b([0-1]+)".r
//    val all_zero_pattern = "0+".r
//    def const_merge(arr:Array[String]):Array[String] ={
//      def go(idx:Int,  bit_width:Int, bit_tail:String, cur_arr:List[String]):List[String] = {
//        lazy val cur_member = if(bit_width == 0) "" else bit_tail match {
//                case all_zero_pattern() => s"$bit_width'b0"
//                case _ => s"$bit_width'b$bit_tail"
//              }
//        if(idx == arr.length) (cur_member::cur_arr).reverse.filter(_ != "")
//        else arr(idx) match {
//          case const_pattern(item_width, item_tail) =>
//            val item_width_int = if (item_width == null) item_tail.length else item_width.toInt
//            if(item_width_int != item_tail.length) println("*E const bit size mismatch")
////            println(s"${arr(idx)}, item_width:$item_width, item_tail:$item_tail, bit_width:$bit_width, bit_tail:$bit_tail, item_width_int:$item_width_int")
//            go(idx+1, bit_width+item_width_int, bit_tail + item_tail, cur_arr)
//          case _ =>
//            go(idx+1, 0, "", arr(idx)::cur_arr)
//        }
//      }
//      go(0,0,"",List()).toArray
//    }
//    def idx_merge(arr:Array[String]):Array[String] = {
//      @tailrec
//      def go(idx:Int, left: String, right: String, cur_range: (Int, Int), cur_arr: List[String]): List[String] = {
//        //      println(idx, arr(idx), left, right, cur_range, cur_arr);
//        lazy val cur_member = if (cur_range._2 < 0) "" else
//          s"${if (left == null) "" else left}[${cur_range._1}:${cur_range._2}]${if (right == null) "" else right}"
//        if (idx == arr.length) (cur_member :: cur_arr).reverse.filter(_ != "")
//        else arr(idx) match {
//          case bit_item_pattern(cur_left, num, cur_right) =>
////            println(cur_left, num, cur_right)
//            val num2int = num.toInt
//            if ((num2int == cur_range._2 - 1) && (cur_left == left) && (cur_right == right)) go(idx + 1, left, right, (cur_range._1, num2int), cur_arr)
//            else go(idx + 1, cur_left, cur_right, (num2int, num2int), cur_member :: cur_arr)
//          case _ => go(idx + 1, "", "", (-1, -1), arr(idx) :: cur_member :: cur_arr)
//        }
//      }
//      go(0, null, null, (-1, -1), List()).toArray
//    }
//    idx_merge(const_merge(arr))
//  }
//  val pb_pat = "(\\+|-)([0-9]+)\\*([0-9]+)".r
//  def pb_cal(arr:Array[String], odd:Boolean = true):Array[String] = {
//     arr.zipWithIndex.map {
//       case (pb_pat(direction, offset, length), idx) => {
//         val start = if(direction == "+") idx + offset.toInt else idx - offset.toInt - length.toInt + 1
//         val end = start + length.toInt
//         val sub_arr = bit_merge(arr.slice(start, end))
//         (if (odd) "~" else "") + s"^{${sub_arr.mkString(",")}}"
//       }
//       case i => i._1
//     }
//
//  }
//
////  class sv_expr_single(owner:String, source:sv_var, XSSFRow: XSSFRow, start_col:Int = 0){
////    lazy val position = s" @ row_num:${XSSFRow.getRowNum} sheet:${XSSFRow.getSheet.getSheetName}"
////    private val arr = read_row(XSSFRow, start_col, source.width)
////    def get_body(arr:List[String]):String = {
////      val tmp0 = idx2bitItem(arr.toArray, source.name)
////      val tmp1 = pb_cal(tmp0)
////      val tmp2 = bit_merge(tmp1)
////      tmp2.mkString(",")
////    }
////    val wrap:String = get_body(arr)
////
////    override def toString: String = s""
////  }
//  class tmp_class(val aa:Int, val bb:Int){
////    val a: Int = aa
////    val b: Int = bb
//    def this(ss:String) = {
//      this(ss(0).toInt, ss(1).toInt)
//    }
//  }
//
//  val file_path = raw"D:\Code\Scala\six.xlsx"
//  val xss = new XSSFWorkbook(new FileInputStream(file_path))
//  val sheet = xss.getSheetAt(0)
////  val cell = sheet.getMergedRegion(0)
//  val cells = extractLine(sheet.getRow(22), 0)
//  cells.foreach(println)
////  val row = sheet.getRow(65)
////  val tmp_val = Var(row)
////  println(tmp_val.wrap_without_io)
////  println(tmp_val.wrap)
////  val tmp = new tmp_class("ss")
////  println(tmp.aa, tmp.bb)
//}
