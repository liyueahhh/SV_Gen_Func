package utils

import base.Base.read_until
import base.Idx
import org.apache.poi.hssf.usermodel.HSSFRow

import scala.util.matching.Regex

//case class Shape(){
//  override def toString: String = {
//    dimension match {
//      case List(1) => ""
//      case List(n) => ""
//    }
//  }
//} //dimension List(1) for width == 1, List(n) for width == n, List(2, 3) for bit [1:0][2:0] var;

case class SVar(name:String, dimension:List[Int], keyword:String, direction: String, origin:Any){
  if(dimension.isEmpty) throw new Exception(s"Unexpected empty dimension")
  override def toString: String = {
    val before_name = if(dimension.last < 0) throw new Exception(s"Unexpected dimension ${dimension}") else {
      val without_io = if (dimension.last == 1) keyword else s"$dimension $keyword [${dimension.last-1}:0]"
      if(direction != "") s"$direction $without_io" else without_io
    }
    val norm = before_name + name + s""" ${dimension.init.map(i=>s"[$i-1]")}""" + ";"
    if(keyword == "comb") origin.toString else norm
  }
}
object SVar{
  val bit_pat: Regex = "\\d*'(?:h\\h+|d\\d+|b\\b+)".r
  val str_pat: Regex = "\"[^\"]*\"".r
  val int_pat: Regex = "(\\d+)(?:\\.0+)?".r
  val idx_pat: Regex = "(\\w+)(?:\\[(\\d+):?(\\d+)?])?".r
  val seg_pat: Regex = "(\\d+)x".r
  val ext_pat: Regex = "(.+#\\d.*)%(?:\\((.+)\\))?".r
  def apply(src:String):SVar = {
    val tmp:SVar = src match {
      case int_pat(v) => new SVar("", List(1), "int", direction = "", origin = v.toInt)
      case bit_pat() => val t = Bit(src); new SVar("", List(t.width), keyword = "bit", direction = "", origin = t)
      case str_pat() => new SVar("", List(1), keyword = "string", direction = "", origin = src)
      case idx_pat() => val t = Idx(src); new SVar(name=t.src, List(t.width), keyword = "bit", direction = "", origin = t)
      case seg_pat(w) => new SVar("", List(w.toInt), keyword = "seg", direction = "", origin = src)
      case ext_pat(_*) => new SVar(name = "", List(1), keyword = "ext", direction = "", origin = src)
      case _ => new SVar(name = "", List(1), keyword = "comb", direction = "", origin = src)
    }
    tmp
  }
  def apply(row:HSSFRow):SVar = {
    val cells = read_until(row)(0, "")
    if(cells.head != "input" && cells.head != "output") throw new Exception(s"SVar cannot parse row $cells")
    else {
      val direction = cells.find(c => c == "input" || c == "output").getOrElse("")
      val name_pat = "([a-zA-Z_]+\\w+)".r
      val name = cells.find{case name_pat(name) => true; case _ => false}.getOrElse(throw new Exception("Can find a variable name in the row"))
      val keyword = cells.find(c => c == "bit" || c == "string" || c == "bit").getOrElse("")
      val dim = if(keyword == "bit") cells.collect{case seg_pat(w) => w.toInt}.toList else List(1)
      val dimension = if(dim.isEmpty) List(1) else dim
      new SVar(name, dimension, keyword, direction, cells.mkString(" "))
    }
  }
  def extPatTrans(src:SVar):Vector[SVar] = {
    if(src.keyword != "ext") Vector(src)
    else {
      val org = src.origin.toString
      val ext_pat(singleton, copy_str) = org
      val copy_pat = "(\\d+)(?::(\\d+))?".r
      println(copy_str)
      val copy_arr: Vector[(Int, Int, Int)] = copy_str.split(",").toVector.map(_.replace(" ", "")).map{
        case copy_pat(left, right) if right == null => (left.toInt, left.toInt, 1)
        case copy_pat(left, right) if left.toInt <= right.toInt => (left.toInt, right.toInt, 1)
        case copy_pat(left, right)  => (left.toInt, right.toInt, -1)
      }
      val copy_num:Int = copy_arr.map{ i => (i._2 - i._1).abs + 1}.max
      val copy_arr_expand: Vector[Vector[Int]] = copy_arr.map{case(i, i1, i2) => if(i == i1) Vector.fill(copy_num)(i) else (i to i1 by i2).toVector}
      val copy_arr_expand_T = copy_arr_expand.transpose
      val target = copy_arr_expand_T.map{vars => vars.zipWithIndex.foldLeft(singleton){(src:String, ele:(Int, Int)) => {
        src.replace("#"+ele._2.toString, ele._1.toString)
      }}}
      target.map{i => new SVar(name = "", List(1), keyword = "comb", direction = "", origin = i)}
    }
  }
}

case class SvFunction( vars:List[Nothing], exprs:List[String])

//object SvFunction{
//  def lineDecoder(row: HSSFRow): String = {
//    val row_head: String = read_cell(row)(0)
//    row_head match {
//      case v if v == "Function" || v == "function" => "func"
//      case v if v == "input" || v == "output" || v == "wire" || v == "bit" => "var"
//      cse
//    }
//  }
//}