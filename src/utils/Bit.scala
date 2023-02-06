package utils

import utils.Extend.{error, log}

import scala.language.postfixOps
import scala.util.matching.Regex

case class Bit(value_tmp: Vector[Boolean]=Vector(false), width_tmp:Int=0){
  val width = if(width_tmp <= 0) value_tmp.length else width_tmp
  val value = if(width >= value_tmp.length) Vector.fill(width-value_tmp.length)(false) ++ value_tmp else value_tmp.takeRight(width)
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
  def cat (tar: Bit): Bit = {
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
}


object Extend {
  implicit class Name(s: String) {
    def :=(value: String): Unit = {
      println(s"$s = \"$value\";")
    }
    def :=(value: Any):Unit = {
      println(s"$s = ${value.toString};")
    }
//    def declaration: String = value match {
//    case _: Int => s"int\t\t$name;"
//    case _: String => s"string\t\t$name;"
//    case v: Bit => s"bit [${v.width - 1}:0]\t$name;"
//    case v: Vector[Bit] => s"bit [${v(0).width - 1}:0]\t$name [${v.length - 1}:0]"
//    case v: List[Bit] => s"bit [${v.head.width - 1}:0]\t$name [$$]"
//    case _ => throw new Exception(s"unexpected type ${value.getClass.getSimpleName}")
//  }
  }
  def log(s:String)={}
  def error(s:String) = {
    throw new Exception(s)
  }
}

class Declaration(val name: String, val value: Any, val expression: String, val direction: String=""){
  def == (target: Declaration): Boolean = {
    if(name != target.name) false
    else (value, target.value) match {
      case (a:Bit, b:Bit) => if(a.width != b.width) {log(s"width mismatch ${a.width} != ${b.width}"); true} else true
      case (a,b) if a.getClass.getSimpleName == b.getClass.getSimpleName => true
      case (a,b) => error(s"SV variable ${name} attempt to declare two type:${a.getClass.getSimpleName} and ${b.getClass.getSimpleName}")
    }
  }
}
object Declaration{
  def apply(name:String, value: Any, direction:String=""): Declaration ={
      val declaration = value match {
        case _: Int => s"int\t\t$name"
        case _: String => s"string\t\t$name"
        case v: Bit => s"bit [${v.width - 1}:0]\t$name"
        case v: Vector[Bit] => s"bit [${v(0).width - 1}:0]\t$name [${v.length - 1}:0]"
        case v: List[Bit] => s"bit [${v.head.width - 1}:0]\t$name [$$]"
        case _ => throw new Exception(s"unexpected type ${value.getClass.getSimpleName}")
      }
      new Declaration(name, value, declaration, direction)
  }
}
//case class Branch(conditions:List[String], actions:List[String]) extends {
//  override def toString: String = {
//    if(conditions.isEmpty) actions.head
//    else if(conditions.tail.isEmpty) {
//      val conditions_wrap = (conditions.init.map{i => s"if($i)"} :+ conditions.last)
//    }
//  }
//}
