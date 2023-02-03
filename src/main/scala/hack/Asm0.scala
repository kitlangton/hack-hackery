package hack

import scala.collection.immutable.{AbstractSet, SortedSet}
import zio.parser._

// // Computes R0 = 2 + 3  (R0 refers to RAM[0])
//
// @2
// D=A
// @3
// D=D+A
// @0
// M=D

sealed trait AsmExpr1 extends Product with Serializable { self =>
  import AsmExpr1._

  def toBinary: String =
    self match {
      case AInst(value) =>
        val binary  = value.toBinaryString
        val padding = "0" * (16 - binary.length)
        s"$padding$binary"

      case CInst(cmp, dest, jmp) =>
        val destBinary = List(Register1.A, Register1.D, Register1.M).map { r =>
          if (dest(r)) "1" else "0"
        }.mkString
        val jmpBinary = jmp match {
          case JmpExpr1.No    => "000"
          case JmpExpr1.JGT   => "001"
          case JmpExpr1.JEQ   => "010"
          case JmpExpr1.JGE   => "011"
          case JmpExpr1.JLT   => "100"
          case JmpExpr1.JNE   => "101"
          case JmpExpr1.JLE   => "110"
          case JmpExpr1.JMP$1 => "111"
        }
        val cmpBinary = cmp match {
          case CmpExpr1.A      => "0110000"
          case CmpExpr1.DPlusA => "0000010"
          case CmpExpr1.D      => "0001100"
          case _               => throw new Exception(s"Binary code not implemented ${cmp}")
        }
        s"111$cmpBinary$destBinary$jmpBinary"
    }

}

object AsmExpr1 {

  def parse(input: String): List[AsmExpr1] =
    input.split("\n").flatMap(parseExpr).toList

  private def parseExpr(line: String): Option[AsmExpr1] =
    line.takeWhile(_ != ' ') match {
      case s"@${value}" => Some(AInst(value.toInt))

      case s"${cmp};${jmp}" =>
        val cmpExpr = parseCmp(cmp)
        val jmpExpr = jmp match {
          case "JGT" => JmpExpr1.JGT
          case "JEQ" => JmpExpr1.JEQ
          case "JGE" => JmpExpr1.JGE
          case "JLT" => JmpExpr1.JLT
          case "JNE" => JmpExpr1.JNE
          case "JLE" => JmpExpr1.JLE
          case "JMP" => JmpExpr1.JMP$1
        }
        Some(CInst(cmpExpr, Set.empty, jmpExpr))

      case s"${dest}=${cmp}" =>
        val registers = dest.map {
          case 'A' => Register1.A
          case 'D' => Register1.D
          case 'M' => Register1.M
        }.toSet

        val cmpExpr = parseCmp(cmp)
        Some(CInst(cmpExpr, registers, JmpExpr1.No))

      case _ =>
        None
    }

  private def parseCmp(cmp: String) =
    cmp match {
      case "0"   => CmpExpr1.Zero
      case "1"   => CmpExpr1.One
      case "A"   => CmpExpr1.A
      case "D+A" => CmpExpr1.DPlusA
      case "D"   => CmpExpr1.D
      case "M"   => CmpExpr1.M
      case "D-M" => CmpExpr1.DMinusM
    }

  // Later on this might be a symbol
  final case class AInst(value: Int)                                         extends AsmExpr1
  final case class CInst(cmp: CmpExpr1, dest: Set[Register1], jmp: JmpExpr1) extends AsmExpr1
}

sealed trait CmpExpr1 extends Product with Serializable

object CmpExpr1 {
  case object A       extends CmpExpr1
  case object DPlusA  extends CmpExpr1
  case object D       extends CmpExpr1
  case object M       extends CmpExpr1
  case object DMinusM extends CmpExpr1
  case object Zero    extends CmpExpr1
  case object One     extends CmpExpr1
}

sealed trait JmpExpr1 extends Product with Serializable

object JmpExpr1 {
  case object No    extends JmpExpr1
  case object JGT   extends JmpExpr1
  case object JEQ   extends JmpExpr1
  case object JGE   extends JmpExpr1
  case object JLT   extends JmpExpr1
  case object JNE   extends JmpExpr1
  case object JLE   extends JmpExpr1
  case object JMP$1 extends JmpExpr1
}

sealed trait Register1 extends Product with Serializable

object Register1 {
  case object A extends Register1
  case object D extends Register1
  case object M extends Register1
}
