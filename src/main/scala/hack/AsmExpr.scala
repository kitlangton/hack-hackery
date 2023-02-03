package hack
import zio.Chunk
import zio.parser._

import scala.annotation.tailrec

sealed trait AsmExpr {}

object AsmExpr {
  final case class LoadA(value: Value)                                       extends AsmExpr
  final case class Compute(compute: ComputeExpr, destination: Set[Register]) extends AsmExpr
  final case class Jump(compute: ComputeExpr, jump: JumpExpr)                extends AsmExpr
  final case class Label(name: String)                                       extends AsmExpr

  val builtInSymbols: Map[String, Int] =
    (0 to 15).map(i => s"R$i" -> i).toMap

  object Parsers {
    private val stringUntilWhitespace: Syntax[String, Char, Char, String] =
      Syntax.charNotIn(" \t\n=;").repeat.string

    val loadA =
      Syntax.char('@') ~> stringUntilWhitespace
        .transform[LoadA](
          string =>
            string.toIntOption.orElse(builtInSymbols.get(string)) match {
              case Some(int) => LoadA(Value.Literal(int))
              case None      => LoadA(Value.Variable(string))
            },
          {
            case LoadA(Value.Literal(int))  => builtInSymbols.find(_._2 == int).map(_._1).getOrElse(int.toString)
            case LoadA(Value.Variable(str)) => str
          }
        )

    // D=A
    // D=D+A
    // D=D+M
    val register = Syntax
      .charIn("ADM")
      .transform[Register](
        {
          case 'A' => Register.A
          case 'D' => Register.D
          case 'M' => Register.M
        },
        {
          case Register.A => 'A'
          case Register.D => 'D'
          case Register.M => 'M'
        }
      )

    val registerSet =
      register.repeat.transform[Set[Register]](_.toSet, Chunk.fromIterable)

    val computeExpr =
      stringUntilWhitespace.transform[ComputeExpr](
        {
          case "A"   => ComputeExpr.A
          case "D"   => ComputeExpr.D
          case "M"   => ComputeExpr.M
          case "D+A" => ComputeExpr.DPlusA
          case "D-A" => ComputeExpr.DMinusA
          case "D-M" => ComputeExpr.DMinusM
          case "D+M" => ComputeExpr.DPlusM
          case "0"   => ComputeExpr.Zero
          case "1"   => ComputeExpr.One
        },
        {
          case ComputeExpr.A       => "A"
          case ComputeExpr.D       => "D"
          case ComputeExpr.DPlusA  => "D+A"
          case ComputeExpr.DMinusA => "D-A"
          case ComputeExpr.DMinusM => "D-M"
          case ComputeExpr.DPlusM  => "D+M"
          case ComputeExpr.M       => "M"
          case ComputeExpr.Zero    => "0"
          case ComputeExpr.One     => "1"
        }
      )

    val compute =
      (registerSet ~ Syntax.char('=') ~ computeExpr).transform[Compute](
        { case (destination, compute) => Compute(compute, destination) },
        { case Compute(compute, destination) => (destination, compute) }
      )

    val jumpExpr =
      stringUntilWhitespace.transform[JumpExpr](
        {
          case "JGT" => JumpExpr.JGT
          case "JEQ" => JumpExpr.JEQ
          case "JGE" => JumpExpr.JGE
          case "JLT" => JumpExpr.JLT
          case "JNE" => JumpExpr.JNE
          case "JLE" => JumpExpr.JLE
          case "JMP" => JumpExpr.JMP
        },
        {
          case JumpExpr.JGT => "JGT"
          case JumpExpr.JEQ => "JEQ"
          case JumpExpr.JGE => "JGE"
          case JumpExpr.JLT => "JLT"
          case JumpExpr.JNE => "JNE"
          case JumpExpr.JLE => "JLE"
          case JumpExpr.JMP => "JMP"
        }
      )

    val jump =
      (computeExpr ~ Syntax.char(';') ~ jumpExpr).transform[Jump](
        { case (compute, jump) => Jump(compute, jump) },
        { case Jump(compute, jump) => (compute, jump) }
      )

    val label =
      Syntax
        .charNotIn(")")
        .repeat
        .string
        .between(Syntax.char('('), Syntax.char(')'))
        .transform[Label](Label(_), _.name)

    val asmExpr =
      (label.widen[AsmExpr] | loadA.widen[AsmExpr] | compute.widen[AsmExpr] | jump.widen[AsmExpr])
        .surroundedBy(Syntax.charIn(' ', '\t').repeat0.unit(Chunk.empty))

    val program =
      asmExpr.repeatWithSep(Syntax.whitespace.repeat0.unit(Chunk.empty))
  }

}

sealed trait ComputeExpr extends Product with Serializable

object ComputeExpr {
  case object A       extends ComputeExpr
  case object D       extends ComputeExpr
  case object M       extends ComputeExpr
  case object DMinusA extends ComputeExpr
  case object DPlusA  extends ComputeExpr
  case object DMinusM extends ComputeExpr
  case object DPlusM  extends ComputeExpr
  case object Zero    extends ComputeExpr
  case object One     extends ComputeExpr
}

sealed trait JumpExpr extends Product with Serializable

object JumpExpr {
  case object JGT extends JumpExpr
  case object JEQ extends JumpExpr
  case object JGE extends JumpExpr
  case object JLT extends JumpExpr
  case object JNE extends JumpExpr
  case object JLE extends JumpExpr
  case object JMP extends JumpExpr
}

sealed trait Value extends Product with Serializable

object Value {
  final case class Literal(int: Int)      extends Value
  final case class Variable(name: String) extends Value
}

sealed trait Register extends Product with Serializable

object Register {
  case object A extends Register // Address
  case object D extends Register // Data
  case object M extends Register // Memory (RAM[A])
}

object ParserExamples extends App {
  // test labels
//  testParser(List("(hello)", "(world)"), AsmExpr.Parsers.label)
  testParser(List("D=A", "DM=D+A", "D=D+M", "1;JGT", "(hello)"), AsmExpr.Parsers.asmExpr)

  private def testParser[A](inputs: List[String], syntax: Syntax[String, Char, Char, A]): Unit = {
    val asts    = inputs.map(syntax.parseString(_)).collect { case Right(value) => value }
    val printed = asts.map(syntax.printString(_)).collect { case Right(value) => value }
    val zipped = inputs.zip(asts).zip(printed).map { case ((input, ast), printed) =>
      s"$input -> $ast -> $printed"
    }
    println(
      renderTable(
        List("Input", "AST", "Printed") :: List("-----", "---", "-------") :: zipped.map(_.split(" -> ").toList)
      ) + "\n"
    )
  }

  def renderTable[A](rows: List[List[A]]): String = {
    val columns      = rows.transpose
    val columnWidths = columns.map(_.map(_.toString.length).max)
    val rowsWithPaddedColumns = rows.map { row =>
      row.zip(columnWidths).map { case (value, width) =>
        " " + value.toString.padTo(width + 1, ' ')
      }
    }
    rowsWithPaddedColumns.map(_.mkString("|")).mkString("\n")
  }
}

object RealParser extends App {

  val input =
    """
      |   @R0
      |   D=M
      |   @R1
      |   D=D-M
      |   @OUTPUT_FIRST
      |   D;JGT
      |   @R1
      |   D=M
      |   @OUTPUT_D
      |   0;JMP
      |(OUTPUT_FIRST)
      |   @R0
      |   D=M
      |(OUTPUT_D)
      |   @R2
      |   M=D
      |(INFINITE_LOOP)
      |   @INFINITE_LOOP
      |   0;JMP
      |""".stripMargin.trim

  val ast = AsmExpr.Parsers.program.parseString(input).toOption.get
  println(ast.mkString("\n"))
  val symbolTable = Assembler.makeSymbolTable(ast.toList)
  println(symbolTable)

  val emulator =
    CPUEmulator
      .make(ast.toList)
      .setRam("R0", 55)
      .setRam("R1", 33)

  @tailrec
  def loop(emulator: CPUEmulator): Unit = {
    println(emulator.render)
    val nextEmulator = emulator.step
    if (nextEmulator.isHalted) {
      println("HALTED")
    } else {
      loop(nextEmulator)
    }
  }

  loop(emulator)
}

object Assembler {
  // symbol table
  def makeSymbolTable(asm: List[AsmExpr]): Map[String, Int] = {
    @tailrec
    def loop(
        asm: List[AsmExpr],
        instructionCount: Int,
        variables: List[String],
        acc: Map[String, Int]
    ): Map[String, Int] =
      asm match {
        case Nil =>
          val variableTable =
            variables.filterNot(acc.contains).reverse.distinct.zipWithIndex.map { case (name, index) =>
              name -> (index + 16)
            }
          acc ++ variableTable
        case AsmExpr.LoadA(Value.Variable(name)) :: tail =>
          loop(tail, instructionCount + 1, name :: variables, acc)
        case AsmExpr.Label(name) :: tail =>
          loop(tail, instructionCount, variables, acc + (name -> instructionCount))
        case _ :: tail =>
          loop(tail, instructionCount + 1, variables, acc)
      }
    loop(asm, 0, List.empty, Map.empty)
  }

}

final case class CPUEmulator(
    program: Vector[AsmExpr],
    symbolTable: Map[String, Int],
    aRegister: Int = 0,
    dRegister: Int = 0,
    programCounter: Int = 0,
    ram: Vector[Int] = Vector.fill(32)(0),
    isHalted: Boolean = false
) {

  def render: String = {
    val ram0 =
      Vector((aRegister, "A"), (dRegister, "D")) ++ ram.zipWithIndex.map { case (value, index) =>
        value -> index.toString
      }

    val ramLines0 = ram0
      .map { case (value, index) =>
        val wider = value.toString.length max index.toString.length + 1
        val color = if (value != 0) scala.Console.BLUE else scala.Console.WHITE
        List( //
          index.toString.padTo(wider, ' '),
          color + value.toString.padTo(wider, ' ') + scala.Console.RESET
        )
      }
    val ramLines = ramLines0.transpose
      .map(_.mkString(" "))
      .mkString("\n")

    val annotatedProgram = program.zipWithIndex
      .map { case (expr, index) =>
        val printedExpr = AsmExpr.Parsers.asmExpr.printString(expr).toOption.get
        val color       = if (index == programCounter) scala.Console.GREEN else scala.Console.WHITE
        color + printedExpr + scala.Console.RESET
      }
      .mkString("\n")

    val symbolTableColored = symbolTable.toList
      .sortBy(_._2)
      .map { case (name, index) =>
        def white(string: String) =
          (if (index == aRegister) scala.Console.BLUE else "") + string + scala.Console.RESET
        white(name + "=") + index + scala.Console.RESET
      }
      .mkString(" ")
    s"""
       |$ramLines
       |$symbolTableColored
       |$annotatedProgram
       |""".stripMargin.trim + "\n"
  }

  def setRam(index: Int, value: Int): CPUEmulator =
    copy(ram = ram.updated(index, value))

  def setRam(symbol: String, value: Int): CPUEmulator = {
    val index = symbolTable
      .get(symbol)
      .orElse(AsmExpr.builtInSymbols.get(symbol))
      .getOrElse(throw new Exception(s"Unknown symbol $symbol"))
    setRam(index, value)

  }

  def compute(computeExpr: ComputeExpr): Int =
    computeExpr match {
      case ComputeExpr.A       => aRegister
      case ComputeExpr.D       => dRegister
      case ComputeExpr.M       => ram(aRegister)
      case ComputeExpr.DMinusA => dRegister - aRegister
      case ComputeExpr.DPlusA  => dRegister + aRegister
      case ComputeExpr.DMinusM => dRegister - ram(aRegister)
      case ComputeExpr.DPlusM  => dRegister + ram(aRegister)
      case ComputeExpr.Zero    => 0
      case ComputeExpr.One     => 1
    }

  def step: CPUEmulator = {
    val instruction = program(programCounter)
    instruction match {
      case AsmExpr.LoadA(value) =>
        val a = value match {
          case Value.Literal(int) =>
            int
          case Value.Variable(name) =>
            symbolTable.getOrElse(name, throw new IllegalArgumentException(s"Unknown variable $name"))
        }
        copy(aRegister = a, programCounter = programCounter + 1)

      case AsmExpr.Compute(computeExpr, destination) =>
        val value = compute(computeExpr)
        val nextA = if (destination.contains(Register.A)) value else aRegister
        val nextD = if (destination.contains(Register.D)) value else dRegister
        val nextM = if (destination.contains(Register.M)) ram.updated(aRegister, value) else ram
        copy(aRegister = nextA, dRegister = nextD, ram = nextM, programCounter = programCounter + 1)

      case AsmExpr.Jump(computeExpr, jumpExpr) =>
        val value = compute(computeExpr)
        val nextProgramCounter = jumpExpr match {
          case JumpExpr.JGT if value > 0  => aRegister
          case JumpExpr.JEQ if value == 0 => aRegister
          case JumpExpr.JLT if value < 0  => aRegister
          case JumpExpr.JGE if value >= 0 => aRegister
          case JumpExpr.JLE if value <= 0 => aRegister
          case JumpExpr.JNE if value != 0 => aRegister
          case JumpExpr.JMP               => aRegister
          case _                          => programCounter + 1
        }
        copy(
          programCounter = nextProgramCounter,
          isHalted = nextProgramCounter == programCounter - 1
        )

      case AsmExpr.Label(_) =>
        copy(programCounter = programCounter + 1)
    }
  }
}

object CPUEmulator {
  def make(asm: List[AsmExpr]): CPUEmulator = {
    val symbolTable = Assembler.makeSymbolTable(asm)
    val program     = asm.toVector.filterNot(_.isInstanceOf[AsmExpr.Label])
    CPUEmulator(program, symbolTable)
  }
}
