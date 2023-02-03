package asm

import asm.Asm.{Computation, JumpExpr, Register, SymbolOrValue}

// R0 R1 ... R15 x  y
// 0  1  ... 15  16 17

sealed trait Asm {
  //
}

object Asm {
  sealed trait SymbolOrValue extends Product with Serializable

  object SymbolOrValue {
    final case class Symbol(string: String) extends SymbolOrValue
    final case class Value(int: Int)        extends SymbolOrValue
  }
  // 1. LoadA
  // @x    <- symbol
  // @125  <- value
  final case class LoadA(symbolOrValue: SymbolOrValue) extends Asm

  final case class Label(name: String) extends Asm
  // 2. Compute
  // A=M
  // M=D+A
  // destinations = computation
  final case class Compute(registers: Set[Register], computation: Computation) extends Asm

  final case class Jump(computation: Computation, jumpExpr: JumpExpr) extends Asm

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

  sealed trait Computation extends Product with Serializable
  object Computation {
    case object Zero    extends Computation
    case object One     extends Computation
    case object D       extends Computation
    case object M       extends Computation
    case object A       extends Computation
    case object DPlusA  extends Computation
    case object DPlusM  extends Computation
    case object DMinusA extends Computation
    case object DMinusM extends Computation
  }

  sealed trait Register extends Product with Serializable

  object Register {
    case object A extends Register
    case object D extends Register
    case object M extends Register // RAM[A]
  }

  // 3. Jump
  // @destination
  // M=D+A;JMP
  // computation ; jumpExpr
}

final case class CpuEmulator(
    program: Vector[Asm],
    aRegister: Int,
    dRegister: Int,
    programCounter: Int,
    ram: Vector[Int]
) {

  def processComputation(computation: Asm.Computation): Int =
    computation match {
      case Computation.Zero    => 0
      case Computation.One     => 1
      case Computation.D       => dRegister
      case Computation.A       => aRegister
      case Computation.M       => ram(aRegister)
      case Computation.DPlusA  => dRegister + aRegister
      case Computation.DPlusM  => dRegister + ram(aRegister)
      case Computation.DMinusA => dRegister - aRegister
      case Computation.DMinusM => dRegister - ram(aRegister)
    }

  def step: CpuEmulator = {
    val inst = program(programCounter)

    inst match {
      case Asm.LoadA(SymbolOrValue.Value(value)) =>
        copy(aRegister = value, programCounter = programCounter + 1)

      case Asm.Compute(registers, computation) =>
        val compResult = processComputation(computation)
        val nextA      = if (registers.contains(Register.A)) compResult else aRegister
        val nextD      = if (registers.contains(Register.D)) compResult else dRegister
        val nextRam    = if (registers.contains(Register.M)) ram.updated(aRegister, compResult) else ram
        copy(aRegister = nextA, dRegister = nextD, programCounter = programCounter + 1, ram = nextRam)

      case Asm.Jump(computation, jumpExpr) =>
        val compResult = processComputation(computation)
        val nextPc = jumpExpr match {
          case JumpExpr.JGT => if (compResult > 0) aRegister else programCounter + 1
          case JumpExpr.JEQ => if (compResult == 0) aRegister else programCounter + 1
          case JumpExpr.JGE => if (compResult >= 0) aRegister else programCounter + 1
          case JumpExpr.JLT => if (compResult < 0) aRegister else programCounter + 1
          case JumpExpr.JNE => if (compResult != 0) aRegister else programCounter + 1
          case JumpExpr.JLE => if (compResult <= 0) aRegister else programCounter + 1
          case JumpExpr.JMP => aRegister
        }
        copy(programCounter = nextPc)

      case Asm.LoadA(SymbolOrValue.Symbol(name)) => throw new Exception(s"Symbol BAD; ${name}")
      case Asm.Label(_)                          => throw new Exception(s"LABELS SHOULD HAVE BEEN PROCESSED BY NOW MY DUDE")
    }
  }

}

object CpuEmulatorDemo extends App {

  import Asm._
  val asm =
    Vector(
      LoadA(SymbolOrValue.Value(0)),
      Compute(Set(Register.D), Computation.M),
      LoadA(SymbolOrValue.Value(1)),
      Compute(Set(Register.D), Computation.DMinusM),
      LoadA(SymbolOrValue.Value(10)),
      Jump(Computation.D, JumpExpr.JGT),
      LoadA(SymbolOrValue.Value(1)),
      Compute(Set(Register.D), Computation.M),
      LoadA(SymbolOrValue.Value(12)),
      Jump(Computation.Zero, JumpExpr.JMP),
      LoadA(SymbolOrValue.Value(0)),
      Compute(Set(Register.D), Computation.M),
      LoadA(SymbolOrValue.Value(2)),
      Compute(Set(Register.M), Computation.D),
      LoadA(SymbolOrValue.Value(14)),
      Jump(Computation.Zero, JumpExpr.JMP)
    )

  val emulator =
    CpuEmulator(asm, 0, 0, 0, Vector.fill(24)(0).updated(0, 55).updated(1, 33))

  def loop(emulator: CpuEmulator): Unit = {
    val message =
      s"""
         |  A: ${emulator.aRegister}
         |  D: ${emulator.dRegister}
         |RAM: ${(0 to 23).mkString(" ")}
         |RAM: ${emulator.ram.mkString(" ")}
         | PC: ${emulator.programCounter}
         |""".stripMargin

    println(message)
    io.StdIn.readLine()
    loop(emulator.step)
  }

  loop(emulator)

}
