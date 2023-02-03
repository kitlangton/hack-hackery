package hack

import hack.AsmExpr1.{AInst, CInst, parse}
import zio.test._

object AsmExpr1Spec$ extends ZIOSpecDefault {
  val spec =
    suite("AsmExprSpec")(
      test("AInst") {
        val input =
          """
// Some comment
// Another

@2
D=A // D=2
@3
D=D+A
@0
M=D
          """.trim

        val result = parse(input)
        val expected =
          List(
            AInst(2),
            CInst(CmpExpr1.A, Set(Register1.D), JmpExpr1.No),
            AInst(3),
            CInst(CmpExpr1.DPlusA, Set(Register1.D), JmpExpr1.No),
            AInst(0),
            CInst(CmpExpr1.D, Set(Register1.M), JmpExpr1.No)
          )

        val binary = result.map(_.toBinary).mkString("\n")
        val expectedBinary =
          """
0000000000000010
1110110000010000
0000000000000011
1110000010010000
0000000000000000
1110001100001000
""".trim

        assertTrue(result == expected)
        assertTrue(binary == expectedBinary)
      },
      test("MaxL") {
        val input =
          """
@0
D=M
@1
D=D-M
@10
D;JGT
@1
D=M
@12
0;JMP
@0
D=M
@2
M=D
@14
0;JMP
        """.trim

        val result = parse(input)
        val expected =
          List(
            AInst(0),
            CInst(CmpExpr1.M, Set(Register1.D), JmpExpr1.No),
            AInst(1),
            CInst(CmpExpr1.DMinusM, Set(Register1.D), JmpExpr1.No),
            AInst(10),
            CInst(CmpExpr1.D, Set.empty, JmpExpr1.JGT),
            AInst(1),
            CInst(CmpExpr1.M, Set(Register1.D), JmpExpr1.No),
            AInst(12),
            CInst(CmpExpr1.Zero, Set.empty, JmpExpr1.JMP$1),
            AInst(0),
            CInst(CmpExpr1.M, Set(Register1.D), JmpExpr1.No),
            AInst(2),
            CInst(CmpExpr1.D, Set(Register1.M), JmpExpr1.No),
            AInst(14),
            CInst(CmpExpr1.Zero, Set.empty, JmpExpr1.JMP$1)
          )

        assertTrue(result == expected)
      },
      test("Max") {
        val input =
          """
  @R0
  D=M              // D = first number
  @R1
  D=D-M            // D = first number - second number
  @OUTPUT_FIRST
  D;JGT            // if D>0 (first is greater) goto output_first
  @R1
  D=M              // D = second number
  @OUTPUT_D
  0;JMP            // goto output_d
(OUTPUT_FIRST)
  @R0
  D=M              // D = first number
(OUTPUT_D)
  @R2
  M=D              // M[2] = D (greatest number)
(INFINITE_LOOP)
  @INFINITE_LOOP
  0;JMP            // infinite loop
  """.trim

        val result = parse(input)
        val expected =
          List(
            AInst(0),
            CInst(CmpExpr1.M, Set(Register1.D), JmpExpr1.No),
            AInst(1),
            CInst(CmpExpr1.DMinusM, Set(Register1.D), JmpExpr1.No),
            AInst(10),
            CInst(CmpExpr1.D, Set.empty, JmpExpr1.JGT),
            AInst(1),
            CInst(CmpExpr1.M, Set(Register1.D), JmpExpr1.No),
            AInst(12),
            CInst(CmpExpr1.Zero, Set.empty, JmpExpr1.JMP$1),
            AInst(0),
            CInst(CmpExpr1.M, Set(Register1.D), JmpExpr1.No),
            AInst(2),
            CInst(CmpExpr1.D, Set(Register1.M), JmpExpr1.No),
            AInst(14),
            CInst(CmpExpr1.Zero, Set.empty, JmpExpr1.JMP$1)
          )

        assertTrue(result == expected)
      }
    )
}
