package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeObjectParser, December}

import scala.util.Try

case class Register(a: Long, b: Long, c: Long) {
  def aZero: Boolean = a == 0
}

trait DecemberSeventeenParser extends AdventOfCodeObjectParser[(Register, List[Int])] {

  //Register A: 729
  private def parswRegister(s: String): Long = s match {case s"Register $_: $reg" => reg.toLong }
  private def parseProgram(s: String): List[Int] = {
    val rawProgram = s match {case s"Program: $program" => program }
    Try {
      rawProgram.split(',').toList.map{ i => i.toInt }
    }.toOption.getOrElse(List.empty)
  }


  override def linesToObject(lines: List[String]): (Register, List[Int]) = {

    Try {
      val registerA = parswRegister(lines(0))
      val registerB = parswRegister(lines(1))
      val registerC = parswRegister(lines(2))
      val program = parseProgram(lines(4))
      (Register(registerA, registerB, registerC), program)
    }.toOption.getOrElse((Register(0L, 0L, 0L), List.empty))
  }
}


trait DecemberSeventeen extends December[String, (Register, List[Int]), Long] with DecemberSeventeenParser {

  val (register, program) = rawInput

  override def day: Int = 17

  override def solver: Solver[Long, String] = new Solver[Long, String] {
    override def solution(list: List[Long]): String = list.mkString(",")
  }

  def runProgram(registers: Register, program: List[Int]): List[Long] = {

      def loop(register: Register, pointer: Int = 0, acc: List[Long] = List.empty, commands: List[Int] = List.empty): List[Long] = {
          instructionAndOperator(pointer, program) match {
            case None => acc.reverse
            case Some((instruction, literalOperand)) =>
              val nextCommands = instruction :: commands
              lazy val comboOperand = getComboOperand(literalOperand, register)
              instruction match {
                case 0 => loop(register.copy(a = register.a >> comboOperand), pointer + 2, acc, nextCommands)
                case 1 => loop(register.copy(b = register.b ^ literalOperand), pointer + 2, acc, nextCommands)
                case 2 => loop(register.copy(b = comboOperand % 8), pointer + 2, acc, nextCommands)
                case 3 =>
                  val nextPointer = if(register.aZero) pointer + 2 else literalOperand
                  loop(register, nextPointer, acc, nextCommands )
                case 4 => loop(register.copy(b = register.b ^ register.c), pointer + 2, acc, nextCommands )
                case 5 => loop(register, pointer + 2, comboOperand % 8 :: acc, nextCommands)
                case 6 => loop(register.copy(b = register.a >> comboOperand), pointer + 2, acc, nextCommands)
                case 7 => loop(register.copy(c = register.a >> comboOperand), pointer + 2, acc, nextCommands)
                case _ => throw new IllegalStateException(s"Illegal instruction: $instruction")
              }
          }
      }
      loop(registers)
  }

  private def getComboOperand(literalOperand: Int, register: Register): Long = literalOperand match {
    case 0 | 1 | 2 | 3 => literalOperand
    case 4 => register.a
    case 5 => register.b
    case 6 => register.c
    case _ => throw new IllegalStateException(s"Illegal operand: ${literalOperand}")
  }

  private def instructionAndOperator(instructionPointer: Int, program: List[Int]): Option[(Int, Int)] =
    (for {
      instruction <- Try(program(instructionPointer))
      rasOperand <- Try(program(instructionPointer + 1))
    } yield (instruction, rasOperand)).toOption

  override def rawSolution: List[Long] = runProgram(register, program)
}

object DecemberSeventeenPartOneTest extends DecemberSeventeen with PuzzleTest
object DecemberSeventeenPartOneSolution extends DecemberSeventeen with PuzzleSolution