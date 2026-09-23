package com.evolutiongaming.bootcamp.basics

import scala.io.Source

object ControlStructuresHomework2 {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double])                extends Command
    final case class Average(numbers: List[Double])            extends Command
    final case class Min(numbers: List[Double])                extends Command
    final case class Max(numbers: List[Double])                extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  final case class ProcessedResult(value: String) extends Result

  // Helper to print integers clean (e.g., 4 instead of 4.0) while retaining double precision when needed
  private def formatDouble(d: Double): String =
    if (d == d.toLong) d.toLong.toString else d.toString

  private def parseDoubles(strs: List[String]): Either[ErrorMessage, List[Double]] =
    strs.foldRight[Either[ErrorMessage, List[Double]]](Right(Nil)) { (str, acc) =>
      for {
        tail <- acc
        d    <- str.toDoubleOption.toRight(ErrorMessage(s"Invalid number: '$str'"))
      } yield d :: tail
    }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).

    x.trim.split("\\s+").toList.filter(_.nonEmpty) match {
      case "divide" :: dividendStr :: divisorStr :: Nil =>
        for {
          dividend <- dividendStr.toDoubleOption.toRight(ErrorMessage("Invalid dividend"))
          divisor  <- divisorStr.toDoubleOption.toRight(ErrorMessage("Invalid divisor"))
        } yield Command.Divide(dividend, divisor)

      case "divide" :: _ =>
        Left(ErrorMessage("Command 'divide' requires exactly 2 arguments"))

      case "sum" :: rest if rest.nonEmpty =>
        parseDoubles(rest).map(Command.Sum)

      case "sum" :: _ =>
        Left(ErrorMessage("Command 'sum' requires at least 1 number"))

      case "average" :: rest if rest.nonEmpty =>
        parseDoubles(rest).map(Command.Average)

      case "average" :: _ =>
        Left(ErrorMessage("Command 'average' requires at least 1 number"))

      case "min" :: rest if rest.nonEmpty =>
        parseDoubles(rest).map(Command.Min)

      case "min" :: _ =>
        Left(ErrorMessage("Command 'min' requires at least 1 number"))

      case "max" :: rest if rest.nonEmpty =>
        parseDoubles(rest).map(Command.Max)

      case "max" :: _ =>
        Left(ErrorMessage("Command 'max' requires at least 1 number"))

      case cmd :: _ =>
        Left(ErrorMessage(s"Unknown command: '$cmd'"))

      case Nil =>
        Left(ErrorMessage("Empty input"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = x match {
    case Command.Divide(dividend, divisor) =>
      if (divisor == 0) {
        Left(ErrorMessage("Division by zero"))
      } else {
        val res = dividend / divisor
        Right(ProcessedResult(s"${formatDouble(dividend)} divided by ${formatDouble(divisor)} is ${formatDouble(res)}"))
      }

    case Command.Sum(numbers) =>
      val res = numbers.sum
      Right(ProcessedResult(s"the sum of ${numbers.map(formatDouble).mkString(" ")} is ${formatDouble(res)}"))

    case Command.Average(numbers) =>
      if (numbers.isEmpty) {
        Left(ErrorMessage("Cannot calculate average of empty list"))
      } else {
        val res = numbers.sum / numbers.size
        Right(ProcessedResult(s"the average of ${numbers.map(formatDouble).mkString(" ")} is ${formatDouble(res)}"))
      }

    case Command.Min(numbers) =>
      if (numbers.isEmpty) {
        Left(ErrorMessage("Cannot calculate minimum of empty list"))
      } else {
        val res = numbers.min
        Right(ProcessedResult(s"the minimum of ${numbers.map(formatDouble).mkString(" ")} is ${formatDouble(res)}"))
      }

    case Command.Max(numbers) =>
      if (numbers.isEmpty) {
        Left(ErrorMessage("Cannot calculate maximum of empty list"))
      } else {
        val res = numbers.max
        Right(ProcessedResult(s"the maximum of ${numbers.map(formatDouble).mkString(" ")} is ${formatDouble(res)}"))
      }
  }

  def renderResult(x: Result): String = x match {
    case ProcessedResult(value) => value
  }

  def process(x: String): String = {
    import cats.implicits._

    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    (for {
      cmd    <- parseCommand(x)
      result <- calculate(cmd)
    } yield renderResult(result))
      .leftMap(err => s"Error: ${err.value}")
      .merge
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
