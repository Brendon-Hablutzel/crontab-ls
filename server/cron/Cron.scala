package cron

import scala.collection.mutable.ListBuffer

case class CronError(line: Int, startChar: Int, endChar: Int, message: String)

enum CronTermType(val minVal: Int, val maxVal: Int):
  case Minute extends CronTermType(0, 59)
  case Hour extends CronTermType(0, 23)
  case DayOfMonth extends CronTermType(1, 31)
  case Month extends CronTermType(1, 12)
  case DayOfWeek extends CronTermType(0, 6)

  override def toString: String =
    this match
      case Minute     => "minute"
      case Hour       => "hour"
      case DayOfMonth => "day of month"
      case Month      => "month"
      case DayOfWeek  => "day of week"

object CronTermType:
  val indexToTermType: Vector[CronTermType] = Vector(
    CronTermType.Minute,
    CronTermType.Hour,
    CronTermType.DayOfMonth,
    CronTermType.Month,
    CronTermType.DayOfWeek
  )

enum CronTermValue:
  case Any
  case Single(value: Int)
  case Many(values: List[Int])
  case Range(start: Int, end: Int)
  case Interval(step: Int)

  override def toString: String =
    this match
      case Any           => "any"
      case Single(value) => s"at $value"
      case Many(values)  =>
        s"at any of ${values.mkString(" ")}"
      case Range(start, end) =>
        s"from $start to $end (inclusive)"
      case Interval(step) => s"every $step"

object CronTermValue:
  def parseTermValue(
      termString: String,
      termType: CronTermType
  ): Either[List[CronTermError], CronTermValue] = {
    val (minVal, maxVal) = (termType.minVal, termType.maxVal)

    val AnyRe = "^\\*$".r
    val SingleRe = "^([0-9]+)$".r
    val ManyRe = ".*?,.*".r
    val RangeRe = "^([0-9]+)-([0-9]+)$".r
    val IntervalRe = "^\\*/([0-9]+)$".r

    termString match {
      case AnyRe() =>
        Right(CronTermValue.Any)

      case SingleRe(valueStr) =>
        val value = valueStr.toInt

        if value >= minVal && value <= maxVal then
          Right(CronTermValue.Single(value))
        else
          Left(
            List(
              CronTermError(
                0,
                termString.length,
                s"${termType.toString} term value must be between $minVal and $maxVal"
              )
            )
          )

      case ManyRe() =>
        val values = termString.split(',').map(_.toInt).toList

        val errors = values.zipWithIndex
          .filter { case (value, _) =>
            value < minVal || value > maxVal
          }
          .map { case (value, index) =>
            CronTermError(
              index * 2,
              index * 2 + value.toString.length,
              s"${termType.toString} term values must be between $minVal and $maxVal"
            )
          }

        // TODO: warn on duplicate values

        errors match {
          case Nil    => Right(CronTermValue.Many(values))
          case errors => Left(errors)
        }

      case RangeRe(startStr, endStr) =>
        val start = startStr.toInt
        val end = endStr.toInt
        val startValid = start >= minVal && start <= maxVal
        val endValid = end >= minVal && end <= maxVal
        val separatorIndex = termString.indexOf('-')

        // TODO: warn on same start and end value, suggest using single value

        val startError =
          if startValid then None
          else
            Some(
              CronTermError(
                0,
                separatorIndex,
                s"${termType.toString} term start range value must be between $minVal and $maxVal"
              )
            )

        val endError =
          if endValid then None
          else
            Some(
              CronTermError(
                separatorIndex + 1,
                termString.length,
                s"${termType.toString} term end range value must be between $minVal and $maxVal"
              )
            )

        List(startError, endError).flatten match {
          case Nil    => Right(CronTermValue.Range(start, end))
          case errors => Left(errors)
        }

      case IntervalRe(stepStr) =>
        val step = stepStr.toInt
        // TODO: warn if */1, suggest using *
        if step == 0 then
          Left(
            List(
              CronTermError(
                2,
                3,
                s"${termType.toString} term step interval cannot be 0"
              )
            )
          )
        else Right(CronTermValue.Interval(step))

      case termString =>
        Left(
          List(
            CronTermError(
              0,
              termString.length,
              s"bad ${termType.toString} term: '$termString'"
            )
          )
        )
    }
  }

/** @param startChar
  *   inclusive starting character for the error, within this token
  * @param endChar
  *   exclusive ending character for the error, within this token
  * @param message
  *   the error message
  */
case class CronTermError(startChar: Int, endChar: Int, message: String)

enum CrontabToken:
  case Comment(lineIndex: Int, content: String)
  case Term(
      lineIndex: Int,
      charIndex: Int,
      termType: CronTermType,
      termValue: Either[List[CronTermError], CronTermValue],
      content: String
  )
  case Command(lineIndex: Int, charIndex: Int, content: String)

  def getCharRange: (Int, Int) =
    this match
      case CrontabToken.Comment(_, content)            => (0, content.length)
      case CrontabToken.Command(_, charIndex, content) =>
        (charIndex, charIndex + content.length)
      case CrontabToken.Term(_, charIndex, _, _, content) =>
        (charIndex, charIndex + content.length)

class CrontabParser {
  private var lineIndex = 0
  private var charIndex = 0

  def parseFile(content: String): Vector[CrontabToken] = {
    val lines = content.linesIterator

    // TODO: vector builder?
    val tokens = ListBuffer.empty[CrontabToken]
    for (line <- lines) do {
      if line.isEmpty then ()
      else if line.startsWith("#") then tokens.addOne(parseComment(line))
      else tokens.addAll(parseEntry(line))

      charIndex = 0
      lineIndex += 1
    }

    tokens.toVector
  }

  private def parseComment(line: String): CrontabToken.Comment = {
    CrontabToken.Comment(
      lineIndex,
      line
    )
  }

  private def parseEntry(line: String): Vector[CrontabToken] = {
    val stringTerms = line.split(" ").take(5)
    val stringCommand = line.split(" ").drop(5).mkString(" ")

    val parsedTerms = stringTerms.zipWithIndex.map { case (stringTerm, index) =>
      // can throw but shouldn't, since index has to be less than 5
      val termType = CronTermType.indexToTermType(index)
      parseTerm(stringTerm, termType)
    }
    val parsedCommand = parseCommand(stringCommand)

    (parsedTerms :+ parsedCommand).toVector
  }

  private def parseTerm(
      termString: String,
      termType: CronTermType
  ): CrontabToken = {
    val termValue = CronTermValue.parseTermValue(termString, termType)

    val term = CrontabToken.Term(
      lineIndex,
      charIndex,
      termType,
      termValue,
      termString
    )

    // advance past term and the space after
    charIndex += termString.length + 1

    term
  }

  private def parseCommand(command: String): CrontabToken.Command = {
    CrontabToken.Command(
      lineIndex,
      charIndex,
      command
    )
  }
}
