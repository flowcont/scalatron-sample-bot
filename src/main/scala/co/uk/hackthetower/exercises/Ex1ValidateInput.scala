package co.uk.hackthetower.exercises

import cats.data.Validated._
import cats.data.{OneAnd, ValidatedNel}
import co.uk.hackthetower.commands.server._
import scala.util.{Try,Success,Failure}
import scala.collection.Map

/**
  * First exercise: Implement method 'parseInput'.
  *
  * This method must validate the input received from the server and return an object indicating
  * if the input was correct or not.
  * As per specification the input, if valid, will contain a single command, and will match one of the
  * 'ServerCommands' defined in the codebase. Incorrect scenarios may include invalid commands,
  * multiple commands, etc.
  *
  * Aims:
  * - Learn to work with Validated and OneAnd instances (NonEmptyList in this case)
  * - Understand the differences between Validated and Xor
  * - Understand the differences between OneAnd and NonEmptyList
  *
  * See:
  * - http://typelevel.org/cats/tut/validated.html
  * - http://typelevel.org/cats/tut/oneand.html
  */
object Ex1ValidateInput {

  //TODO: START HERE
  /**
    * This method parses the input of the server and validates it to ensure we got a valid command
    *
    * @param input the input sent by the server. As per specification it will only have 1 command.
    * @return a ValidatedNel[String, ServerCommand], equivalent to Validated[NonEmptyList[String], ServerCommand]
    */
  def parseInput(input: String): ValidatedNel[String, ServerCommand] = {

    if (input.isEmpty)
      Invalid(OneAnd("Empty input", List()))
    else
    {
      val comms = Try(extract(input))


      comms match {
        case Success(commands) =>
          Try(commands("Welcome")) match {
            case Success(m) => Valid(Welcome(m("name"), m("apocalypse").toInt, m("round").toInt, m("maxslaves").toInt))
            case Failure(_) => Try(commands("Goodbye")) match {
              case Success(m) => Valid(Goodbye(m("energy").toInt))
              case Failure(_) => Try(commands("React")) match {
                case Success(m) => Valid(React(m("generation").toInt, m("name"), m("time").toInt, m("view"),
                  m("energy"),
                  Try((m("master").split(":")(0).toInt, m("master").split(":")(1).toInt)) match{
                    case Success(v) => (v._1, v._2)
                    case Failure(_) => (0,0)
                  },
                  Try((m("collision").split(":")(0).toInt, m("collision").split(":")(1).toInt)) match {
                    case Success(v) => (v._1, v._2)
                    case Failure(_) => (2,2)
                  },
                  m("slaves").toInt, scala.Predef.Map(m("state") -> "")))
                case Failure(_) => Invalid(OneAnd("Invalid command " + commands.head._1, List()))
              }
            }
          }
        case Failure(_) => Invalid(OneAnd("Wrong input: Command not recognized", List()))
      }
    }


    //Invalid(OneAnd("Missing implementation", List()))
  }

  def extract(input: String): Map[String, Map[String, String]] = {
    val start = input.indexOf("(")
    val end = input.lastIndexOf(")")

    val command = input.substring(0,start)

    val group: Array[String] = input.substring(start+1,end).split(",")

//    React(generation: Int, name: String, time : Int, view: String, energy: String,
//      master: (Int, Int), collision: (Int, Int), slaves: Int, state: Map[String, String])

    val mapArgs = group.map{ elem =>
      val arr = elem.split("=")
      arr(0).toLowerCase -> arr(1)
    }.toMap

    Map(command -> mapArgs)
  }


}
