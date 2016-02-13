package co.uk.hackthetower.exercises

import cats.data.Validated.Valid
import cats.data.{ValidatedNel, Validated, Xor}
import co.uk.hackthetower.commands.bot.{Explode, Spawn, Move, BotCommands}
import co.uk.hackthetower.commands.server.{Goodbye, React, Welcome, ServerCommand}

/**
  * Second exercise: Implement method 'processServerCommand'
  *
  * This method receives an Xor[String, ServerCommand] instance and will return an Xor answer.
  *
  * Input:
  * - we receive a Left of String if the input is invalid
  * - we receive a Right of ServerCommand if we got the right command
  *
  * Note that receiving a Left doesn't mean we can't still send BotCommands to the server with instructions.
  *
  * Output:
  * - if the processing fails, we will send a Left with an error message.
  * This will be automatically converted to Say and Log commands for the server.
  * - if the processing succeeds, we will send a Right with a list of BotCommands to send to the server
  *
  * Aims:
  * - Learn to work with Xor to propagate error states
  * - Learn to use both right/left sides as well as mapping over them
  *
  * See:
  * - http://typelevel.org/cats/tut/xor.html
  */
object Ex2BotLogic {

  val r = scala.util.Random

  def processServerCommand(command: Xor[String, ServerCommand]): Xor[String, List[BotCommands]] = {

    val comm =
    if (command.isLeft){
      val a: ValidatedNel[String, ServerCommand] = Ex1ValidateInput.parseInput(command.swap.getOrElse(""))
      if (a.isValid) a.getOrElse()
      else a
    }
    else
      command.getOrElse()

    //    React(generation: Int, name: String, time : Int, view: String, energy: String,
    //      master: (Int, Int), collision: (Int, Int), slaves: Int, state: Map[String, String])

    val n1 = r.nextInt(2) match {
      case 0 => 0
      case 1 => 1
      case 2 => -1
    }

    val n2 = r.nextInt(2) match {
      case 0 => 0
      case 1 => 1
      case 2 => -1
    }

    comm match {
      case Welcome(name,apoc,round,max) => Xor.Right(List(Move(1,1), Spawn((1,-1),"miniMe",1000,Map())))
      case React(0, name, t, view, energy, m, coll, numSlaves, _) if coll._1 < 2 => Xor.Right(List(Move(n1,n2) ) )
      case React(0, name, t, view, energy, m, coll, numSlaves, _) => Xor.Right(List(Move(1,1),
        Spawn((1,-1),"miniMe",2,Map()) ) )
      case React(gen, name, t, view, energy, m, coll, numSlaves, _) if coll._1 < 2 => Xor.Right(List(Explode(2)))
      case React(gen, name, t, view, energy, m, coll, numSlaves, _) => Xor.Right(List(Move(n1,n2)))
      case Goodbye(energy) => Xor.Left("Bye bye!!")
      case _ => Xor.Left(comm.toString)
    }

//    Xor.left("Not sure what to do")
  }




}
