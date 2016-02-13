package co.uk.hackthetower.exercises

import cats.data.Xor
import co.uk.hackthetower.commands.server.{React, Goodbye}
import org.scalatest.{FlatSpec, Matchers}


class Ex2BotLogicSpec extends FlatSpec with Matchers {

  "processServerCommand" should "return Left if Goodbye is received with a large negative number as we can't lose, " +
    "obviously it's a mistake!" in {
    val input = Xor.right(Goodbye(-10000))
    Ex2BotLogic.processServerCommand(input).isLeft should be(true)
  }

//React(generation=int,name=string,time=int,view=string,energy=string,master=int:int,collision=int:int,slaves=int,...)

  "processServerCommand" should "return Right if React is received " in {
    val input = Xor.right(React(0,"Master",2,"","10",(1,1),(2,2),0,Map()))
    Ex2BotLogic.processServerCommand(input).isRight should be(true)
  }

  "processServerCommand" should "return Right if React as String is received " in {
    val input = Xor.left("""React(generation=0,name=Master,time=2,view="",energy=10,master=1:1,collision=2:2,slaves=0,state=None""")
    Ex2BotLogic.processServerCommand(input).isRight should be(true)
  }
  //TODO: add tests for your logic!
}
