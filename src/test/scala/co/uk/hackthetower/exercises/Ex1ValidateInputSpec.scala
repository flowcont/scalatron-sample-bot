package co.uk.hackthetower.exercises

import org.scalatest.{Matchers, FlatSpec}

import scala.util.Random


class Ex1ValidateInputSpec extends FlatSpec with Matchers {

  "parseInput" should "return Valid if input contains a valid server command" in {
    val input = "Goodbye(energy=-1)"
    Ex1ValidateInput.parseInput(input).isValid should be(true)
  }

  it should "return Invalid if input is empty" in {
    val input = ""
    Ex1ValidateInput.parseInput(input).isInvalid should be(true)
  }

  it should "return Invalid if input is nonsensical garbage" in {
    val input = Random.nextString(40)
    Ex1ValidateInput.parseInput(input).isInvalid should be(true)
  }

  it should "return Invalid if input doesn't match a server command" in {
    val input = "Log(text=this is a bot command not a server command!)"
    Ex1ValidateInput.parseInput(input).isInvalid should be(true)
  }

  it should "return Invalid if input contains multiple server commands" in {
    val input = "Goodbye(1)|Goodbye(4)"
    Ex1ValidateInput.parseInput(input).isInvalid should be(true)
  }

}
