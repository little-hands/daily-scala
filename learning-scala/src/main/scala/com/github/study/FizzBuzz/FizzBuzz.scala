package com.github.study.FizzBuzz

import scala.annotation.tailrec
import scala.util.Try

/**
  * Created by tamaki on 2015/02/08.
  */
object FizzBuzz {
  def main(args: Array[String]): Unit = {
    println((1 to 100)
      .map(fizzBuzz)
      .filter(x => x.forall(_.isDigit))
      .map(x => x.toInt)
      .sum)
  }

@tailrec
def printFizzBuzz(num: Int, endNum: Int): Unit =
  num match {
    case x if x < endNum => {
      println(fizzBuzz(x))
      printFizzBuzz(x + 1, endNum)
    }
    case x => println(fizzBuzz(x))
  }


  def fizzBuzz(num: Int): String =
    (num % 3, num % 5) match {
      case (0, 0) => "FizzBuzz"
      case (0, _) => "Fizz"
      case (_, 0) => "Buzz"
      case _ => num.toString
    }

}
