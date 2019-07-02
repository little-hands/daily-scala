package com.github.study.nnp

import scala.annotation.tailrec

/**
  * Created by tamaki on 2015/02/08.
  */
trait NNP10 {

  // P01 (*) Find the last element of a list.
  def last(list: List[Int]): Int = {
    @tailrec
    def last0(ints: List[Int]): Int = {
      ints match {
        case x :: Nil => x
        case _ :: xs => last0(xs)
      }
    }

    last0(list)
  }

  // P02 (*) Find the last but one element of a list.
  def penultimate(list: List[Int]): Int = {
    @tailrec
    def recursive(ints: List[Int]): Int = {
      ints match {
        case x :: _ :: Nil => x
        case _ :: xs => recursive(xs)
      }
    }

    recursive(list)
  }

  def nth(n: Int, list: List[Int]): Int = {

    @tailrec
    def recursive(n: Int, ints: List[Int]): Int = {
      n match {
        case 0 => ints.head
        case _ => recursive(n - 1, ints.tail)
      }
    }

    recursive(n, list)
  }

  def length(list: List[Int]): Int = {
    @tailrec
    def recursive(n: Int, ints: List[Int]): Int = {
      ints match {
        case _ :: Nil => n
        case _ :: xs => recursive(n + 1, xs)
      }
    }

    recursive(1, list)
  }

  def reverse(list: List[Int]): List[Int] = {
    @tailrec
    def recursive(ints: List[Int], result: List[Int]): List[Int] = {
      ints match {
        case Nil => result
        case x :: xs => recursive(xs, x :: result)
      }
    }

    recursive(list, Nil)
  }

  def isPalindrome(list: List[Int]): Boolean = {
    ???
  }

  def flatten(nested: List[Any]): List[Any] = {
    ???
  }

  def compress(list: List[Symbol]): List[Symbol] = {
    ???
  }

  def pack(list: List[Symbol]): List[List[Symbol]] = {
    ???
  }

  def encode(list: List[Symbol]): List[(Int, Symbol)] = {
    ???
  }

}