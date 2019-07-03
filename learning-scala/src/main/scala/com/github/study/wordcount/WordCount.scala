package com.github.study.wordcount


case class Fruit(s: String) extends AnyVal
class Fruit2(val s: String) extends AnyVal

/**
  * Created by tamaki on 2015/02/08.
  */
class WordCount {

  //------------------------------------------------------
  // ワードカウント問題
  // https://gist.github.com/j5ik2o/7210762
  //------------------------------------------------------
  def countFruitsFromLines(lines: List[String]): Map[String, Int] = {
    val m: (String, Int) = ("aa", 2)

    lines.flatMap(_.split(" "))
      .groupBy(identity)
//            .map { x =>
//              x match {
//                case (k, v) if v.nonEmpty => k -> v.length
//                case (k, v)  => k -> v.length
//              }
//            }
      .map { case (k, v) => k -> v.length }

    lines.flatMap(_.split(" "))
      .groupBy(identity)
        .map(x => x._1 -> x._2)

    lines.flatMap(_.split(" "))
      .groupBy(identity)
      .mapValues(_.length)

    lines
      .foldLeft(List[String]())(
        (list, str) => list ++ str.split(" ").toList)
      .foldLeft(Map[String, Int]())(
        (map, str) => map.get(str) match {
          case None => map + (str -> 1)
          case Some(count) => map + (str -> (count + 1))
        })
  }

  // 以下、メソッド分割して見たパターン

  def countFruitsFromLines2(lines: List[String]): Map[String, Int] = {
    lines
      .foldLeft(List[String]())((list, str) => flattenSpaceSeparatedStrings(list, str))
      .foldLeft(Map[String, Int]())((map, str) => incrementKeyCount(map, str))
  }

  private def flattenSpaceSeparatedStrings(list: List[String], str: String): List[String] =
    list ++ str.split(" ").toList

  private def incrementKeyCount(map: Map[String, Int], str: String) = {
    map.get(str) match {
      case None => map + (str -> 1)
      case Some(count) => map + (str -> (count + 1))
    }
  }

  // 関数型っぽくないやつ

  def countFruitsFromLines3(lines: List[String]): Map[String, Int] = {
    val fruits = joinSpaceSeparatedStrings(lines)
    countFruits(fruits)
  }

  def joinSpaceSeparatedStrings(target: List[String]): List[String] = {
    target.foldLeft(List[String]())(
      (list, str) => list ++ str.split(" ").toList)
  }

  def countFruits(fruits: List[String]): Map[String, Int] = {
    fruits.flatMap(_.split(" "))
      .groupBy(identity)
      .mapValues(_.length)


    //    fruits.groupBy(_ => identity(_))
    //    fruits.groupBy(_ => identity(_))
    //    fruits.groupBy(identity)
    //
    //    fruits.foldLeft(Map[String, Int]())(
    //      (map, str) => map.get(str) match {
    //        case None => map + (str -> 1)
    //        case Some(count) => map + (str -> (count + 1))
    //      })
  }

}


//  /**
//   * 勉強会をやる発端となったダメコード
//   * @param lines
//   * @return
//   */
//  def countFruitsFromLines(lines: List[String]): Map[String, Int] = {
//    lines.foldLeft(new scala.collection.mutable.HashMap[String, Int]) { (b, line) =>
//      line.split(" "). map( m => {
//        val cnt:Int = b.get(m).getOrElse(0) + 1
//        b.put(m, cnt)
//      })
//      b
//    }.toMap
//  }