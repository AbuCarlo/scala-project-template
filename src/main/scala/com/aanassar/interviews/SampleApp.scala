package com.aanassar.interviews

import scala.annotation.migration

object SampleApp extends App {

  import scalaz.Memo;

  def findChains(words: String*) = {

    val sortedWords = words.map(w => w.sorted).toSet

    lazy val f: String => Int = Memo.mutableHashMapMemo { w =>
      if (!sortedWords.contains(w))
        0
      else {
        1 + w.combinations(w.size - 1).map(f(_)).max
      }
    }

    sortedWords.map(f(_)).max
  }

  val sample = Array("a", "b", "ba", "bca", "bda", "bdca")

  println(findChains(sample: _*))
}