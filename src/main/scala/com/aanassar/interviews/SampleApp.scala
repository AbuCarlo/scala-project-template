package com.aanassar.interviews

import org.log4s.getLogger

import scala.annotation.migration

object SampleApp extends App {
  
  private[this] val logger = getLogger

  import scalaz.Memo;

  def findChains(words: String*) = {
    
    val sortedWords = words.map(w => w.sorted).toSet
    
    logger.trace(s"Words reduced to ${sortedWords}")

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

  assert(findChains(sample: _*) == 4)
    
  assert(findChains("tony", "not") == 2)
    
  assert(findChains("a") == 1)
  
  assert(findChains("z", "zz", "zzz", "x", "xx") == 3)
}