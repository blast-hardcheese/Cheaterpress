import scopt.immutable.OptionParser

object App {
  type WordMapping = Map[LetterMap, WordList]
  type LetterMap = List[Tuple2[Char, Int]]
  type WordList = List[String]

  case class Config(
    limit: Int = 15,
    lengths: List[Int] = List(), // [7], [7, 8, 9]
    priority: String = "",
    gamefile: String = ""
  )

  def getWordsFromDirectory(dirName: String) = {
    getWords(new java.io.File(dirName).listFiles)
  }

  def getWords(fileList:Array[java.io.File]) = {
    (fileList.flatMap { f =>
      io.Source.fromFile(f).getLines
    }).toList.sorted
  }

  def groupLetters(word: String) = {
    word.toList.sorted.foldLeft[LetterMap](List(Tuple2(' ', 0)))((a: LetterMap, b: Char) => a.last match {
      case Tuple2(`b`, c) => a.init :+ Tuple2(b, c+1)
      case _ => a :+ Tuple2(b, 1)
    }).tail
  }

  def createGraphFromWords(words: WordList): WordMapping = {
    words.foldLeft[WordMapping](Map())((mapping, word: String) => {
      val letterGroups = groupLetters(word)
      val wordList = mapping.getOrElse(letterGroups, List())
      mapping.updated(letterGroups, (wordList :+ word))
    })
  }

  def lettersInLetters(small: LetterMap, big:LetterMap) = {
    val bigMap = big.toMap

    val available = small.map { case (char, needed) =>
      bigMap.getOrElse(char, 0) >= needed
    }

    !(available contains false)
  }

  def processRawLettersInWordMap(rawLetters: String, wordMap: WordMapping) = {

    val allLetters = groupLetters(rawLetters)
    val filteredKeys = wordMap.filterKeys(potentialLetters => {
      lettersInLetters(potentialLetters, allLetters)
    })
    val words = filteredKeys.toList.flatMap( { case (_, wordList) => wordList } )

    words.sortWith( (left, right) => {
      left.length < right.length
    } ).reverse
  }

  def useConfig(config: Config) {
    val limit = config.limit
    val lengths = config.lengths
    val priority = config.priority
    val filename = config.gamefile

    val wordList = getWordsFromDirectory("letters")
    val wordMap = createGraphFromWords(wordList)

    import java.io.{FileReader, FileNotFoundException, IOException}

    try {
      val game = io.Source.fromFile(filename).getLines.toList
      val played = game.tail

      val notUsedYetOption = game.headOption map { rawLetters =>
        val sortedWords = processRawLettersInWordMap(rawLetters, wordMap)
        sortedWords.filter(word => !(played contains word))
      }

      val notUsedYet = notUsedYetOption.getOrElse(List())
      val lengthFiltered = if(lengths.length == 0) {
        notUsedYet
      } else {
        notUsedYet.filter { word => lengths contains word.length }
      }

      val priorityFiltered = if(priority.size == 0) {
        lengthFiltered
      } else {
        lengthFiltered.map({ word =>
          val wordList = word.toList
          ((wordList intersect priority).size, word)
        }).filter(pair => pair._1 > priority.size / 2).sortWith( (left, right) => {
          (left._1 * 100 + ( 25 - left._2.length ) ) > (right._1 * 100 + ( 25 - right._2.length ) )
        }).flatten( pair => List(pair._2) )
      }

      val product = priorityFiltered

      product.take(limit).foreach( word => println(word.length + ": " + word) )
    } catch {
      case ex: FileNotFoundException => println("Unable to access \"" + filename + "\"")
      case ex: IOException => println("Had an IOException trying to read \"" + filename + "\"")
    }
  }

  def main(args: Array[String]) {
    val parser = new OptionParser[Config]("Cheaterpress", "1.1") { def options = Seq(
      intOpt("l", "limit", "Maximum values to return (Default: 15)") { (v: Int, c: Config) => c.copy(limit = v) },

      opt("r", "length", "Restrict words to specified lengths. Specified as a space-seperated list: \"7\", or \"7 8 9 10\"."){ (v: String, c: Config) =>
        val lengths = v.split(" ").toList.map(Integer.parseInt)
        c.copy(lengths = lengths)
      },

      opt("p", "priority", "Include as many of these letters in the result as possible") { (v: String, c: Config) => c.copy(priority = v) },

      arg("<file>", "Game file to play") { (v: String, c: Config) => c.copy(gamefile = v) }
    ) }

    parser.parse(args, Config()) map { config =>
      useConfig(config)
    } getOrElse {
      // arguments are bad, usage message will have been displayed
    }
  }
}
