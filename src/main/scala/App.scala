import scopt.immutable.OptionParser

object App {
  type WordMapping = Map[LetterMap, WordList]
  type LetterMap = List[Tuple2[Char, Int]]
  type WordList = List[String]

  case class Config(
    limit: Int = 15,
    lengths: List[Int] = List(), // [7], [7, 8, 9]
    priority: String = "",
    gamefile: String = "",
    wordTree: String = "letters"
  )

  def getWordsFromDirectory(dirName: String): WordList = {
    getWords(new java.io.File(dirName).listFiles)
  }

  def getWords(fileList:Array[java.io.File]): WordList = {
    (fileList.flatMap { f =>
      io.Source.fromFile(f).getLines
    }).toList.sorted
  }

  def groupLetters(word: String): LetterMap = {
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

  def lettersInLetters(small: LetterMap, big:LetterMap): Boolean = {
    val bigMap = big.toMap

    small.foldLeft[Boolean](true)( (r, next: Tuple2[Char, Int]) => {
      r && bigMap.getOrElse(next._1, 0) >= next._2
    })
  }

  def processRawLettersInWordMap(rawLetters: String, wordMap: WordMapping): WordList = {
    // Returns sorted list of all possible words in current game
    val allLetters = groupLetters(rawLetters)
    val filteredKeys = wordMap.filterKeys(potentialLetters => {
      lettersInLetters(potentialLetters, allLetters)
    })
    val words = filteredKeys.toList.flatMap( { case (_, wordList) => wordList } )

    words.sortWith( (left, right) => {
      left.length < right.length
    } ).reverse
  }

  def playGame(config: Config, lines: List[String], wordMap: WordMapping): List[String] = {
    val lengths = config.lengths
    val priority = config.priority

    val played = lines.tail

    // Get all playable words (words that are possible in this game, ignore words that have been played (or start with a word that has been played)
    val notUsedYet: List[String] = lines.headOption.map({ rawLetters =>
      val sortedWords = processRawLettersInWordMap(rawLetters, wordMap)
      sortedWords.filterNot(word => played.foldLeft[Boolean](false)( (r, next) => r || next.startsWith(word) ))
    }).getOrElse(List())

    // Filter by length
    val lengthFiltered = if(lengths.isEmpty) {
      notUsedYet
    } else {
      notUsedYet.filter { word => lengths contains word.length }
    }

    // Filter by priority letters
    val priorityFiltered = if(priority.isEmpty) {
      lengthFiltered
    } else {
      lengthFiltered.map({ word =>
        val wordList = word.toList
        ((wordList intersect priority).size, word) // (numberOfPriorityLetters, word)
      }).filter(pair => pair._1 > priority.size / 2).sortWith( (left, right) => {
        (left._1 * 100 + ( 25 - left._2.length ) ) > (right._1 * 100 + ( 25 - right._2.length ) ) // (intersections * 100 + (25 - wordSize)) <- Prioritize highest intersections, then by fewest characters
      }).map( pair => pair._2 )
    }

    priorityFiltered
  }

  def getWords(config: Config, words: List[String]): String = {
    val limit = config.limit
    words.take(limit).map( word => word.length + ": " + word ).mkString("\n")
  }

  def getStats(config: Config, words: List[String]): String = {
    type WordlengthCountPair = Tuple2[Int, Int]

    words.foldLeft[List[WordlengthCountPair]](List((0, 0)))((last: List[WordlengthCountPair], next: String) => {
      val length = next.length
      last.last match {
        case (`length`, count) => last.init :+ (length, count + 1)
        case _ => last :+ (length, 1)
      }
    }).tail.map( _.toString ).mkString("\n")
  }

  def useConfig(config: Config) {
    import java.io.{FileReader, FileNotFoundException, IOException}

    val filename = config.gamefile
    val wordTree = config.wordTree

    try {
      val wordList = getWordsFromDirectory(wordTree)
      val wordMap = createGraphFromWords(wordList)

      val lines = io.Source.fromFile(filename).getLines.toList
      val wordsLeft = playGame(config, lines, wordMap)

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
