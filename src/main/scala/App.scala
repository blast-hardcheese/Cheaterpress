import scopt.immutable.OptionParser

object App {
  type WordMapping = Map[LetterMap, WordList]
  type LetterMap = List[Tuple2[Char, Int]]
  type WordList = List[String]

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

  def main(args: Array[String]) {
    case class Config(foo: Int = 5, bar: String = "bar", xyz: Boolean = false, libname: String = "libname", libfile: String = "libfile", maxlibname: String = "maxlibname", maxcount: Int = 5, whatnot: String = "whatnot")

    val parser = new OptionParser[Config]("scopt", "2.x") { def options = Seq(
      intOpt("f", "foo", "foo is an integer property") { (v: Int, c: Config) => c.copy(foo = v) },
      opt("o", "output", "output") { (v: String, c: Config) => c.copy(bar = v) },
      booleanOpt("xyz", "xyz is a boolean property") { (v: Boolean, c: Config) => c.copy(xyz = v) },
      keyValueOpt("l", "lib", "<libname>", "<filename>", "load library <libname>")
        { (key: String, value: String, c: Config) => c.copy(libname = key, libfile = value) },
      keyIntValueOpt(None, "max", "<libname>", "<max>", "maximum count for <libname>")
        { (key: String, value: Int, c: Config) => c.copy(maxlibname = key, maxcount = value) },
      arg("<file>", "some argument") { (v: String, c: Config) => c.copy(whatnot = v) }
    ) }
    // parser.parse returns Option[C]
    parser.parse(args, Config()) map { config =>
      println(config)
    } getOrElse {
      // arguments are bad, usage message will have been displayed
    }

    if(args.length == 0) {
      val wordMap = Map(
        List(('a',2), ('l',1), ('r',1), ('s',1), ('t',1))                   -> List("altars", "astral", "ratals", "talars", "tarsal"),
        List(('d',1), ('e',1), ('i',1), ('k',1), ('n',1), ('r',1), ('s',1)) -> List("kinders", "kinreds", "redskin"),
        List(('e',1), ('o',1), ('p',1), ('s',2), ('t',1))                   -> List("estops", "pestos", "posset", "ptoses", "stoeps", "stopes"),
        List(('e',1), ('h',1), ('l',1), ('o',1), ('s',1))                   -> List("helos", "holes", "hosel", "sheol"),
        List(('i',1), ('k',1), ('s',1))                                     -> List("kis", "sik", "ski")
      )
      val rawLetters = "a"
      val allLetters = groupLetters(rawLetters)
      val filteredKeys = wordMap.filterKeys(potentialLetters => {
        lettersInLetters(potentialLetters, allLetters)
      })
    } else {
      val filename = args(0)

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
        for(word <- notUsedYet.take(15)) {
          println(word.length + ": " + word)
        }
      } catch {
        case ex: FileNotFoundException => println("Unable to access \"" + filename + "\"")
        case ex: IOException => println("Had an IOException trying to read \"" + filename + "\"")
      }
    }
  }
}
