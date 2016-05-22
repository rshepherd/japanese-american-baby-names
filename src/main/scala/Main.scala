import purecsv.safe._
import java.io.PrintWriter
import scala.util.Success

object Main {

  // Japanese is formed with phonetic alphabet(s). What follows is a possibly incomplete set of Japanese phonemes.
  val syllabary = Set("a", "i", "u", "e", "o", "ka", "ki", "ku", "ke", "ko", "kya", "kyu", "kyo", "sa", "shi", "su",
    "se", "so", "sha", "shu", "sho", "ta", "chi", "tsu", "te", "to", "cha", "chu", "cho", "na", "ni", "nu", "ne",
    "no", "nya", "nyu", "nyo", "ha", "hi", "fu", "he", "ho", "hya", "hyu", "hyo", "ma", "mi", "mu", "me", "mo", "mya",
    "myu", "myo", "ya", "yu", "yo", "ra", "ri", "ru", "re", "ro", "rya", "ryu", "ryo", "wa", "wi", "we", "wo", "n", "ga",
    "gi", "gu", "ge", "go", "gya", "gyu", "gyo", "za", "ji", "zu", "ze", "zo", "ja", "ju", "jo", "da", "ji", "zu", "de",
    "do", "ja", "ju", "jo", "ba", "bi", "bu", "be", "bo", "bya", "byu", "byo", "pa", "pi", "pu", "pe", "po", "pya",
    "pyu", "pyo", "li")


  // A case class encapsulating a record in the the census data for a 'given' American name
  case class Name(name: String, gender: String, usages: Int) {
    // Method that tests if this name meets our criteria.
    def isCandidate = {
      def isFemale = gender.headOption.contains('F')
      def hasUsages = usages >= Name.MIN_USAGES
      isFemale && hasUsages && Name.isJapanese(name.toLowerCase)
    }

    def +(n: Name) = Name(name, gender, usages + n.usages)

    override def toString = s"""$name, $usages\n"""
  }

  case object Name {
    // The minimum number of times that name needs to appear in a given year's census data
    // in order for it to be taken into consideration.
    final val MIN_USAGES = 250

    // The algorithm itself is 'pretty good'. Its by no means perfect. And keep in
    // mind that its only attempting to identify possible candidates. There will certainly
    // be plenty of names output that would not work well at all.
    // However, I might argue worse is better for this particular purpose.
    def isJapanese(name: String): Boolean = name.length() match {
      case len if len >= 3 && syllabary.contains(name.take(3)) => isJapanese(name.drop(3))
      case len if len >= 2 && syllabary.contains(name.take(2)) => isJapanese(name.drop(2))
      case len if len >= 1 && syllabary.contains(name.take(1)) => isJapanese(name.drop(1))
      case len => len == 0
    }
  }

  // The approach is certainly not intended to be efficient, since the size of the data is
  // fairly modest. This program will run over 135 years worth of data in a couple seconds.

  def main(args: Array[String]) {
    // Extract all the names that are phonetically valid in Japanese
    // yielding a list that is those names sorted by popularity.
    val names = (1880 until 2015).par.flatMap { year =>
      val filename = getClass.getResource(s"yob$year.txt").getFile
      val names = CSVReader[Name].readCSVFromFileName(filename, skipHeader = true)
      names.flatMap {
        case Success(n) if n.isCandidate => Some(n)
        case _ => None
      }
    }.groupBy(_.name).mapValues(_.reduce(_ + _)).values.toList.sortBy(_.usages)

    // Dump the list to a file.
    new PrintWriter("names.txt") {
      names.foreach { n =>
        write(n.toString)
      }
      close()
    }

    println("arigatou gozaimasu!")
  }

}