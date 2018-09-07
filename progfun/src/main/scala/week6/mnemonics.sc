import scala.io.Source

object mnemonics {
  val wordsfile = "C:\\Users\\denis_verkhoturov\\IdeaProjects\\coursera\\progfun\\src\\main\\resources\\week6\\linuxwords.txt"
  val source = Source.fromFile(wordsfile)
  val words = source.getLines.toList filter (_ forall (_.isLetter))

  val mnemonics = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  val charCode: Map[Char, Char] = for ((digit, string) <- mnemonics; letter <- string) yield letter -> digit

  def wordCode(word: String): String = word.toUpperCase map charCode

  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq.empty

  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List.empty)
    else (for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest).toSet

  def translate(number: String): Set[String] = encode(number) map (_ mkString " ")

  wordCode("Java")
  translate("7225247386")
}
