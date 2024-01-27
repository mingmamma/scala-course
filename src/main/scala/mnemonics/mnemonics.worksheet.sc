class Mnemonics(dictionary: Set[String]):

  private val phoneKeys: Map[Char, String] = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ"
  )

  private val letterToDigit: Map[Char, Char] =
    for
      (digit, letters) <- phoneKeys
      letter           <- letters
    yield letter -> digit

  private def wordToDigits(word: String): String =
    word.toUpperCase.map(letterToDigit)
  
  private val wordsByDigits: Map[String, Set[String]] = 
    dictionary.groupBy(wordToDigits)
  
// recusive split that takes a sequence of digits as a parameter 
// and returns a collection listing all the ways to split that 
// sequence of digits into sub-sequences of digits  
  def ofPhoneNumber(digits: String): Seq[Seq[String]] = 
    if digits.isEmpty() then Seq(Nil)
    else 
      for
        splitPoint <- 1 to digits.length
        // str.splitAt(n) == (str.take(n), str.drop(n))
        (firstDigits, remainingDigits) = digits.splitAt(splitPoint)
        word <- wordsByDigits.getOrElse(firstDigits, Nil)
        words <- ofPhoneNumber(remainingDigits)
      yield word +: words
end Mnemonics

val dictionary = Set("of", "me", "on", "no")
val mnemonics = Mnemonics(dictionary)
mnemonics.ofPhoneNumber("63")
mnemonics.ofPhoneNumber("6366")