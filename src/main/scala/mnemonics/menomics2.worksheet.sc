class Menonimcs2(dict: Set[String]):
    val letterToDigit2: Map[Char, Char] =
        Map( 'A' -> '2', 'B' -> '2', 'C' -> '2',
             'D' -> '3', 'E' -> '3', 'F' -> '3',
             'G' -> '4', 'H' -> '4', 'I' -> '4',
             'J' -> '5', 'K' -> '5', 'L' -> '5',
             'M' -> '6', 'N' -> '6', 'O' -> '6',
             'P' -> '7', 'Q' -> '7', 'R' -> '7', 'S' -> '7',
             'T' -> '8', 'U' -> '8', 'V' -> '8',
             'W' -> '9', 'X' -> '9', 'Y' -> '9', 'Z' -> '9')

    val phoneKeys: Map[Char, String] = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

    def letterToDigit(phoneKeys: Map[Char, String]): Map[Char, Char] =
        for
            (num, letters) <- phoneKeys
            letter <- letters
        yield (letter, num)

    assert(letterToDigit(phoneKeys) == letterToDigit2)

    def wordToDigits(word: String): String =
        word.toUpperCase().map(letterToDigit2)

    assert(wordToDigits("Scala") == "72252")
    assert(wordToDigits("Rocks") == "76257")

    val dictionary = Set("Scala", "rocks", "is", "fun", "love", "thank", "me", "you", "of")
    
    val digitToWordsIndex: Map[String, Set[String]] = 
        dictionary.groupBy(s => wordToDigits(s))

    def split(digit: String): Seq[Seq[String]] = 
        if digit.isEmpty() then Seq(Nil)
        else 
            (1 to digit.length).flatMap(i =>
                val (head, tail) = digit.splitAt(i)
                split(tail).map(tailSeq =>
                    head +: tailSeq))

    def split2(digit: String): Seq[Seq[String]] =
        if digit.isEmpty() then Seq(Nil)
        else
            for
                i <- (1 to digit.length)
                (head, tail) = digit.splitAt(i)
                tailSeq <- split(tail)
            yield head +: tailSeq
            

    assert(split("837") == Vector(List("8", "3", "7"), List("8", "37"), List("83", "7"), List("837")))
    assert(split2("837") == Vector(List("8", "3", "7"), List("8", "37"), List("83", "7"), List("837")))

    def phrases(digit: String): Seq[Seq[String]] =
        if digit.isEmpty() then Seq(Nil)
        else
            (1 to digit.length).flatMap(i =>
                val (head, tail) = digit.splitAt(i)
                digitToWordsIndex.getOrElse(head, Nil).flatMap(headWord =>
                    phrases(tail).map(tailPhrase =>
                        headWord +: tailPhrase)))

    def phrases2(digit: String): Seq[Seq[String]] =
        if digit.isEmpty() then Seq(Nil)
        else
            for
                i <- (1 to digit.length)
                (head, tail) = digit.splitAt(i)
                headWord <- digitToWordsIndex.getOrElse(head, Nil)
                tailPhrase <- phrases(tail)
            yield headWord +: tailPhrase

    phrases("63")
    phrases2("63")

val dictionary = Set("Scala", "rocks", "is", "fun", "love", "thank", "me", "you", "of")

val menonimcs2 = Menonimcs2(dictionary)
menonimcs2.phrases("7225276257")
menonimcs2.phrases2("7225276257")

menonimcs2.phrases("7225247386")
menonimcs2.phrases2("7225247386")

menonimcs2.phrases("7225284265968")
menonimcs2.phrases2("7225284265968")

menonimcs2.phrases("968568363")
menonimcs2.phrases2("968568363")