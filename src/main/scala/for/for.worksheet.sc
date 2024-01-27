// Generating a list of pairs (x,y), s.t. y < x < n and x + y is a prime
def genPrimeSum(n: Int): List[(Int, Int)] =
    for
        x <- (1 until n).toList
        y <- 1 until x
        if isPrime(x + y)
    yield (x, y)

// alternative rewrite
def genPrimeSum2(n: Int): List[(Int, Int)] =
    (1 until n).toList.flatMap(x =>
        (1 until x).withFilter(y => 
            isPrime(x+y)).map( y =>
                (x, y)
            )
        )
    

// elementary definition to check if a number is prime
def isPrime(k: Int): Boolean =
    (2 until k).forall(i => k % i != 0)


genPrimeSum(5)
genPrimeSum2(5)


// Books examples

case class Book(title: String, authors: List[String])

val books: List[Book] = List(
Book(title = "Structure and Interpretation of Computer Programs",
     authors = List("Abelson, Harald", "Sussman, Gerald J.")),
Book(title = "Introduction to Functional Programming",
     authors = List("Bird, Richard", "Wadler, Phil", "Bird Richard 2")),
Book(title = "Effective Java",
     authors = List("Bloch, Joshua")),
Book(title = "Java Puzzlers",
     authors = List("Bloch, Joshua", "Gafter, Neal")),
Book(title = "Programming in Scala",
     authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

// return a list of books titles whose author's name is Bird
books.flatMap(book =>
    book.authors.withFilter(author =>
        author.startsWith("Bird")).map(author =>
            book.title))

// return a list book titles that contain "Program"
books.withFilter(book =>
    book.title.contains("Program")).map(book =>
        book.title)

// return a list of authors who has written at least two books
// following a comparable SQL technique
for
    bookA <- books
    bookB <- books
    if bookA.title < bookB.title
    authorOfBookA <- bookA.authors
    authorOfBookB <- bookB.authors
    if authorOfBookA == authorOfBookB
yield authorOfBookA

// :) equivalent re-write
books.flatMap(bookA =>
    books.flatMap(bookB =>
        bookA.authors.withFilter(authorOfBookA =>
            bookA.title < bookB.title).map(authorOfBookA => 
                authorOfBookA).flatMap(authorOfBookA =>
                    bookB.authors.withFilter(authorOfBookB =>
                        authorOfBookA == authorOfBookB).map(authorOfBookB =>
                            authorOfBookA))))