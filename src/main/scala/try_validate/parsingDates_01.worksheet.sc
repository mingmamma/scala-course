import scala.util.Success
import scala.io.Source
import scala.util.Using.Manager.Resource
import scala.util.Using.Releasable
import java.io.FileReader
import java.io.BufferedReader
import scala.reflect.io.File
import scala.util.Using
import java.time.{LocalDate, Period}

import scala.util.Try

def readFromSource(filepath: String): Try[Vector[String]] =
  // Construct a Try is done the apply method of companion object of Try that takes a by-name parameter
  // https://www.scala-lang.org/api/3.3.1/scala/util/Try$.html#
  // Given the implementation of apply: https://github.com/scala/scala/blob/v2.13.10/src/library/scala/util/Try.scala#L209-L212
  // def apply[T](r: => T): Try[T] =
  //  try Success(r) catch {
  //    case NonFatal(e) => Failure(e)
  //  }
  // we see the wrapping of the keyword try, which entails the property of the apply method s.t. if there is an exception
  // during the executation of a block of code, the execution STOPS at the line of code where exception occurs, resulting
  // in NOT executing remaining lines of the block of code
  Try {
    // the fromFile method in Source returns an instance of BufferedSource. 
    // where Source and BufferedSource provides the convinience of creating iterable representation of a source file
    // https://www.scala-lang.org/api/3.3.1/scala/io/Source.html#
    // https://www.scala-lang.org/api/3.3.1/scala/io/BufferedSource.html#
    // Hence a BufferedSource can be understood as a java.io.BufferReader with extra iterator capacity
    // https://www.scala-lang.org/api/3.3.1/scala/io/Source$.html#fromFile-43f
    val source = Source.fromFile(filepath)
    val dateStrings = source.getLines().toVector
    // Intended the close the underlying resource https://www.scala-lang.org/api/3.3.1/scala/io/BufferedSource.html#close-94c
    // With previous lines likely to throw an exception, there is a risk that the resource is not closed in this implementation
    source.close()
    dateStrings
  }

def readFromSource2(filePath: String): Try[Vector[String]] = 
  // the Using provides automatic resource management, intended to mitigate the risk of unclosed resource 
  // in event of exception in the last implementation
  // https://www.scala-lang.org/api/3.3.1/scala/util/Using$.html#
  // Only managing a single resource in the following use case, the apply method of Using is suitable which take two parameters.
  // https://www.scala-lang.org/api/3.3.1/scala/util/Using$.html#apply-fffff48c
  Using(Source.fromFile(filePath))(source =>
    // make use of the getLines call from the iterator capacity
    source.getLines().toVector
  )

// read and parse dates V1
def readAndParseDates(filePath: String): Try[Vector[LocalDate]] =
  readFromSource(filePath).flatMap((dateStrings: Vector[String]) =>
    // Considering the types for reasoning the following implementation.
    // As given, the return type in the end is Try[Vector[LocalDate]]. Hence parameter the function parameter in
    // the current flatMap gives Vector[String] => Try[Vector[LocalDate]]. Noting this implies the unsuitability of map
    // or flatMap since the type of these would give Vector[?]. With that in mind, foldLeft[Try[Vector[LocalDate]]]
    // would give the expected return type while providing the mechenism of element-wise processing of the Vector collection
    dateStrings.foldLeft[Try[Vector[LocalDate]]](Success(Vector.empty))((tryDates, dateString) =>
      tryDates.flatMap(parsedDates =>
        Try(LocalDate.parse(dateString)).map(date =>
          parsedDates :+ date
        )
      )
      // would-be equivelent for-expression
      // for 
      //   parsedDates <- tryDates
      //   date <- Try(LocalDate.parse(dateString))
      // yield
      //   parsedDates :+ date
    )
  )

// Noting the limit of modelling possibly multiple error/exceptions with Try is that given the property of Try that it stops
// on the first occurance of an error/exceptions to return as the Failure containing the throwable implies subsequent error/exceptions are omitted
readAndParseDates("./src/main/scala/try_validate/dates-file.txt")

// Modelling validated values is best done with Either type, noting that Either type takes two type parameters
// and multiple errors&exceptions are modelled by a Seq[String] (of error messages)
type Errors = Seq[String]
type Validated[A] = Either[Errors, A]

def parseDate(str: String): Validated[LocalDate] =
  // java.time.LocalDate.parse() method to parse text String into a localDate instance
  // with the possibility to throw a DateTimeParseException
  // https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#parse-java.lang.CharSequence-
  Try.apply(LocalDate.parse(str))
    // Cast the Try into an Either with the toEither that gets a Either[Throwable, LocalDate]
    // https://www.scala-lang.org/api/3.3.1/scala/util/Try.html#
    .toEither
    // and the additional required transformation on the left to get to the Validated type from the original modelling
    // Either[Throwable, LocalDate] => LeftProjection[Throwable, LocalDate] => Either[Seq[String], LocalDate]
    // https://www.scala-lang.org/api/3.3.1/scala/util/Either$$LeftProjection.html
    .left.map(error => Seq(error.getMessage))

// Validate V1
def validatePeriod(str1: String, str2: String): Validated[Period] =
  // Both flatMap and map only affect the Right and keep the Left unchanged
  // https://www.scala-lang.org/api/3.3.1/scala/util/Either.html#
  // https://www.scala-lang.org/api/3.3.1/scala/util/Either.html#map-fffff769
  // Hence in this implementation error strings are NOT accumulated yet
  parseDate(str1).flatMap(date1 =>
    parseDate(str2).map(date2 =>
      Period.between(date1, date2)
    )
  )

// Validate V1 implementation having the similar limit with read and parse dates V1 of only having the first occurance of error/exception
validatePeriod("not a date first", "not a date second")
validatePeriod("2024-01-26", "not a date second")
// A valid object of a negative period
validatePeriod("2024-01-26", "2024-01-25")

def readFromSource3(filePath: String): Try[Vector[String]] =
  // Example from Using object
  // First by name argument of RELEASABLE is given a bufferedReader which takes a Reader instance and within that, create a FileReader instance which take a String (as path)
  Using(new BufferedReader(new FileReader(filePath)))((reader =>
    // Second argument is a higher order function where the second part is type annotated as below
    // that part works by using the continually utility in the Iterator companion object
    // which takes an by-name argument that is for the evaluation of next to create an INFINITE Iterator
    // Noting that the evaluation of reader.readLine() returns a String for each invocation to return a line of text terminuated by but not including LF/CR/EOF
    // before it returns null when all done, which is detected with the takeWhile() call that creates the FINITE Iterator (of the desired data)
    (Iterator.continually(reader.readLine()).takeWhile(_ != null).toVector: Vector[String])
  ))

// Validate V2, reasoning the contract of the method s.t. it should take two valid values to return a valid pair of values,
// and otherwise if any value is invalid, the return type is the Error type (which should accumulate BOTH errors by the IMPLEMENTAION)
// gives the type of method (even without implementation) to be: Validated[A], Validated[B] => Validated[(A, B)]
def zip[A, B](validatedA: Validated[A], validatedB: Validated[B]): Validated[(A, B)] =
  (validatedA, validatedB) match 
    case (Right(a), Right(b)) => Right((a, b)) 
    case (Left(e),  Right(_)) => Left(e)
    case (Right(_), Left(e))  => Left(e)
    case (Left(e1), Left(e2)) => Left(e1 ++ e2)

// The return type is an Either of a right value of a vector of ALL the dates if every string are valid be parsed, or a left value of a 
// sequnce of ALL error strings accumulated beyond the first occurance of error
// Using a List as the input collection SEEM to have performance concern with the append op in foldLeft, hence working with a Vector[String] as input
def parseDates(dates: Vector[String]): Validated[Vector[LocalDate]] =
  // the type annotation SEEMS neccessary for foldLeft, which would be sensible otherwise the type inference of first parameter
  // of foldLeft is ambigious
  dates.foldLeft[Validated[Vector[LocalDate]]]
    (Right(Vector.empty)) 
    ((validatedDates, nextDateString) =>
      val nextParsed = parseDate(nextDateString)
      // if any of validatedDates or nextDateString is a Left value, the (three) branches of zip
      // dealing with present Left values(s) get used s.t. the error strings are accumulated
      zip(validatedDates, nextParsed).map((previousValidDates, parsedDate) =>
        // the parameter function in map ONLY deals with a right value
        // https://www.scala-lang.org/api/3.3.1/scala/util/Either.html#map-fffff769
        previousValidDates :+ parsedDate)
    )

// Parse all valid input Strings
parseDates(Vector("2020-01-04", "2020-08-09", "2023-11-11"))
// read and parse dates V2. Noting improving from the limit of V1 of only being able to accumulate the first occurance of error
parseDates(Vector("first unparsabble text", "2020-09-13", "second unparsable text"))    
  
// Generalize validate V2, the elements abstracted away is the input collection of elements of generic type, and the parse function
// taking generic types: A => Validated[B]
def traverse[A, B](as: Vector[A], parseFunc: A => Validated[B]): Validated[Vector[B]] =
  as.foldLeft[Validated[Vector[B]]](Right(Vector.empty))(
    (validatedBs, nextA) => 
      val nextParsed = parseFunc(nextA)
      zip(validatedBs, nextParsed).map((previousValidBs, validNextA) =>
        // reminder: :+ is alias for `appended`, note that `:`ending operators are right associative. 
        // A mnemonic for them is: the COLon goes on the COLlection side.
        previousValidBs:+validNextA)
  )

// validate V2 becomes a concreate instance of the abtract traverse method
def parseDates2(dates: Vector[String]): Validated[Vector[LocalDate]] = 
  traverse(dates: Vector[String], parseDate)

// read and parse dates V2 provides a clear modelling of two classes of possible error&exception with the return type Try[Validated[Vector[LocalDate]]]
// the failure value of Try type models an exception that stops the program from proceeding, steming from error from reading the file
// the left value of Validated models errors of invalid string to be parsed into date with accumulation capacity 
def readAndParseDates2(filePath: String): Try[Validated[Vector[LocalDate]]] =
  readFromSource2(filePath).map(dateStrings =>
    parseDates2(dateStrings)
  )

// Noting the improvement over read and parse dates V1 where the return type Try[Vector[LocalDate]] cannot accumulate error&exceptions
// beyond the first occurance
readAndParseDates2("./src/main/scala/try_validate/dates-file.txt")

