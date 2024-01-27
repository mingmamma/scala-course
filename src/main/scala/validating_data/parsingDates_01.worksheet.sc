import java.time.{LocalDate, Period}

import scala.util.Try

type Errors = Seq[String]

type Validated[A] = Either[Errors, A]

def parseDate(str: String): Validated[LocalDate] =
  // Use the LocalDate parse() method which accepts CharSequence/String arguement
  // It is wrapped in a Try since
  // In the successful case, it returns a LocalDate instance
  // Othwise, it is possible to throw a DateTimeParseException as documented
  // which is a non-fatal exception admissable by the Try type
  // https://www.scala-lang.org/api/current/scala/util/Try.html
  // LocalDate.parse(str) is supplied as a by-name parameter to the Try call:
  // https://docs.scala-lang.org/tour/by-name-parameters.html#inner-main
  Try.apply(LocalDate.parse(str))
    // Convinient helper to convert a Try[T] to Either[Throwable, T]
    .toEither
    // Takes the Throwable from the left to create a Seq with
    // single element of the error message (as in String)
    // Hence the total return is Either[Seq[String], LocalDate]
    .left.map(error => Seq(error.getMessage))

// Validate V1
def validatePeriod(str1: String, str2: String): Validated[Period] =
  // Both flatMap and map will keep the Left unchanged
  // Thus if the first parseDate(str1) errors out, the final return would be: Left(Seq(firstErrorMsgString))
  // i.e. Errors are NOT accumulated if BOTH ops error out
  parseDate(str1).flatMap(date1 =>
    parseDate(str2).map(date2 =>
      // the date1 and date2 variables will be passed down
      // ONLY IF the previous parseDate calls were succesful 
      // s.t. they are LocalDate instances
      Period.between(date1, date2)
    )
  )

validatePeriod("not a date first", "not a date second")
validatePeriod("2024-01-26", "not a date second")
// An odd but valid object entailing a "negative" period
validatePeriod("2024-01-26", "2024-01-25")

// Validate V2, use pattern matching to enable to process two GENERIC Either results s.t.
// if BOTH results are Lefts, the final return is a Left containing BOTH error
def validateBoth[A, B](
  validatedA: Validated[A], 
  validatedB: Validated[B]
  ): Validated[(A, B)] = // Validated[(A, B)] entails Right(a: A, b: B) OR Left[Seq[String]], complied by all following cases
  (validatedA, validatedB) match 
    case (Right(a), Right(b)) => Right((a, b)) 
    case (Left(e),  Right(_)) => Left(e)
    case (Right(_), Left(e))  => Left(e)
    case (Left(e1), Left(e2)) => Left(e1 ++ e2)


// In the following case, a Collection of Strings are to be parsed. The final outcomes can be two possiblities
// depending on how many Strings in the Collection error out upon parsing
// 1. A Collection[LocalDate] instance is expected for all-successful-passes
// 2. Otherwise, a Collection[String] instance is expected, and the Collection is supposed to accumulate the ErrorMsgString
// from all the original input Strings that fail parsing
// Hence the return of such result SHOULD be modelled as Either[Seq[String], Collection[LocalDate]], or equivalently Validated[Collection[LocalDate]]

// This element-wise collection processing ops is best implemented by FoldLeft(), which is available at the generic Iterable trait
// The validateBoth supplies the case handling logic for combining the existing partial result AND the parsing of the upcoming element for various scenarios:
// previous result: Validated[List[LocalDate]] <- can be all the previously successfully parsed dates OR all the previously accumulated ErrorMsgStrings
// upcoming result: Validated[LocalDate] <- a Right(LocalDate) OR a Left[Seq[ErrorString]]

// Using a List as the input collection would be awkward when coupled with the foldLeft use case here
// since each processed item needs to be combined back in the APPEND fashion, which is undesirable performance-wise
def parseDates(dates: Vector[String]): Validated[Vector[LocalDate]] =
  // the type annotation of the foldLeft is neccessary in this case for the 
  // compiler to correctly infer the types of the input elements as the second
  // argument of the foldLeft call where the binary combination op is defined
  dates.foldLeft[Validated[Vector[LocalDate]]]
    (Right(Vector.empty)) // Right(Vector.empty[LocalDate]) as the would-be return of the base case where a Vector.empty[String] is the input start value
    ((validatedDates, nextDateString) =>
      val nextParsed = parseDate(nextDateString)
      // if any of the previousValidDates or parsedDate is Left, the 3 branches of validateBoth
      // involving Left get reused s.t. the ErrorMsgString are accumulated
      validateBoth(validatedDates, nextParsed).map((previousValidDates, parsedDate) =>
        // if both the previous elements are right, just need to restructure the Tuple
        // from the original Right result from validateBoth call into a Vector
        // making use of the property that map call of Either only operate Right result
        previousValidDates.appended(parsedDate))
    )

// Parses all valid input String into a Vec of LocalDate 
parseDates(Vector("2020-01-04", "2020-08-09", "2023-11-11"))
// Return a Vec of all ErrorMsgStrings ever encountered during parsing
parseDates(Vector("first unparsabble text", "2020-09-13", "second unparsable text"))    
  
