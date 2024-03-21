// package follow_the_types

import java.time.LocalDate
import java.nio.file.Path

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

// Model a matrix with nested type Vector[Vector[?]]
// where an element of the outer Vector can model a row of a matrix
// and an element of the inner Vector can model a cell of a row
type Matrix[A] = Vector[Vector[A]]
// finds the first row with all even integers, if existing. The conditional 
// nature is modelled by the nested type Option[Seq[?]], which is the return
// type of a find method on collection
// 
def findFirstRowWithEvenValues(rows: Matrix[Int]): Option[Seq[Int]] =
  rows.find(row => row.forall(x => x % 2 == 0))

// an example of nested types leading to application of method
// of the same name but essentially on different types, leading
// to difficulty for reasoning
def increment(maybeXs: Option[Seq[Int]]): Option[Seq[Int]] =
  maybeXs.map(xs => xs.map(x => x + 1))

// an example of working with several nested types leading to undesirable outcome
// the first nested type Try[String] resulting from reading from a Path and returning
// a tentatative result of String, modelled by Try[String]
def read(path: Path): Try[String] =
  Using(Source.fromFile(path.toFile)) { source =>
    source.getLines().mkString("\n")
  }

// the second nested type Try[Seq[Path]] as the function parameters, modelling a tentative Seq[Path]
// leading to a incorrect implementation to naively chain the read method, resulting in the undesirable
// return type Try[Seq[Try[String]]]
def readAllFilesWrong(tentativePaths: Try[Seq[Path]]): Try[Seq[Try[String]]] =
  tentativePaths.map(paths => 
    paths.map(path => 
      read(path)
    )
  )

// For the sake of baselining, here is an implementation of `readAllFiles`
// that does not do any error handling, and does not show in its result type
// that there is a chance of failure.
// On the one hand, the code is simpler, but on the other hand, it does not
// fairly inform the developers of its possibilities of failure. In some cases,
// this is perfectly fine (e.g., a non-critical one-off task), but in other
// cases (e.g., a long-running resilient server), this may not be the best
// solution.
def readAllFilesWithExceptions(paths: Seq[Path]): Seq[String] =
  for path <- paths yield read(path).get  

// Debugging readAllFiles with explicit type ascription
// def readAllFiles(tentativePaths: Try[Seq[Path]]): Try[Seq[String]] = // ascribing the final desired return type
//   tentativePaths.map[Seq[String]](paths => // ascribing the expected type parameter to the map of Try[Seq[Path]]
//     paths.map[String](path => // ascribing the expected type parameter to the map of Seq[Path]
//      // confirming the legitimacy of compilation complaint that the read call returing Try[String] is incompatible with last expected ascribed type 
//      read(path) 
//     )
//   )

// In light of the debugging outcome, also noting that there are two classes of Try failures being modelled in Try[Seq[String]]
// one coming from the possible failure of tentativePaths: Try[Seq[Path]] and another from read(path: Path): Try[String]
// Hence flatMap is proper for first working with tentativePaths: Try[Seq[Path]] given the neccessity that the read call
// will return a Try type of some kind. Within the flatMap which transforms paths: Seq[Path] to Try[Seq[String]], foldLeft on paths
// will be proper as opposed to map or flatMap to return a Try type after iteratively transforming elements in the Seq collection type
def readAllFiles(tentativePaths: Try[Seq[Path]]): Try[Seq[String]] =
  tentativePaths.flatMap[Seq[String]](paths =>
    paths.foldLeft[Try[Seq[String]]](Success(Seq.empty[String]))(
      (previousTriedPaths, currentPath) =>
        previousTriedPaths.flatMap[Seq[String]](previousReadStrings =>
          read(currentPath).map[Seq[String]](currentReadString =>
            previousReadStrings :+ currentReadString
          )
        )
    )
  )

// Implementation of `readAllFiles`, using a functional programming style
// for the loop within `foldLeft` on the sequence, in place of the flatMap & map construct
// Noting the improvement of readability
// Drawback of the foldLeft approach to note is that the required implementation can stop proceeding if any
// Tried operation returns an exception. But the inherent behaviour of foldLeft on a collection loops through
// all elements of the collection regardless, leading to suboptimal efficiency
def readAllFilesFunctionally(tentativePaths: Try[Seq[Path]]): Try[Seq[String]] =
  tentativePaths.flatMap { paths =>
    paths.foldLeft[Try[Seq[String]]](Success(Vector.empty[String])) {
      (tentativePreviousTexts, path) =>
        for
          previousTexts <- tentativePreviousTexts
          text          <- read(path)
        yield previousTexts :+ text
    }
  }

// Another implementation of `readAllFiles` using a functional programming style,but the loop is 
// implemented by a recursive method `tryReadPaths` on the data structure backed by an immutable linked list
// Noting the difference with the `foldLeft` approach is that the loop is terminated on the
// occurance of failure of a tried read of an element of the list, enabled by the conditional
// capacity to only proceed recursion for success of a tried read
def readAllFilesFunctionally2(tentativePaths: Try[List[Path]]): Try[Seq[String]] =
  def tryReadPaths(paths: List[Path], previouslyReadTexts: Vector[String]): Try[Seq[String]] =
    paths match
      case Nil => Success(previouslyReadTexts)
      case path :: remainingPaths =>
        read(path) match
          case Success(text) =>
            tryReadPaths(remainingPaths, previouslyReadTexts :+ text)
          case Failure(error) => Failure(error)
  tentativePaths.flatMap[Seq[String]](paths => tryReadPaths(paths, Vector.empty))

// The last implementation of `readAllFiles`, this time with an “imperative-style” loop
// Noting its ability to terminate loop on failure of a tried read
def readAllFilesImperatively(tentativePaths: Try[Seq[Path]]): Try[Seq[String]] =
  tentativePaths.flatMap { paths =>
    val texts = Seq.newBuilder[String]
    var maybeFailure = Option.empty[Failure[Seq[String]]]
    val it = paths.iterator
    while it.hasNext && maybeFailure.isEmpty do
      read(it.next()) match
        case Success(text)  => texts += text
        case Failure(error) => maybeFailure = Some(Failure(error))
    maybeFailure match
      case Some(failure) => failure
      case None          => Success(texts.result())
  }  

// Demonstration of the general applicability of the `fold` approach for dealing with other nested container types
def readOptionalPath(optionalPath: Option[Path]): Try[Option[String]] =
  optionalPath.fold[Try[Option[String]]](Success(None))(somePath =>
    read(somePath).map[Option[String]](readString => Some(readString))
  )

def readSomethingOrPath[A](somethingOrPath: Either[A, Path]): Try[Either[A, String]] =
  somethingOrPath.fold[Try[Either[A, String]]]((a: A) => Success(Left(a)), 
    (path: Path) => read(path).map[Either[A, String]](readString => 
      Right(readString)
    )
  )

// Would-be equivalent implementations for comparison
def readOptionalFile(maybePath: Option[Path]): Try[Option[String]] =
  maybePath.fold(Success(None))(path => for text <- read(path) yield Some(text))

def readOptionalFile2(maybePath: Option[Path]): Try[Option[String]] =
  maybePath match
    case None       => Success(None)
    case Some(path) => for text <- read(path) yield Some(text)

def readSomethingOrPath2[A](aOrPath: Either[A, Path]): Try[Either[A, String]] =
  aOrPath.fold(
    a  => Success(Left(a)),
    path => for text <- read(path) yield Right(text)
  )

def readSomethingOrPath3[A](aOrPath: Either[A, Path]): Try[Either[A, String]] =
  aOrPath match
    case Left(a)   => Success(Left(a))
    case Right(path) => for text <- read(path) yield Right(text)

type Errors = Seq[String]
type Validated[A] = Either[Errors, A]  

// Given a nested type Try[Validated[A]] that is frequently used in the program, it is beneficial to create
// a dedicated abstraction, in the following case being a case class wrapper
case class MyResult[A](tentativeValidatedValue: Try[Validated[A]]):

  // Commonly used high-level functions e.g. map, flatMap are reimplemented for the abstracted type
  def map[B](f: A => B): MyResult[B] =
    MyResult(tentativeValidatedValue.map[Validated[B]](validatedValue => 
      validatedValue.map[B](f))
    )

  def flatMap[B](f: A => MyResult[B]): MyResult[B] =
    val tentativeValidatedB: Try[Validated[B]] =
      tentativeValidatedValue.flatMap[Validated[B]] {
        case Right(value) => f(value).tentativeValidatedValue
        case Left(errors) => Success(Left(errors))
      }
    MyResult(tentativeValidatedB)
  
  // Implementation attempts
  def recoverException(handler: PartialFunction[Throwable, A]): MyResult[A] =
    val recoveredTry = tentativeValidatedValue.recover[Validated[A]](
      // doubt: handler pf turns the Throwable value into an value of type A. 
      // then value of that A is wrapped straight away in Right(), assuming
      // it valid. Is such assumption sound?
      handler.andThen[Validated[A]]((a: A) => 
        Right(a)
      )
    )
    MyResult(recoveredTry)
    
  
  def recoverValidatedErrors(handler: Errors => A): MyResult[A] =
    val recoveredValidatedValue: Try[Validated[A]] = tentativeValidatedValue.map[Validated[A]] {
      case Right(value) => Right(value)
      case Left(error) => Right(handler(error))
    }
    MyResult(recoveredValidatedValue)

end MyResult

object MyResult:
  // Also benefitial to define various construction methods to create the abstracted type from various input types
  def successfulValid[A](value: A): MyResult[A] =
    MyResult(Success(Right(value)))
  def successfulInvalid[A](errors: Errors): MyResult[A] =
    MyResult(Success(Left(errors)))
  def successfulValidated[A](validatedValue: Validated[A]): MyResult[A] =
    MyResult(Success(validatedValue))
  def failed[A](throwable: Throwable): MyResult[A] =
    MyResult(Failure(throwable))
end MyResult

// Examplary comparison of employing abstracted type vs handling nested types in the original form
def incrementDates(dates: Seq[LocalDate]): Seq[LocalDate] =
  dates.map(date => date.plusDays(1))

def incrementTentativeValidatedDates(
  tentativeValidatedDates: Try[Validated[Seq[LocalDate]]]
): Try[Validated[Seq[LocalDate]]] =
  tentativeValidatedDates
    .map(validatedDates => validatedDates.map(dates => incrementDates(dates)))

// Same as `incrementTentativeValidatedDates`, but with abstracted `MyResult` instead
def incrementDatesResult(datesResult: MyResult[Seq[LocalDate]]) =
  datesResult.map(dates => incrementDates(dates))