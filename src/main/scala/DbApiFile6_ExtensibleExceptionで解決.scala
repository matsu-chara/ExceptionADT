import DbApiFile6_ExtensibleExceptionで解決._
import utils.:->
import utils.Implicit._

object DbApiFile6_ExtensibleExceptionで解決 {

  // DataBaseException = DbConnectionException | DbResultException
  sealed abstract class DataBaseException(message: String) extends RuntimeException(message)
  case class DbConnectionException(message: String) extends DataBaseException(message)
  case class DbResultException(message: String) extends DataBaseException(message)

  object DataBase {
    def fetchAll(): Either[DataBaseException, Seq[String]] = {
      //      Left(new DbConnectionException("can not connect to host:port"))
      Right(Seq("a"))
    }
  }

  // WebApiException = WebApiConnectionException | WebApiTimeoutException
  sealed abstract class WebApiException(message: String) extends RuntimeException(message)
  case class WebApiConnectionException(message: String) extends WebApiException(message)
  case class WebApiTimeoutException(message: String) extends WebApiException(message)

  object WebApi {
    def postAll(data: Seq[String]): Either[WebApiException, Seq[Boolean]] = {
      Left(WebApiTimeoutException("timeout. 3 sec"))
    }
  }

  // FileException = FileReadException | FileWriteException
  sealed abstract class FileException(message: String) extends RuntimeException(message)
  case class FileReadException(message: String) extends FileException(message)
  case class FileWriteException(message: String) extends FileException(message)

  object File {
    def writeAll(data: Seq[String]): Either[FileException, Seq[Boolean]] = {
      Left(FileWriteException("can not write to hoge.txt"))
    }
  }

  // DbWebException = DataBaseException | WebApiException
  case class DbWebException(cause: Throwable) extends RuntimeException(cause)

  object DbWebException {
    implicit val t1 = new :->[DataBaseException, DbWebException] {
      def cast(a: DataBaseException): DbWebException = DbWebException(a)
    }
    implicit val t2 = new :->[WebApiException, DbWebException] {
      def cast(a: WebApiException): DbWebException = DbWebException(a)
    }
  }

  // DbFileException = DataBaseException | FileException
  case class DbFileException(cause: Throwable) extends RuntimeException(cause)

  object DbFileException {
    implicit val t1 = new :->[DataBaseException, DbFileException] {
      def cast(a: DataBaseException): DbFileException = DbFileException(a) // DataBaseExceptionへの要素追加に影響を受けない
    }
    implicit val t2 = new :->[FileException, DbFileException] {
      def cast(a: FileException): DbFileException = DbFileException(a)
    }
  }

}

object Main6 {
  def main(args: Array[String]): Unit = {
    fetch() match {
      case Right(v) => println(s"result is $v")
      case Left(e: DbWebException) => e.cause match {
        case e: DataBaseException => println(s"DataBaseException: ${e.getMessage}") // connectionExceptionもreultExceptionも拾える
        case e: WebApiTimeoutException => println(s"WebApiTimeoutException: ${e.getMessage}") // ばらして拾うのも可
        case e: WebApiConnectionException => println(s"WebApiConnectionException: ${e.getMessage}") // ばらして拾うのも可
      }
    }

    fetch2() match {
      case Right(v) => println(s"result is $v")
      case Left(e) => e.cause match {
        case FileReadException(message) => println(message)
        case FileWriteException(message) => println(message)
      }
    }
  }

  // DbWebException
  def fetch(): Either[DbWebException, Boolean] = for {
    data <- DataBase.fetchAll().as[DbWebException].right // as[T]のTには思考停止で統合後の例外を書いておけばOK
    results <- WebApi.postAll(data).as[DbWebException].right
  } yield results.forall(_ == true)

  // DbFileException
  def fetch2(): Either[DbFileException, Boolean] = for {
    data <- DataBase.fetchAll().as[DbFileException].right
    results <- File.writeAll(data).as[DbFileException].right
  } yield results.forall(_ == true)
}

