import DbApiFile8_ExtensibleExceptionの実験._
import utils.{Transform, :->}
import utils.Implicit._

object DbApiFile8_ExtensibleExceptionの実験 {

  // DataBaseException = DbConnectionException | DbResultException
  sealed abstract class DataBaseException(message: String) extends RuntimeException(message)
  case class DbConnectionException(message: String) extends DataBaseException(message)
  case class DbResultException(message: String) extends DataBaseException(message)

  object DataBase {
    def fetchAll(): Either[DataBaseException, Seq[String]] = {
      Left(DbConnectionException("can not connect to host:port"))
      // Right(Seq("a"))
    }

    def fetchAllWithAuth(): Either[DbAuthException, Seq[String]] = {
      Left(DbPasswordIncorrectException("can not connect to host:port password is incorrect"))
      //Right(Seq("a"))
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

  // サブタイプで関連付けされたDbWebExceptionから見て２階層下のADT
  abstract class DbAuthException(message: String) extends DataBaseException(message)
  case class DbUserNotExistsException(message: String) extends DbAuthException(message)
  case class DbPasswordIncorrectException(message: String) extends DbAuthException(message)

  // (DbExceptionが)型クラスで関連付けされたDbWebFileExceptionから見て２階層下のADTになる
  case class DbWebFileException(cause: Throwable) extends RuntimeException(cause)
  object DbWebFileException {
    implicit val t1 = Transform.castable[DbWebException, DbWebFileException]
    implicit val t2 = Transform.castable[FileException, DbWebFileException]
  }
}

object Main8 {
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
        case e: DbWebException => println(s"DataBaseException: ${e.getMessage}")
        case FileReadException(message) => println(message)
        case FileWriteException(message) => println(message)
      }
    }
  }

  // DbWebException
  def fetch(): Either[DbWebException, Boolean] = for {
    data <- DataBase.fetchAllWithAuth().as[DbWebException].right // as[T]のTには思考停止で統合後の例外を書いておけばOK
    results <- WebApi.postAll(data).as[DbWebException].right
  } yield results.forall(_ == true)

  // DbWebFileException
  def fetch2(): Either[DbWebFileException, Boolean] = {
    implicit val x = DbWebException.t1 // implicitパラメータの解決に必要
    for {
      data <- DataBase.fetchAll().as[DbWebFileException].right
      results <- File.writeAll(data).as[DbWebFileException].right
    } yield true //results.forall(_ == true)
  }
}

