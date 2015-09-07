import DbApiFile6_Coproductでの解決._
import shapeless._

object DbApiFile6_Coproductでの解決 {

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
  type DbWebException = DataBaseException :+: WebApiException :+: CNil

  // DbFileException = DataBaseException | FileException
  type DbFileException = DataBaseException :+: FileException :+: CNil

}

object Main6 {
  object DbWebHandler extends Poly1 {
    implicit def caseDataBaseException = at[DataBaseException](e => println(s"DataBaseException: ${e.getMessage}"))
    implicit def caseWebApiException = at[WebApiException] {
      case e: WebApiTimeoutException => println(s"WebApiTimeoutException: ${e.getMessage}")
      case e: WebApiConnectionException => println(s"WebApiConnectionException: ${e.getMessage}")
    }
  }

  object DbFileHandler extends Poly1 {
    implicit def caseDataBaseException = at[DataBaseException](e => println(s"DataBaseException: ${e.getMessage}"))
    implicit def caseFileApiException = at[FileException] {
      case e: FileReadException => println(s"FileReadException: ${e.getMessage}")
      case e: FileWriteException => println(s"FileWriteException: ${e.getMessage}")
    }
  }


  def main(args: Array[String]): Unit = {
    // コンパイル速度が遅い、コンパイルエラーメッセージがわかりにくいなどのデメリットあり
    fetch() match {
      case Right(v) => println(s"result is $v")
      case Left(e: DbWebException) => e map DbWebHandler
    }

    fetch2() match {
      case Right(v) => println(s"result is $v")
      case Left(e: DbFileException) => e map DbFileHandler
    }
  }

  // DbWebException
  def fetch(): Either[DbWebException, Boolean] = for {
    data <- DataBase.fetchAll().left.map(Coproduct[DbWebException](_)).right
    results <- WebApi.postAll(data).left.map(Coproduct[DbWebException](_)).right
  } yield results.forall(_ == true)

  // DbFileException
  def fetch2(): Either[DbFileException, Boolean] = for {
    data <- DataBase.fetchAll().left.map(Coproduct[DbFileException](_)).right
    results <- File.writeAll(data).left.map(Coproduct[DbFileException](_)).right
  } yield results.forall(_ == true)

}

