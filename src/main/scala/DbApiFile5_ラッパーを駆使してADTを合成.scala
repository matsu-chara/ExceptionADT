import DbApiFile5_ラッパーを駆使してADTを合成._

object DbApiFile5_ラッパーを駆使してADTを合成 {
  // DbWebException = DataBaseException | WebApiException
  sealed abstract class DbWebException(cause: Throwable) extends RuntimeException(cause)
  case class DbWebDatabaseException(cause: DataBaseException) extends DbWebException(cause)
  case class DbWebWebApiException(cause: WebApiException) extends DbWebException(cause)

  // DbFileException = DataBaseException | FileException
  sealed abstract class DbFileException(cause: Throwable) extends RuntimeException(cause)
  case class DbFileDatabaseException(cause: DataBaseException) extends DbFileException(cause)
  case class DbFileFileException(cause: FileException) extends DbFileException(cause)

  // DataBaseException = DbConnectionException | DbResultException
  sealed abstract class DataBaseException(message: String) extends RuntimeException(message)
  case class DbConnectionException(message: String) extends DataBaseException(message)
  case class DbResultException(message: String) extends DataBaseException(message)

  object DataBase {
    def fetchAll(): Either[DataBaseException, Seq[String]] = {
      Left(new DbConnectionException("can not connect to host:port"))
    }
  }

  // WebApiException = WebApiConnectionException | WebApiTimeoutException
  sealed abstract class WebApiException(message: String) extends RuntimeException(message)
  case class WebApiConnectionException(message: String) extends WebApiException(message)
  case class WebApiTimeoutException(message: String) extends WebApiException(message)

  object WebApi {
    def postAll(data: Seq[String]): Either[WebApiException, Seq[Boolean]] = {
      Right(data.map(_ => true))
    }
  }

  // FileException = FileReadException | FileWriteException
  sealed abstract class FileException(message: String) extends RuntimeException(message)
  case class FileReadException(message: String) extends FileException(message)
  case class FileWriteException(message: String) extends FileException(message)

  object File {
    def writeAll(data: Seq[String]): Either[FileException, Seq[Boolean]] = {
      Right(data.map(_ => true))
    }
  }

}


object Main5 {
  def main(args: Array[String]): Unit = {
    fetch() match {
      case Right(v) => println(s"result is $v")
      case Left(DbWebDatabaseException(e)) => println(s"DataBaseException: ${e.getMessage}") // 一括でも拾える。
      case Left(DbWebWebApiException(e: WebApiTimeoutException)) => println(s"WebApiTimeoutException: ${e.getMessage}") // バラバラでも拾える（依存の依存が出てくるたびにネストする）
      case Left(DbWebWebApiException(e: WebApiConnectionException)) => println(s"WebApiConnectionException: ${e.getMessage}") // バラバラでも拾える（依存の依存が出てくるたびにネストする）
    }
  }

  // DbWebException
  def fetch(): Either[DbWebException, Boolean] = for {
    data <- DataBase.fetchAll().left.map(DbWebDatabaseException).right
    results <- WebApi.postAll(data).left.map(DbWebWebApiException).right
  } yield results.forall(_ == true)

  // DbFileException
  def fetch2(): Either[DbFileException, Boolean] = for {
    data <- DataBase.fetchAll().left.map(DbFileDatabaseException).right
    results <- File.writeAll(data).left.map(DbFileFileException).right
  } yield results.forall(_ == true)
}
