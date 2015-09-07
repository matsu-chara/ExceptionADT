import DbApiFile4_下位モジュールの例外に無理やり組み込んで破綻._

object DbApiFile4_下位モジュールの例外に無理やり組み込んで破綻 {
  // DbWebException = DataBaseException | WebApiException
  trait DbWebException

  // DbFileException = DataBaseException | FileException
  trait DbFileException


  // DataBaseException = DbConnectionException | DbResultException
  sealed abstract class DataBaseException(message: String) extends RuntimeException(message) with DbWebException with DbFileException
  case class DbConnectionException(message: String) extends DataBaseException(message)
  case class DbResultException(message: String) extends DataBaseException(message)

  object DataBase {
    def fetchAll(): Either[DataBaseException, Seq[String]] = {
      Left(new DbConnectionException("can not connect to host:port"))
    }
  }

  // WebApiException = WebApiConnectionException | WebApiTimeoutException
  sealed abstract class WebApiException(message: String) extends RuntimeException(message) with DbWebException
  case class WebApiConnectionException(message: String) extends WebApiException(message)
  case class WebApiTimeoutException(message: String) extends WebApiException(message)

  object WebApi {
    def postAll(data: Seq[String]): Either[WebApiException, Seq[Boolean]] = {
      Right(data.map(_ => true))
    }
  }

  // FileException = FileReadException | FileWriteException
  sealed abstract class FileException(message: String) extends RuntimeException(message) with DbFileException
  case class FileReadException(message: String) extends FileException(message)
  case class FileWriteException(message: String) extends FileException(message)

  object File {
    def writeAll(data: Seq[String]): Either[FileException, Seq[Boolean]] = {
      Right(data.map(_ => true))
    }
  }

}

object Main4 {
  def main(args: Array[String]): Unit = {
    println(fetch())
  }

  // DbWebException
  def fetch(): Either[DbWebException, Boolean] = for {
    data <- DataBase.fetchAll().right
    results <- WebApi.postAll(data).right
  } yield results.forall(_ == true)

  // DbFileException
  def fetch2(): Either[DbFileException, Boolean] = for {
    data <- DataBase.fetchAll().right
    results <- File.writeAll(data).right
  } yield results.forall(_ == true)
}
