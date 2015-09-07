import DbApiFile3_下位モジュールの例外にむりやり組み込む._

object DbApiFile3_下位モジュールの例外にむりやり組み込む {
  // DbWebException = DataBaseException | WebApiException
  trait DbWebException

  // DataBaseException = DbConnectionException | DbResultException
  sealed abstract class DataBaseException(message: String) extends RuntimeException(message) with DbWebException
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
}

object Main3 {
  def main(args: Array[String]): Unit = {
    println(fetch())
  }

  // DataBaseException
  def fetch(): Either[DbWebException, Boolean] = for {
    data <- DataBase.fetchAll().right
    results <- WebApi.postAll(data).right
  } yield results.forall(_ == true)
}


