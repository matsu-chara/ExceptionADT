import DbApiFile2_全部Throwable._

object DbApiFile2_全部Throwable {

  // DataBaseException = DbConnectionException | DbResultException
  sealed abstract class DataBaseException(cause: Throwable) extends RuntimeException(cause)
  case class DbConnectionException(cause: Throwable) extends DataBaseException(cause)
  case class DbResultException(cause: Throwable) extends DataBaseException(cause)

  object DataBase {
    def fetchAll(): Either[DataBaseException, Seq[String]] = {
      Right(Seq("a"))
    }
  }

  // WebApiException = WebApiConnectionException | WebApiTimeoutException
  sealed abstract class WebApiException(cause: Throwable) extends RuntimeException(cause)
  case class WebApiConnectionException(cause: Throwable) extends WebApiException(cause)
  case class WebApiTimeoutException(cause: Throwable) extends WebApiException(cause)

  object WebApi {
    def postAll(data: Seq[String]): Either[WebApiException, Seq[Boolean]] = {
      Right(data.map(_ => true))
    }
  }

}

object Main2 {

  def main(args: Array[String]): Unit = {
    println(fetch())
  }

  // Throwable
  def fetch(): Either[Throwable, Boolean] = for {
    data <- DataBase.fetchAll().right
    results <- WebApi.postAll(data).right
  } yield results.forall(_ == true)
}

