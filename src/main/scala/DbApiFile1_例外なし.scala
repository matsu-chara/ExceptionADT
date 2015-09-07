import DbApiFile1_例外なし._

object DbApiFile1_例外なし {

  object DataBase {
    def fetchAll(): Either[Throwable, Seq[String]] = {
      Right(Seq("a"))
    }
  }

  object WebApi {
    def postAll(data: Seq[String]): Either[Throwable, Seq[Boolean]] = {
      Right(data.map(_ => true))
    }
  }

}

object Main1 {

  def main(args: Array[String]): Unit = {
    println(fetch())
  }

  def fetch(): Either[Throwable, Boolean] = for {
    data <- DataBase.fetchAll().right
    results <- WebApi.postAll(data).right
  } yield results.forall(_ == true)
}
