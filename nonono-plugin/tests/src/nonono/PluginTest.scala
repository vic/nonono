package nonono

import utest.*

import scala.concurrent.Future

object PluginTest extends TestSuite {

  NoNoNo[Option[Any]](_.get)("Prefer Option.getOrElse")
  val x = Option(22).get

  NoNoNo[Iterable[Number]](_.head)("Use match or headOption")
  val y = Seq(1, 2, 3).head

//  NoNoNo[Option[Any]](_.fold[String](???)(???))("bla")
//  NoNoNo[Future[Any]](_.ready(???)(???))("WOOOT")

  override def tests: Tests = Tests {
    test("foo") { true }
  }

}
