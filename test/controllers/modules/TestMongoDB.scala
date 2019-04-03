package controllers.modules

import com.mongodb.casbah.MongoConnection
import it.gov.daf.catalogmanager.catalog.MongoRepository
import javax.inject.Inject
import play.api.Configuration
import play.api.libs.ws.WSClient
import scala.concurrent.Future

class TestMongoDB @Inject()(implicit val configuration: Configuration) extends MongoRepository {

  val mongoConn = MongoConnection()
  val mongoDB = mongoConn("testDB")

  override val collection = mongoDB("testColl")

  override def canDeleteCatalog(isSysAdmin: Boolean, name: String, token: String, wsClient: WSClient, user: String): Future[Boolean] =
    name match{
      case "fake_token" => Future.successful(false)
      case _            => Future.successful(true)
    }
}
