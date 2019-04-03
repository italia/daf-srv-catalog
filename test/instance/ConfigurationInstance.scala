package instance

import com.typesafe.config.ConfigFactory
import play.api.Configuration

trait ConfigurationInstance {

  protected val configFile = "test"
  implicit val configurationTest: Configuration = Configuration {
    ConfigFactory.load()
  }

}
