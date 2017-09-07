package scorex.core

import java.net.InetSocketAddress

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

package object settings {
  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = { (config: Config, path: String) =>
    new InetSocketAddress(
      config.as[String](s"$path.address"),
      config.as[Int](s"$path.port")
    )
  }
}
