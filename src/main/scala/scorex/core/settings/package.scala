package scorex.core

import java.io.File
import java.net.InetSocketAddress

import com.typesafe.config.Config
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}
import net.ceedubs.ficus.readers.namemappers.HyphenNameMapper
import scorex.core.utils.ByteStr

package object settings {
  implicit val hyphenCase: NameMapper = HyphenNameMapper
  implicit val byteStrReader: ValueReader[ByteStr] = (cfg, path) => ByteStr.decodeBase58(cfg.getString(path)).get
  implicit val fileReader: ValueReader[File] = (cfg, path) => new File(cfg.getString(path))
  implicit val byteValueReader: ValueReader[Byte] = (cfg, path) => cfg.getInt(path).toByte
  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = { (config: Config, path: String) =>
    val splitted = config.getString(path).split(":")
    new InetSocketAddress(
      splitted(0),
      splitted(1).toInt
    )
  }
}
