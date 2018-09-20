package scorex.core.network

import java.net.{InetAddress, InetSocketAddress}

import org.bitlet.weupnp.{GatewayDevice, GatewayDiscover, PortMappingEntry}
import scorex.core.settings.NetworkSettings
import scorex.util.ScorexLogging

import scala.collection.JavaConverters._

trait UPnPGateway {
  def localAddress: InetAddress
  def externalAddress: InetAddress
  def addPort(port: Int): Unit
  def deletePort(port: Int): Unit
  def getLocalAddressForExternalPort(extrenalPort: Int):Option[InetSocketAddress]
}

object UPnP extends ScorexLogging {
  def getValidGateway(settings: NetworkSettings): Option[UPnPGateway] = {
    try {
      log.info("Looking for UPnP gateway device...")
      val defaultHttpReadTimeout = settings.upnpGatewayTimeout.map(_.toMillis.toInt).getOrElse(GatewayDevice.getHttpReadTimeout)
      GatewayDevice.setHttpReadTimeout(defaultHttpReadTimeout)
      val discover = new GatewayDiscover()
      val defaultDiscoverTimeout = settings.upnpDiscoverTimeout.map(_.toMillis.toInt).getOrElse(discover.getTimeout)
      discover.setTimeout(defaultDiscoverTimeout)

      val gatewayMap = Option(discover.discover).map(_.asScala).map(_.toMap).getOrElse(Map())
      if (gatewayMap.isEmpty) {
        log.debug("There are no UPnP gateway devices")
        None
      } else {
        gatewayMap.foreach { case (addr, _) =>
          log.debug("UPnP gateway device found on " + addr.getHostAddress)
        }
        val gateway = Option(discover.getValidGateway)
        if (gateway.isEmpty) {
          log.debug("There is no connected UPnP gateway device")
        }
        gateway.map(new UPnPGatewayImpl(_))
      }
    } catch { case t: Throwable =>
      log.error("Unable to discover UPnP gateway devices", t)
      None
    }
  }
}

class UPnPGatewayImpl(gateway: GatewayDevice) extends UPnPGateway with ScorexLogging {

  override val localAddress: InetAddress = gateway.getLocalAddress
  override val externalAddress: InetAddress = InetAddress.getByName(gateway.getExternalIPAddress)

  log.debug("Using UPnP gateway device on " + localAddress.getHostAddress)
  log.info("External IP address is " + externalAddress.getHostAddress)


  override def addPort(port: Int): Unit = {
    try {
      if (gateway.addPortMapping(port, port, localAddress.getHostAddress, "TCP", "Scorex")) {
        log.debug("Mapped port [" + externalAddress.getHostAddress + "]:" + port)
      } else {
        log.debug("Unable to map port " + port)
      }
    } catch {
      case t: Throwable =>
        log.error("Unable to map port " + port + ": " + t.toString)
    }
  }

  override def deletePort(port: Int): Unit = {
    try {
      if (gateway.deletePortMapping(port, "TCP")) {
        log.debug("Mapping deleted for port " + port)
      } else {
        log.debug("Unable to delete mapping for port " + port)
      }
    } catch {
      case t: Throwable =>
        log.error("Unable to delete mapping for port " + port + ": " + t.toString)
    }
  }

  override def getLocalAddressForExternalPort(externalPort: Int):Option[InetSocketAddress] = {
    try {
      val entry = new PortMappingEntry
      if (gateway.getSpecificPortMappingEntry(externalPort, "TCP", entry)) {
        val host = entry.getInternalClient
        Some(new InetSocketAddress(InetAddress.getByName(host), entry.getInternalPort))
      } else {
        None
      }
    } catch {
      case t: Throwable =>
        log.error("Unable to get local address for external port " + externalPort + ": " + t.toString)
        None
    }
  }
}
