package scorex.core.network.peer

import java.net.InetSocketAddress

import org.mapdb.{DBMaker, HTreeMap, Serializer}
import scorex.core.settings.Settings

import scala.collection.JavaConverters._

class PeerDatabaseImpl(settings: Settings, filename: Option[String]) extends PeerDatabase {

  val database = filename match {
    case Some(file) => DBMaker.fileDB(file).make()
    case None => DBMaker.memoryDB().make()
  }

  private val whitelistPersistence = database.hashMap("whitelist", Serializer.JAVA, Serializer.JAVA).createOrOpen()
    .asInstanceOf[HTreeMap[InetSocketAddress, PeerInfo]]

  private val blacklist = database.hashMap("blacklist", Serializer.STRING, Serializer.LONG).createOrOpen()

  private lazy val ownNonce = settings.nodeNonce

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo = Option(whitelistPersistence.get(address)).map { dbPeerInfo =>
      val nonceOpt = peerInfo.nonce.orElse(dbPeerInfo.nonce)
      val nodeNameOpt = peerInfo.nodeName.orElse(dbPeerInfo.nodeName)
      PeerInfo(peerInfo.lastSeen, nonceOpt, nodeNameOpt)
    }.getOrElse(peerInfo)
    whitelistPersistence.put(address, updatedPeerInfo)
    database.commit()
  }

  override def blacklistPeer(address: InetSocketAddress): Unit = {
    whitelistPersistence.remove(address)
    if (!isBlacklisted(address)) blacklist.asScala += address.getHostName -> System.currentTimeMillis()
    database.commit()
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.asScala.contains(address.getHostName))
  }

  override def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo] =
    (excludeSelf match {
      case true => knownPeers(false).filter(_._2.nonce.getOrElse(-1) != ownNonce)
      case false => whitelistPersistence.asScala.keys.flatMap(k => Option(whitelistPersistence.get(k)).map(v => k -> v))
    }).toMap

  override def blacklistedPeers(): Seq[String] = blacklist.asScala.keys.toSeq

  override def isEmpty(): Boolean = whitelistPersistence.size() == 0
}
