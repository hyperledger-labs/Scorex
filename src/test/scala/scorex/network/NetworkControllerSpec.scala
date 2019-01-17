package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorSystem}
import akka.io.Tcp
import akka.io.Tcp.{Message => _, _}
import akka.io.Tcp.SO.KeepAlive
import akka.testkit.TestProbe
import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.network.peer.{LocalAddressPeerFeature, LocalAddressPeerFeatureSerializer, PeerManagerRef}
import scorex.core.settings.{NetworkSettings, ScorexSettings}
import scorex.core.utils.LocalTimeProvider
import org.scalatest.TryValues._
import org.scalatest.OptionValues._
import org.scalatest.EitherValues._
import scorex.core.app.{ScorexContext, Version}

import scala.concurrent.ExecutionContext
import scala.util.Try

class NetworkControllerSpec extends FlatSpec with Matchers {

  import scala.concurrent.ExecutionContext.Implicits.global
  private val settings = ScorexSettings.read(None)

  "A NetworkController" should "send local address on handshake when peer and node address are in localhost" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()
    val networkControllerRef: ActorRef = createNetworkController(settings, tcpManagerProbe)
    val testPeer = new TestPeer(settings, networkControllerRef, tcpManagerProbe)

    val peerAddr = new InetSocketAddress("127.0.0.1", 5678)
    val nodeAddr = new InetSocketAddress("127.0.0.1", settings.network.bindAddress.getPort)
    testPeer.connect(peerAddr, nodeAddr)

    val handshakeFromNode  = testPeer.receiveHandshake
    handshakeFromNode.success.value.declaredAddress shouldBe empty
    val localAddressFeature = extractLocalAddrFeat(handshakeFromNode)
    localAddressFeature.value.address should be (nodeAddr)

    system.terminate()
  }

  it should "send local address on handshake when the peer are in local network" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()
    val networkControllerRef: ActorRef = createNetworkController(settings, tcpManagerProbe)
    val testPeer = new TestPeer(settings, networkControllerRef, tcpManagerProbe)

    val nodeAddr = new InetSocketAddress("127.0.0.1", settings.network.bindAddress.getPort)
    testPeer.connect(new InetSocketAddress("192.168.0.1", 5678), nodeAddr)

    val handshakeFromNode  = testPeer.receiveHandshake
    handshakeFromNode.success.value.declaredAddress shouldBe empty
    val localAddressFeature = extractLocalAddrFeat(handshakeFromNode)
    localAddressFeature.value.address should be (nodeAddr)

    system.terminate()
  }

  it should "not send local address on handshake when the peer is external" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()
    val networkControllerRef: ActorRef = createNetworkController(settings, tcpManagerProbe)
    val testPeer = new TestPeer(settings, networkControllerRef, tcpManagerProbe)

    val nodeAddr = new InetSocketAddress("127.0.0.1", settings.network.bindAddress.getPort)
    testPeer.connect(new InetSocketAddress("88.77.66.55", 5678), nodeAddr)

    val handshakeFromNode  = testPeer.receiveHandshake
    handshakeFromNode.success.value.declaredAddress shouldBe empty
    val localAddressFeature = extractLocalAddrFeat(handshakeFromNode)
    localAddressFeature shouldBe empty

    testPeer.sendHandshake(None, None)
    system.terminate()
  }

  it should "send declared address when node and peer are public" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()

    val bindAddress = new InetSocketAddress("88.77.66.55", 12345)
    val settings2 = settings.copy(network = settings.network.copy(bindAddress = bindAddress))
    val networkControllerRef: ActorRef = createNetworkController(settings2, tcpManagerProbe)
    val testPeer = new TestPeer(settings2, networkControllerRef, tcpManagerProbe)

    testPeer.connect(new InetSocketAddress("88.77.66.55", 5678), bindAddress)

    val handshakeFromNode  = testPeer.receiveHandshake
    handshakeFromNode.success.value.declaredAddress.value should be (bindAddress)
    val localAddressFeature = extractLocalAddrFeat(handshakeFromNode)
    localAddressFeature shouldBe empty

    testPeer.sendHandshake(None, None)
    system.terminate()
  }

  it should "send known public peers" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()

    val nodeAddr = new InetSocketAddress("88.77.66.55", 12345)
    val settings2 = settings.copy(network = settings.network.copy(bindAddress = nodeAddr))
    val networkControllerRef: ActorRef = createNetworkController(settings2, tcpManagerProbe)
    val testPeer1 = new TestPeer(settings2, networkControllerRef, tcpManagerProbe)
    val peer1Addr = new InetSocketAddress("88.77.66.55", 5678)
    testPeer1.connect(peer1Addr, nodeAddr)
    testPeer1.receiveHandshake
    testPeer1.sendHandshake(Some(peer1Addr), None)
    testPeer1.receiveGetPeers
    testPeer1.sendPeers(Seq.empty)

    val peer2Addr = new InetSocketAddress("88.77.66.56", 5678)
    val testPeer2 = new TestPeer(settings2, networkControllerRef, tcpManagerProbe)
    testPeer2.connect(peer2Addr, nodeAddr)
    testPeer2.receiveHandshake
    testPeer2.sendHandshake(Some(peer2Addr), None)

    testPeer1.sendGetPeers
    testPeer1.receivePeers should contain theSameElementsAs Seq(peer1Addr, peer2Addr)

    system.terminate()
  }

  it should "send known local peers" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()

    val nodeAddr = new InetSocketAddress("88.77.66.55", 12345)
    val settings2 = settings.copy(network = settings.network.copy(bindAddress = nodeAddr))
    val networkControllerRef: ActorRef = createNetworkController(settings2, tcpManagerProbe)

    val testPeer1 = new TestPeer(settings2, networkControllerRef, tcpManagerProbe)
    val peer1DecalredAddr = new InetSocketAddress("88.77.66.55", 5678)
    val peer1LocalAddr = new InetSocketAddress("192.168.1.55", 5678)
    testPeer1.connect(peer1LocalAddr, nodeAddr)
    testPeer1.receiveHandshake
    testPeer1.sendHandshake(Some(peer1DecalredAddr), Some(peer1LocalAddr))
    testPeer1.receiveGetPeers
    testPeer1.sendPeers(Seq.empty)

    val peer2DeclaredAddr = new InetSocketAddress("88.77.66.56", 5678)
    val peer2LocalAddr = new InetSocketAddress("192.168.1.56", 5678)
    val testPeer2 = new TestPeer(settings2, networkControllerRef, tcpManagerProbe)
    testPeer2.connect(peer2LocalAddr, nodeAddr)
    testPeer2.receiveHandshake
    testPeer2.sendHandshake(Some(peer2DeclaredAddr), Some(peer2LocalAddr))

    testPeer1.sendGetPeers
    testPeer1.receivePeers should contain theSameElementsAs Seq(peer1LocalAddr, peer2LocalAddr)

    system.terminate()
  }

  it should "not send known local address of peer when node is not in local network" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()

    val nodeAddr = new InetSocketAddress("88.77.66.55", 12345)
    val settings2 = settings.copy(network = settings.network.copy(bindAddress = nodeAddr))
    val networkControllerRef: ActorRef = createNetworkController(settings2, tcpManagerProbe)

    val testPeer1 = new TestPeer(settings2, networkControllerRef, tcpManagerProbe)
    val peer1DecalredAddr = new InetSocketAddress("88.77.66.55", 5678)
    val peer1LocalAddr = new InetSocketAddress("192.168.1.55", 5678)
    testPeer1.connect(peer1LocalAddr, nodeAddr)
    testPeer1.receiveHandshake
    testPeer1.sendHandshake(Some(peer1DecalredAddr), Some(peer1LocalAddr))
    testPeer1.receiveGetPeers
    testPeer1.sendPeers(Seq.empty)

    val peer2DeclaredAddr = new InetSocketAddress("88.77.66.56", 5678)
    val testPeer2 = new TestPeer(settings2, networkControllerRef, tcpManagerProbe)
    testPeer2.connect(peer2DeclaredAddr, nodeAddr)
    testPeer2.receiveHandshake
    testPeer2.sendHandshake(Some(peer2DeclaredAddr), None)

    testPeer2.sendGetPeers
    testPeer2.receivePeers should not contain peer1LocalAddr

    system.terminate()
  }


  it should "receive external address from UPnP gateway" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()

    val gatewayAddr = InetAddress.getByName("88.44.33.11")
    val upnp = DummyUPnPGateway(
      gatewayAddr,
      InetAddress.getByName("192.169.1.11")
    ) { _ => None }

    val nodeAddr = new InetSocketAddress(gatewayAddr, settings.network.bindAddress.getPort)
    val networkControllerRef: ActorRef = createNetworkController(settings, tcpManagerProbe, Some(upnp))

    val testPeer1 = new TestPeer(settings, networkControllerRef, tcpManagerProbe)
    val peer1DecalredAddr = new InetSocketAddress("88.77.66.55", 5678)
    val peer1LocalAddr = new InetSocketAddress("192.168.1.55", 5678)
    testPeer1.connect(peer1LocalAddr, nodeAddr)
    val handshakeFromPeer1 = testPeer1.receiveHandshake.success.value
    handshakeFromPeer1.declaredAddress.value.getAddress should be (InetAddress.getByName("88.44.33.11"))
    handshakeFromPeer1.declaredAddress.value.getPort should be (settings.network.bindAddress.getPort)

    system.terminate()
  }

  it should "connect to local address of peer received from UPnP Gateway" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()

    val knownPeerLocalAddress = new InetSocketAddress("192.168.1.56", 12345)
    val knownPeerExternalAddress = new InetSocketAddress("88.44.33.11", 4567)

    val settings2 = settings.copy(network = settings.network.copy(knownPeers = Seq(knownPeerExternalAddress)))

    val upnp = DummyUPnPGateway(
      InetAddress.getByName("88.44.33.11"),
      InetAddress.getByName("192.169.1.11")
    ) { _ => Some(knownPeerLocalAddress) }

    val networkControllerRef: ActorRef = createNetworkController(settings2, tcpManagerProbe, Some(upnp))

    import scala.concurrent.duration._
    val connectAddr = tcpManagerProbe.expectMsgPF(10.seconds) {
      case Tcp.Connect(addr, _, _, _, _) => addr
    }

    connectAddr should be (knownPeerLocalAddress)

    system.terminate()
  }

  it should "not connect to itself" in {
    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()

    val nodeAddr = new InetSocketAddress("127.0.0.1", settings.network.bindAddress.getPort)
    val networkControllerRef: ActorRef = createNetworkController(settings, tcpManagerProbe, None)
    val testPeer = new TestPeer(settings, networkControllerRef, tcpManagerProbe)

    val peerLocalAddress = new InetSocketAddress("192.168.1.2", settings.network.bindAddress.getPort)

    testPeer.connect(new InetSocketAddress("192.168.1.2", 5678), nodeAddr)

    val handshakeFromNode  = testPeer.receiveHandshake
    val nodeLocalAddress = extractLocalAddrFeat(handshakeFromNode).value.address
    testPeer.sendHandshake(None, Some(peerLocalAddress))
    testPeer.sendPeers(Seq(nodeLocalAddress))

    testPeer.sendGetPeers
    val peers = testPeer.receivePeers

    peers should contain theSameElementsAs (Seq(peerLocalAddress))
    system.terminate()
  }

  it should "remove non reachable peers from peers database" in {

    implicit val system = ActorSystem()

    val tcpManagerProbe = TestProbe()

    val nodeAddr = new InetSocketAddress("127.0.0.1", settings.network.bindAddress.getPort)

    val networkControllerRef: ActorRef = createNetworkController(settings, tcpManagerProbe, None)

    val testPeer1DecalredAddress = new InetSocketAddress("55.66.77.88", 5566)
    val testPeer1 = new TestPeer(settings, networkControllerRef, tcpManagerProbe)
    testPeer1.connect(new InetSocketAddress("127.0.0.1", 2322), nodeAddr)
    testPeer1.receiveHandshake
    testPeer1.sendHandshake(Some(testPeer1DecalredAddress), None)

    val testPeer2 = new TestPeer(settings, networkControllerRef, tcpManagerProbe)
    testPeer2.connect(new InetSocketAddress("127.0.0.1", 2323), nodeAddr)
    testPeer2.receiveHandshake
    testPeer2.sendHandshake(None, None)

    testPeer2.sendGetPeers()
    testPeer2.receivePeers should contain theSameElementsAs  Seq(testPeer1DecalredAddress)
    system.terminate()
  }

  private def extractLocalAddrFeat(handshakeFromNode: Try[Handshake]) = {
    handshakeFromNode.success.value.features.collectFirst { case a: LocalAddressPeerFeature => a }
  }

  /**
    * Create NetworkControllerActor
    */
  private def createNetworkController(settings: ScorexSettings, tcpManagerProbe: TestProbe, upnp: Option[UPnPGateway] = None)(implicit system:ActorSystem) = {
    val timeProvider = LocalTimeProvider
    val externalAddr = settings.network.declaredAddress
      .orElse(upnp.map(u => new InetSocketAddress(u.externalAddress, settings.network.bindAddress.getPort)))

    val messageSpecs = Seq(GetPeersSpec, PeersSpec)
    val scorexContext = ScorexContext(messageSpecs, Seq.empty, upnp, timeProvider, externalAddr)

    val peerManagerRef = PeerManagerRef(settings, scorexContext)

    val networkControllerRef: ActorRef = NetworkControllerRef(
      "networkController", settings.network,
      peerManagerRef, scorexContext, tcpManagerProbe.testActor)


    tcpManagerProbe.expectMsg(Bind(networkControllerRef, settings.network.bindAddress, options = KeepAlive(true) :: Nil))

    tcpManagerProbe.send(networkControllerRef, Bound(settings.network.bindAddress))
    networkControllerRef
  }
}

/**
  * Stub for UpNP gateway
  * @param externalAddress - WAN gateway address
  * @param localAddress - LAN getaway address
  * @param getLocalAddrForExtPort - lambda function for returns LAN address for given WAN port
  */
case class DummyUPnPGateway(override val externalAddress: InetAddress,
                            override val localAddress: InetAddress)
                           (getLocalAddrForExtPort: (Int => Option[InetSocketAddress])) extends UPnPGateway {

  override def addPort(port: Int): Unit = {}

  override def deletePort(port: Int): Unit = {}

  override def getLocalAddressForExternalPort(externalPort: Int): Option[InetSocketAddress] = {
    getLocalAddrForExtPort(externalPort)
  }
}

/**
  * Helper class that emulates peers
  */
class TestPeer(settings: ScorexSettings, networkControllerRef: ActorRef, tcpManagerProbe: TestProbe)
              (implicit ec:ExecutionContext) extends Matchers {

  private val timeProvider = LocalTimeProvider
  private val featureSerializers = Map(LocalAddressPeerFeature.featureId -> LocalAddressPeerFeatureSerializer)
  private val handshakeSerializer = new HandshakeSerializer(featureSerializers, settings.network.maxHandshakeSize)
  private val messageSpecs = Seq(GetPeersSpec, PeersSpec)
  private val messagesSerializer = new MessageSerializer(messageSpecs)

  private var connectionHandler:ActorRef = _

  /**
    * Connect peer to node
    * @param peerAddr - peer address
    * @param nodeAddr - node address
    */
  def connect(peerAddr: InetSocketAddress, nodeAddr: InetSocketAddress): Unit = {
    tcpManagerProbe.send(networkControllerRef, Connected(peerAddr, nodeAddr))

    connectionHandler = tcpManagerProbe.expectMsgPF() {
      case Tcp.Register(handler, _, _) => handler
    }

    tcpManagerProbe.expectMsg(Tcp.ResumeReading)
  }

  /**
    * Send handshake message to node
    * @param declaredAddress
    * @param localAddress
    * @return
    */
  def sendHandshake(declaredAddress: Option[InetSocketAddress], localAddress: Option[InetSocketAddress]): Tcp.ResumeReading.type = {
    val features = localAddress.map(LocalAddressPeerFeature(_)).toSeq
    val handshakeToNode = Handshake(settings.network.agentName,
      Version(settings.network.appVersion), "test",
      declaredAddress, features, timeProvider.time())

    tcpManagerProbe.send(connectionHandler, Tcp.Received(ByteString(handshakeSerializer.toBytes(handshakeToNode))))
    tcpManagerProbe.expectMsg(Tcp.ResumeReading)
  }

  /**
    * Receive handshake message from node
    * @return Success with handshake message if received valid handshake message and Fail for invalid message
    */
  def receiveHandshake: Try[Handshake] = {
    val handshakeFromNode = tcpManagerProbe.expectMsgPF() {
      case Tcp.Write(data, e) =>
        handshakeSerializer.parseBytesTry(data.toByteBuffer.array)
    }
    handshakeFromNode
  }

  /**
    * Send GetPeers message to node
    */
  def sendGetPeers(): Unit = {
    val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
    sendMessage(msg)
  }

  /**
    * Receive GetPeer message from node
    * @return
    */
  def receiveGetPeers: Message[_] = {
    val message = receiveMessage
    message.spec.messageCode should be (GetPeersSpec.messageCode)
    message
  }

  /**
    * Receive sequence of peer addresses from node
    */
  def receivePeers: Seq[InetSocketAddress] = {
    val message = receiveMessage
    message.spec.messageCode should be (PeersSpec.messageCode)
    PeersSpec.parseBytes(message.input.left.value)
  }

  /**
    * Send sequence of peer addresses to node
    */
  def sendPeers(peers: Seq[InetSocketAddress]): Unit = {
    val msg = Message(PeersSpec, Right(peers), None)
    sendMessage(msg)
  }

  /**
    * Send message to node
    * @param msg
    */
  def sendMessage(msg: Message[_]): Unit = {
    val byteString = messagesSerializer.serialize(msg)
    tcpManagerProbe.send(connectionHandler, Tcp.Received(byteString))
    tcpManagerProbe.expectMsg(Tcp.ResumeReading)
  }

  /**
    * Receive message from node
    * @param msg
    */
  def receiveMessage: Message[_] = {
    tcpManagerProbe.expectMsgPF() {
      case Tcp.Write(b, _) =>
        messagesSerializer.deserialize(b, None).success.value.value
    }
  }
}
