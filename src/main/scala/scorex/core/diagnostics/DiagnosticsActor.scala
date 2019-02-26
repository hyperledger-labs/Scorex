package scorex.core.diagnostics

import java.io.{File, PrintWriter}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.consensus.SyncInfo
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SyntacticallySuccessfulModifier
import scorex.core.network.SendingStrategy
import scorex.core.network.message.{InvData, Message, ModifiersData}
import scorex.util.{ModifierId, ScorexLogging}

class DiagnosticsActor extends Actor with ScorexLogging {

  import DiagnosticsActor.ReceivableMessages._

  private val outWriter = new PrintWriter(new File(s"/tmp/ergo/out-messages-${context.system.startTime}.json"))
  private val inWriter = new PrintWriter(new File(s"/tmp/ergo/in-messages-${context.system.startTime}.json"))
  private val smJournalWriter = new PrintWriter(new File(s"/tmp/ergo/sm-journal-${context.system.startTime}.json"))
  private val mProfilesWriter = new PrintWriter(new File(s"/tmp/ergo/nvh-profile-${context.system.startTime}.json"))
  private val cacheJournalWriter = new PrintWriter(new File(s"/tmp/ergo/cache-journal-${context.system.startTime}.json"))

  override def preStart(): Unit = {
    Seq(outWriter, inWriter, smJournalWriter, mProfilesWriter, cacheJournalWriter).foreach(_.write("["))
    log.info("Starting diagnostics actor...")
    context.system.eventStream.subscribe(self, classOf[SyntacticallySuccessfulModifier[_]])
  }

  override def postStop(): Unit = {
    Seq(outWriter, inWriter, smJournalWriter, mProfilesWriter, cacheJournalWriter).foreach { w =>
      w.write("]")
      w.close()
    }
  }

  override def receive: Receive = {
    case OutNetworkMessage(Message(spec, Right(data), _), strategy, receivers, timestamp) =>
      receivers.foreach { r =>
        val record =
          s"""{"timestamp":$timestamp,"msgType":"${spec.messageName}","data":${decodeData(data)},"strategy":"$strategy","receiver":"$r"},\n"""
        outWriter.write(record)
      }

    case InNetworkMessage(Message(spec, Right(data), _), sender, timestamp) =>
      val record =
        s"""{"timestamp":$timestamp,"msgType":"${spec.messageName}","data":${decodeData(data)},"sender":"$sender"},\n"""
      inWriter.write(record)

    case SyntacticallySuccessfulModifier(mod, ts) =>
      val record = s"""{"typeId":"${mod.modifierTypeId}","id":"${mod.encodedId}","timestamp":$ts},\n"""
      smJournalWriter.write(record)

    case MethodProfile(tag, elapsedTime, ts) =>
      val record = s"""{"tag":"$tag","elapsedTime":$elapsedTime,"timestamp":$ts},\n"""
      mProfilesWriter.write(record)

    case CacheState(sizeBefore, sizeAfter, cleared, ts) =>
      val record = s"""{"sizeBefore":$sizeBefore,"sizeAfter":$sizeAfter,"cleared":[${cleared.map(id => s""""$id"""").mkString(",")}],"timestamp":$ts},\n"""
      cacheJournalWriter.write(record)

    case other =>
      log.info(s"DiagnosticsActor: unknown message: $other")
  }

  private def decodeData(data: Any) = data match {
    case InvData(typeId, ids) =>
      s"""{"typeId":"$typeId","ids":[${ids.map(id => s""""$id"""").mkString(",")}]}"""
    case ModifiersData(typeId, mods) =>
      s"""{"typeId":"$typeId","ids":[${mods.keys.map(id => s""""$id"""").mkString(",")}]}"""
    case si: SyncInfo =>
      val ids = si.startingPoints
      s"""{"typeId":"101","ids":[${ids.map(id => s""""${id._2}"""").mkString(",")}]}"""
    case other =>
      s""""?$other""""
  }

}

object DiagnosticsActor {

  object ReceivableMessages {

    case class OutNetworkMessage(msg: Message[_], strategy: SendingStrategy, receivers: Seq[String], timestamp: Long)

    case class InNetworkMessage(msg: Message[_], sender: String, timestamp: Long)

    case class MethodProfile(tag: String, elapsedTime: Double, timestamp: Long)

    case class CacheState(sizeBefore: Int, sizeAfter: Int, cleared: Seq[ModifierId], timestamp: Long)

  }

}

object DiagnosticsActorRef {
  def apply(name: String)(implicit system: ActorSystem): ActorRef = system.actorOf(Props[DiagnosticsActor], name)
}
