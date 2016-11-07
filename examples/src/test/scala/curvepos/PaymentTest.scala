
package test
/*
There is just a test, that apply transactions one by one and check, that total balance is not changes. 
1. Write a test, that checks concrete changes of transactions (that recipient got his balance, sender lost his balance and miner got his fee)

1.1. Write a test for special case, when user send coins to himself (sender == recipient)
1.2. Write a test that checks, that incorrect transactions don't pass (amount<0, fee<0, fee + amont > Long.MaxValue, sender don't have enough balance and so on)
1.3. Imagine more checks for single transaction if you can


2. Write a test, that apply block with plenty of transactions (number of transactions should differs from 0 to 1000). Checks shuld be similar to checks in 1.
3. Write tests to SimpleNodeViewHolder. Make sure, that state and blockchain are always persistent (e.g. try to process block, that is valid from point of Blockchain view, but is invalid from point of State view ) (edited)

 */
import curvepos.ExampleGenerators
import examples.curvepos.transaction.SimpleBlock
import examples.curvepos.transaction.SimplePayment
import examples.curvepos.transaction.SimpleState
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.util.Failure
import scala.util.Success
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion

//import scala.util.{Failure, Success}

object Util {
  import scala.util.Random._
  def randULong = nextLong.abs
  def randULong(max:Long):Long = randULong % max
  def randBytes(size:Int) = 1 to size map(i => randULong(256).toByte) toArray
  def randReceiver = PrivateKey25519Companion.generateKeys(randBytes(256))
  implicit def longToInt(l:Long) = l.toInt
  
}
object PaymentTest extends PropSpec with ExampleGenerators with PropertyChecks with Matchers with App{
  import Util._
  // initialize test
  var state = new SimpleState()
  def getBalance(p:PublicKey25519Proposition):Long = state.boxesOf(p).map(_.value).sum
  
  state.totalBalance shouldBe 0
  state.isEmpty shouldBe true

  // apply genesis block
  state = state.applyModifier(genesisBlock).get
  val GenesisBalance = state.totalBalance

  state.isEmpty shouldBe false

  val max = 1000
  
  val keys = 0 to max map {i => randReceiver}
  
  // all below tests fail. See comments in below code for test1, test2, test3
  
  test1 // payment from genesis to random key       
  test2 // payment from random key to random key
  test3 // payment from random key to itself
  
  def test1 = {
   // test payments 1: Genesis => rand keys
    0 to max map {i =>
      state.totalBalance shouldBe Long.MaxValue
      val sender = genesisAcc.publicImage
      val recipient = keys(i)._2    
      val senderOldBalance = getBalance(sender)
      val amount = randULong(senderOldBalance.min(10000000000000000L))
      val fee = randULong(senderOldBalance - amount)    
      val nonce = state.boxesOf(sender).head.nonce
      val timestamp = randULong
      val payment = SimplePayment(sender, recipient, amount, fee, nonce, timestamp)    
      val receiverOldBalance = getBalance(payment.recipient)
      
      receiverOldBalance shouldBe 0

      state.validate(payment).isSuccess shouldBe true

      val block = SimpleBlock(Array.fill(SimpleBlock.SignatureLength)(-1: Byte),
        0L, Array.fill(SimpleBlock.SignatureLength)(0: Byte), 1, genesisAcc.publicImage, Seq(payment))

      state.applyModifier(block) match {
        case Success(newState) =>
          state = newState
          
          /*
           * Below FAILS when amount transferred is 0
           *  org.scalatest.exceptions.TestFailedException: 9223372036854775805 was not equal to 9223372036854775807
           */
          state.totalBalance shouldBe Long.MaxValue
          
          val receiverNewBalance = getBalance(payment.recipient)
          val senderNewBalance = getBalance(payment.sender)
          
          receiverNewBalance shouldBe payment.amount
          
          (senderOldBalance - amount - fee) shouldBe senderNewBalance - fee
        
        case Failure(e) =>
          throw e
      } 
    }
  }
  // test payment 2: rand key => rand key
  def test2 = {
    0 to max map {i =>
      val sender = keys(i)._2
      val recipient = keys(randULong(max))._2    
      val senderOldBalance = getBalance(sender)

      senderOldBalance > 0 shouldBe true

      val amount = randULong(senderOldBalance)
      val fee = randULong(senderOldBalance-amount)    
      val nonce = state.boxesOf(sender).head.nonce
      val timestamp = randULong
      val payment = SimplePayment(sender, recipient, amount, fee, nonce, timestamp)    

      state.validate(payment).isSuccess shouldBe true

      if (amount > 0) {
        val receiverOldBalance = getBalance(recipient)
        val block = SimpleBlock(Array.fill(SimpleBlock.SignatureLength)(-1: Byte),
          0L, Array.fill(SimpleBlock.SignatureLength)(0: Byte), 1, genesisAcc.publicImage, Seq(payment))

        state.applyModifier(block) match {
          case Success(newState) =>
            state = newState

            /* below FAILS with error:
                org.scalatest.exceptions.TestFailedException: -4893054997409295431 was not equal to 9223372036854775807
            */
            state.totalBalance shouldBe Long.MaxValue
            
            val receiverNewBalance = getBalance(payment.recipient)
            val senderNewBalance = getBalance(payment.sender)
            if (payment.sender != payment.recipient) {

              (receiverNewBalance - amount) shouldBe receiverOldBalance

              (senderOldBalance - fee - amount) shouldBe senderNewBalance

            } else {

              receiverOldBalance == senderOldBalance shouldBe true

              senderOldBalance - fee == senderNewBalance              

            }
          case Failure(e) =>
            throw e
        }
      }
    }
  }
  def test3 = {
    // test payment 3: sameKey => sameKey
    0 to max map {i =>
      val sender = keys(i)._2
      val recipient = sender
      val senderOldBalance = getBalance(sender)

      senderOldBalance > 0 shouldBe true

      val amount = randULong(senderOldBalance)
      val fee = randULong(senderOldBalance-amount)    
      val nonce = state.boxesOf(sender).head.nonce
      val timestamp = randULong
      val payment = SimplePayment(sender, recipient, amount, fee, nonce, timestamp)    

      state.validate(payment).isSuccess shouldBe true

      val receiverOldBalance = getBalance(recipient)
  
      if (amount > 0) {
        receiverOldBalance shouldBe senderOldBalance
        val block = SimpleBlock(Array.fill(SimpleBlock.SignatureLength)(-1: Byte),
          0L, Array.fill(SimpleBlock.SignatureLength)(0: Byte), 1, genesisAcc.publicImage, Seq(payment))

        state.applyModifier(block) match {
          case Success(newState) =>
            state = newState
            val senderNewBalance = getBalance(payment.sender)
            val receiverNewBalance = getBalance(payment.recipient)
             
            receiverNewBalance shouldBe senderNewBalance
            
            receiverOldBalance shouldBe senderOldBalance
            
            /*
             * below FAILS 
             * org.scalatest.exceptions.TestFailedException: 19300920783371058 was not equal to 5091909326613251
             */
            (senderOldBalance - fee) shouldBe senderNewBalance

          case Failure(e) =>
            throw e
        }
      }
    }
  }
}

