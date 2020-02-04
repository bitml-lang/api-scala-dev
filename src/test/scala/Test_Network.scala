
import java.io.File

import scala.concurrent.duration._
import akka.testkit.javadsl.TestKit
import akka.pattern.ask
import akka.actor.{ActorSystem, Address, Props}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import fr.acinq.bitcoin.Transaction
import org.scalatest.funsuite.AnyFunSuite
import xyz.bitml.api.TxEntry
import xyz.bitml.api.messaging.{Heartbeat, Node, Ping, Pong}
import xyz.bitml.api.persistence.{MetaStorage, TxStorage}

import scala.collection.immutable.HashMap
import scala.concurrent.Await


class Test_Network extends AnyFunSuite {
  test("Basic remote test. Heartbeat between two nodes on \"different\" networks") {


    // Setup placeholder storage
    val metadb = new MetaStorage(new HashMap[String, TxEntry])
    val txdb = new TxStorage(new HashMap[String, Transaction])

    // Setup network A
    val configFileA = getClass.getClassLoader.
      getResource("test_application.conf").getFile
    val configA = ConfigFactory.parseFile(new File(configFileA))
    val systemA = ActorSystem("TestA" , configA)
    val nodeA = systemA.actorOf(Props(classOf[Node], metadb), name = "HeartbeatNode")
    val addrA = nodeA.path.address // Local address A
    println(addrA)

    val remoteEndpointA =  new Address(protocol = "akka.tcp", system = "TestA", host = "127.0.0.1", port = 5150)

    // Setup network B
    val configFileB = getClass.getClassLoader.
      getResource("test_application_b.conf").getFile
    val configB = ConfigFactory.parseFile(new File(configFileB))
    val systemB = ActorSystem("TestB" , configB)
    val nodeB = systemB.actorOf(Props(classOf[Node], metadb), name = "HeartbeatNode")
    val addrB = nodeB.path.address // Local address B
    println(addrB)

    // TODO: debug properly
    // setting up TestKIt instances writes down all debug messages, including
    val testKitA = new TestKit(systemA)
    val testKitB = new TestKit(systemB)

    nodeB ! Ping() // the sender of a non-actor message is deadLetters. This will log an error when it tries to Pong()
    nodeB ! Pong() // On the other hand this is perfectly legitimate, even if the sender is deadLetters.

    // "Proper" version of the Ping() test above, that catches and prints the response message.
    implicit val timeout: Timeout = Timeout(1 second)
    val future = nodeB.ask(Ping())
    println(Await.result(
      (future), timeout.duration)) // This will return a "Pong()" message.

    // Heartbeat() is asynchronous. This is hard to test, but it will show up on the debug log after a certain timeout.
    nodeB ! Heartbeat(remoteEndpointA.toString)
    Thread.sleep(2000)
    // the debug log should have a few lines about message serialization and shipping back and forth,
    // then a Pong() "Heartbeat..." message with akka.tcp://TestA@127.0.0.1:5150/user/HeartbeatNode... as sender.
    // TODO: find better way to test these. Maybe return the string instead of printing from Node.receive()?
    println("TEST END")

  }
}
