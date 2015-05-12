import java.util.UUID

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.util.Random

/**
 * Created by ondra on 5/12/15.
 */
object GuessMe extends App {
  case object Initiate
  case object Guess
  case class Guessed(uuid: String, number: Int)
  val RANGE: Int = 1001

  class Guesser(uuid: String) extends Actor with ActorLogging {
    def receive = {
      case Guess => {
        val random = new Random().nextInt(RANGE)
        log.info(uuid + " is guessing: " + random)
        sender ! Guessed(uuid, random)
      }
    }
  }

  class Questioner extends Actor with ActorLogging {
    val guessme: Int = new Random().nextInt(RANGE)

    val guessRoute = context.actorOf(
      Props(new Guesser(UUID.randomUUID().toString)).withRouter(RoundRobinRouter(10)),
      name = "guessRoute"
    )

    def receive = {
      case Initiate =>
        log.info("Initialization... please guess number: " + guessme)
        for (i <- 0 until 10)
          guessRoute ! Guess
      case Guessed(uuid, number) =>
        if (number != guessme) {
          sender ! Guess
        } else {
          println("The guesser with uuid " + uuid + " guessed it!")
          for (i <- 0 until 10)
            guessRoute ! PoisonPill
          self ! PoisonPill
          context.system.shutdown()
        }
    }
  }

  val system = ActorSystem("GuessSystem")
  val questioner = system.actorOf(Props(new Questioner()), name = "questioner")
  questioner ! Initiate

}