import java.util.UUID

import akka.actor._
import akka.routing.{Broadcast, RoundRobinRouter}

import scala.util.Random

/**
 * Created by ondra on 5/12/15.
 */
object GuessMe extends App {
  case object Initiate
  case object Guess
  case class Guessed(uuid: String, number: Int)
  val RANGE: Int = 1001
  val GUESSERS_COUNT: Int = 10

  class Guesser(uuid: String) extends Actor {

    def receive = {
      case Guess => {
        val number: Int = Random.nextInt(RANGE)
        sender ! Guessed(uuid, number)
      }
    }
  }

  class Questioner extends Actor {
    val guessme: Int = Random.nextInt(RANGE)

    val guessRoute = context.actorOf(
      Props(new Guesser(UUID.randomUUID().toString)).withRouter(RoundRobinRouter(GUESSERS_COUNT)),
      name = "guessRoute"
    )

    def receive = {
      case Initiate =>
        guessRoute ! Broadcast(Guess)
      case Guessed(uuid, number) =>
        if (number != guessme) {
          sender ! Guess
        } else {
          println(uuid)
          guessRoute ! Broadcast(PoisonPill)
          self ! PoisonPill
          context.system.shutdown()
        }
    }
  }

  val system = ActorSystem("GuessSystem")
  val questioner = system.actorOf(Props(new Questioner()), name = "questioner")
  questioner ! Initiate

}