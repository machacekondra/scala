import akka.actor._
import akka.routing.RoundRobinRouter
import scala.util.Random

/**
 * Created by ondra on 5/12/15.
 */
object GuessMe extends App {
  case object Initiate
  case object Guess
  case class Guessed(number: Int)

  class Guesser() extends Actor with ActorLogging {
    def receive = {
      case Guess => {
        val random = new Random().nextInt(101)
        log.info("I am guessing: " + random)
        sender ! Guessed(random)
      }
    }
  }

  class Questioner extends Actor with ActorLogging {
    val guessme: Int = new Random().nextInt(101)

    val guessRoute = context.actorOf(
      Props[Guesser].withRouter(RoundRobinRouter(10)),
      name = "guessRoute"
    )

    def receive = {
      case Initiate =>
        log.info("Initialization... please guess number: " + guessme)
        for (i <- 0 until 10)
          guessRoute ! Guess
      case Guessed(number) =>
        log.info("Guesser " + sender + " guess " + number)
        if (number == guessme) {
          println("!!!!!!")
          for (i <- 0 until 10)
            guessRoute ! PoisonPill
          context.stop(self)
        } else {
          sender ! Guess
        }
    }
  }

  val system = ActorSystem("GuessSystem")
  val questioner = system.actorOf(Props(new Questioner()), name = "questioner")
  questioner ! Initiate

}