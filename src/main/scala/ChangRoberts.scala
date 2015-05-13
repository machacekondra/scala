import akka.actor._
import akka.dispatch.Dispatchers
import akka.routing._

/**
 * Created by ondra on 5/13/15.
 */
object ChangRoberts extends App {
  val RING_SIZE: Int = 5

  case class LeaderFound(uid: Int)
  case class Election(myuid: Int, uid: Int)

  class Process(puid: Int) extends Actor {
    def receive = {
      case Election(myuid: Int, uid: Int) =>
        println(self + " - " + puid + " - " + uid)
        if(puid == uid) {
          sender ! LeaderFound(uid)
        } else if(uid < puid) {
          sender ! Election(puid, puid)
        } else {
          sender ! Election(puid, uid)
        }
    }
  }

  case class RingRoute(num: Int) extends RouterConfig {

    def routerDispatcher: String = Dispatchers.DefaultDispatcherId

    def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.defaultStrategy

    def nextUid(uid: Int): Int = {
      (uid % num) + 1
    }

    def createRoute(routeeProps: Props, routeeProvider: RouteeProvider): Route = {
      val routees = for (uid <- 1 to num) yield routeeProvider.context.actorOf(Props(new Process(uid)), "p" + uid)

      routeeProvider.registerRoutees(routees)

      {
        case (sender, message) =>
          message match {
            case Election(myuid: Int, uid: Int) =>
              List(Destination(sender, routees(nextUid(myuid) - 1)))
          }
      }
    }
  }

  class Ring extends Actor {

    val ringRoute = context.actorOf(
      Props[Process].withRouter(RingRoute(RING_SIZE)),
      name = "ringRoute"
    )

    def receive = {
      case Election(myuid: Int, uid: Int) =>
        ringRoute ! Election(myuid, uid)
      case LeaderFound(uid: Int) =>
        println("Leader found: " + uid)
        //ringRoute ! Broadcast(PoisonPill)
        self ! PoisonPill
        context.system.shutdown()
    }
  }

  val system = ActorSystem("ChangRoberts")
  val ring = system.actorOf(Props(new Ring()), name = "ring")
  ring ! Election(1, 1)

}
