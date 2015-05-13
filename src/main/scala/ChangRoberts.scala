import akka.actor._
import akka.dispatch.Dispatchers
import akka.routing._

/**
 * Created by ondra on 5/13/15.
 */
object ChangRoberts extends App {
  val RING_SIZE: Int = 2000

  case object ElectionDone
  case class LeaderFound(puid: Int, uid: Int)
  case class Election(myuid: Int, uid: Int)

  class Process(puid: Int) extends Actor {
    var participant: Boolean = false
    var leader: Boolean = false
    var leaderUid: Int = _

    def receive = {
      case Election(myuid: Int, uid: Int) =>
        //println("Election" + self + " - " + puid + " - " + uid)
        if(puid == uid) {
          participant = false
          leader = true
          leaderUid = puid
          sender ! LeaderFound(puid, puid)
        } else if(uid < puid) {
          if(!participant) {
            participant = true
            sender ! Election(puid, puid)
          }
        } else {
          participant = true
          sender ! Election(puid, uid)
        }
      case LeaderFound(myuid:Int, uid: Int) =>
        //println("LeaderFound" + self + " - " + puid + " - " + uid)
        if(leader) {
          println("Leader found with id " + leaderUid)
          sender ! ElectionDone
        } else {
          participant = false
          leaderUid = uid
          sender ! LeaderFound(puid, uid)
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
            case LeaderFound(myuid: Int, uid: Int) =>
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
      case LeaderFound(myuid: Int, puid: Int) =>
        ringRoute ! LeaderFound(myuid, puid)
      case ElectionDone =>
        //ringRoute ! Broadcast(PoisonPill)
        self ! PoisonPill
        context.system.shutdown()
    }
  }

  val system = ActorSystem("ChangRoberts")
  val ring = system.actorOf(Props(new Ring()), name = "ring")
  ring ! Election(1, 1)
  ring ! Election(132, 132)
  ring ! Election(1320, 1320)
  ring ! Election(888, 888)

}
