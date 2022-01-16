//https://www.scala-lang.org/api/2.12.7/scala/collection/immutable/StringOps.html



import scala.compiletime.ops.string
import scala.io.StdIn



@main def hello: Unit = 

  var state = State("",(mainRoom, mainDirectory))
  var input = "look room"
  while (true) {
    state = roomState_loop(input, state)
    input = StdIn.readLine()
  } 

class State[TempData, StateObjects](val tempData:TempData, val stateObjects:StateObjects)

def roomState_loop = text_adventure_state_loop(doActionInRoom)

def validateInput = (input:String) => 
  val tokens = List.from(input.split(" ")) 
  ValidCommand.values.find(_.name == tokens.head) match 
    case Some(command) => Right((command, tokens.tail))
    case None =>  Left(s"${tokens.head} is not a valid command.")

def text_adventure_state_loop[StateObjects] = 
  (doActionWithState:(command_tuple:(ValidCommand, List[String]),state:State[String,StateObjects]) => State[String,StateObjects]) => 
  (input:String, state:State[String,StateObjects]) => 
    val newState = validateInput(input) match 
      case Right(command_tuple) => doActionWithState(command_tuple,state) // Success case
      case Left(string) => State(string, state.stateObjects) // Error case
    println(newState.tempData)
    println("-----")
    newState


def doActionInRoom = (command_tuple:(ValidCommand, List[String]), state:State[String,(Room,RoomDirectory)]) => {
  val room = state.stateObjects._1
  val directory = state.stateObjects._2
  val command = command_tuple._1
  val list = command_tuple._2

  command match 
    case ValidCommand.Exit => State("Exit is not implemented yet.", state.stateObjects)
    case ValidCommand.Inventory => State("Inventory is not implemented yet.", state.stateObjects)
    case ValidCommand.Go => if list.length == 0 
      then State(ValidCommand.Go.errorMessage.get,state.stateObjects)
      else directory.navigate(room, list(0))    
    case ValidCommand.Look =>   if list.length == 0 
          then State(SenseCommand.Look.errorMessage,state.stateObjects)
          else if list(0) == "room" || list(0) == room.name
          then State(room.description + "\n" + "What will you do? \n", state.stateObjects) 
          else State(room.actOnItem((SenseCommand.Look, list)),state.stateObjects)
          // else State("I'm a big stupid gorilla.",state.stateObjects)
    case ValidCommand.Smell => if list.length == 0 
      then State(SenseCommand.Smell.errorMessage,state.stateObjects)
      else State(room.actOnItem((SenseCommand.Smell, list)),state.stateObjects)
}

enum ValidCommand(val name:String, val errorMessage:Option[String] ){ 
  case Look extends ValidCommand("look",Some("What will you look at?"))
  case Smell extends ValidCommand("smell", Some("What will you smell?"))
  case Go extends ValidCommand("go", Some("Where will you go?"))
  case Inventory extends ValidCommand("inventory", None)
  case Exit extends ValidCommand("exit",None)
}

abstract class Command(val arity:CommandArity)

enum CommandArity:
  case Nullary
  case Unary

enum MiscCommand extends Command(CommandArity.Nullary):
  case Inventory
  case Exit 

enum SenseCommand(val name:String) extends Command(CommandArity.Unary):
  case Look extends SenseCommand("look")
  case Smell extends SenseCommand("smell")
  case Touch extends SenseCommand("touch")
  case Hear extends SenseCommand("hear")
  case Taste extends SenseCommand("Taste")
  val errorMessage =
    this match
      case SenseCommand.Hear => "What will you listen to?"
      case SenseCommand.Smell => "What will you smell?"
      case SenseCommand.Taste => "What will you taste?"
      case SenseCommand.Touch => "What will you touch?"
      case SenseCommand.Look => "What will you look at?"
  

class SenseProps(val look:String = "You cannot see it with your eyes.", 
    val smell:String = "It doesn't have a smell.", 
    val taste:String = "It doesn't have a taste.", 
    val touch:String = "It cannot be touched.",
    val hear:String = "It doesn't make a sound.")

abstract class Sensible(val props:SenseProps){
  def sense = (command:SenseCommand) => {
    command match
      case SenseCommand.Look => props.look
      case SenseCommand.Hear => props.hear
      case SenseCommand.Smell => props.smell
      case SenseCommand.Taste => props.taste
      case SenseCommand.Touch => props.touch
  }
}

class Item(val name:String, override val props:SenseProps) extends Sensible(props)

val itemProps = SenseProps(
  "The desk looks wooden",
  "The desk smells like fresh pine needles.",
  "The desk tastes like grime and dust.", 
  "The wood of the desk feels slightly coarse.")

// class Item(val name:String,val props: SenseProps){
//   def sense = (command:ValidCommand) => {
//     props.smell
//   }
// }

val desk_props = SenseProps("The desk is wooden.","The desk smells like fresh pine needles.")
val desk = Item("desk",desk_props)

val floor_props = SenseProps("The floor is stone.","The floor smells of dust.")
val floor = Item("floor",floor_props)

val roomObjects = List(desk, floor)

def actOnItemInList_generic = (itemNotFound:String) => (items:List[Item]) => 
  (command_tuple:(SenseCommand, List[String])) => {
    val command = command_tuple(0)
    items.find(_.name == command_tuple(1)(0)) match 
            case Some(item) => item.sense(command)
            case None => itemNotFound
    }

class Room(
  val name:String, 
  val description:String,
  val items:List[Item]) {

    def actOnItem = actOnItemInList_generic("That object is not in this room.")(items)

}

val barrel_props = SenseProps
  ("The barrel looks round.",
  "Unmistakable scent of a barrel that was once filled with rich, red wine.")
                        
val barrel = Item("barrel", barrel_props)
val sideRoom_objects = List(barrel)

val sideRoom = Room("SideRoom",
  "This is a side room. It is damp, and there is a barrel here.\nThere is a door to the MainRoom", 
  sideRoom_objects
  )

val mainRoom:Room = Room("MainRoom","You are in a room. There is a desk.\n There is a door to the SideRoom.", roomObjects)

/**
* Input a tuple -> (Room, List[AdjascentRooms])
*/
class RoomDirectory(val roomList:List[(Room,List[Room])]){

  def navigate = (from:Room,to:String) => {

       roomList.find(_._1.name == from.name) match 
        case Some(roomTuple) => roomTuple._2.find(_.name == to) match 
          case Some(room) =>  State(s"You go to the ${room.name}\n" + room.description,(room, this))
          case None => State("You cannot get there from this room.", (from,this))

        case None => State("How did you get here?",(from,this))
  }
}

val lint = Item("lint",SenseProps("Looks fuzzy.","Smells extremely dusty."))

class Inventory(val itemList:List[Item]) {
  def actOnItem = actOnItemInList_generic("You do not have that item in your inventory.")(itemList)
}

val inventory = Inventory(List(lint))

val mainDirectory = RoomDirectory(List(
    (mainRoom,List(sideRoom)),
    (sideRoom,List(mainRoom))))

