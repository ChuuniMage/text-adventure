//https://www.scala-lang.org/api/2.12.7/scala/collection/immutable/StringOps.html

package text_adventure

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

enum Arity:
  case Nullary
  case Unary

// type CommandType = SenseCommand | MovementCommand | MiscCommand

// enum SenseCommand:
//   case Look
//   case Smell
//   case Hear
//   case Touch
//   case Taste

// enum MovementCommand:
//   case Go
//   case Exit

// enum MiscCommand:
//   case Inventory

// def honk = (cmd:CommandType) => cmd match
//   case SenseCommand.Hear => ""

// def parseCommandSubtype =(cmd:CommandType) => cmd match
//   case MovementCommand.Exit | MovementCommand.Exit => cmd


enum ValidCommand(override val name:String) extends Named(name){ 
  case Look extends ValidCommand("look")
  case Smell extends ValidCommand("smell")
  case Hear extends ValidCommand("hear")
  case Touch extends ValidCommand("touch")
  case Taste extends ValidCommand("taste")
  case Go extends ValidCommand("go")
  case Inventory extends ValidCommand("inventory")
  case Exit extends ValidCommand("exit")
  val notImplemented = name + " is not implemented yet."
}

def doActionInRoom = (command_tuple:(ValidCommand, List[String]), state:State[String,(Room,RoomDirectory)]) => 
  val room = state.stateObjects._1
  val directory = state.stateObjects._2
  val command = command_tuple._1
  val list = command_tuple._2

  val arity = list.length match 
      case 0 => Some(Arity.Nullary)
      case 1 => Some(Arity.Unary)
      case _ => None

  lazy val senseItem = room.getItem(list(0)) match
          case Left(string) => State(string,state.stateObjects)
          case Right(item) => State(item.sense(command),state.stateObjects)

  arity match
    case Some(arity) => arity match
      case Arity.Nullary => command match 
        case ValidCommand.Exit => State(command.notImplemented, state.stateObjects)
        case ValidCommand.Inventory => State(command.notImplemented, state.stateObjects)
        case ValidCommand.Go => State("Where would you like to go?", state.stateObjects)
        case ValidCommand.Look => State("What would you like to look at?", state.stateObjects)
        case ValidCommand.Smell => State("What will you smell?", state.stateObjects)
        case ValidCommand.Taste => State("What will you taste?", state.stateObjects)
        case ValidCommand.Touch => State("What will you touch?", state.stateObjects)
        case ValidCommand.Hear => State("What will you hear?", state.stateObjects)
      case Arity.Unary =>  command match 
        case ValidCommand.Exit => State(command.notImplemented, state.stateObjects)
        case ValidCommand.Inventory => State(command.notImplemented, state.stateObjects)
        case ValidCommand.Go => directory.navigate(room, list(0))    
        case ValidCommand.Look => if list(0) == "room" || list(0) == room.name
          then State(room.description + "\n" + "What will you do? \n", state.stateObjects) 
          else senseItem
        case ValidCommand.Smell => senseItem
        case ValidCommand.Taste => senseItem
        case ValidCommand.Touch => senseItem
        case ValidCommand.Hear =>  senseItem
    case None => State("Too many commands! Type less things.\n", state.stateObjects)

def actOnItemInList_generic = (itemNotFound:String) => (items:List[Item]) => 
  (command_tuple:(ValidCommand, List[String])) => 
    val command = command_tuple(0)
    val possibleItemName = command_tuple(1)(0)
    items.find(_.name == possibleItemName) match 
            case Some(item) => command match 
                case ValidCommand.Look => item.sense(command)
                case ValidCommand.Smell => item.sense(command)
                case _ => "Why are you trying to sense an item with a non-sense command?"
            case None => itemNotFound
    
case class SensibleObjects(val objects:List[Sensible & Named]){
 def getObject = (command:ValidCommand,name:String) => objects.find(_.name == name) match
   case Some(item) => item.sense(command)
   case None => "You cannot sense that item in this room."
}

class SenseProps(val look:String = "You cannot see it with your eyes.", 
    val smell:String = "It doesn't have a smell.", 
    val taste:String = "It doesn't have a taste.", 
    val touch:String = "It cannot be touched.",
    val hear:String = "It doesn't make a sound.")

case class Item(override val name:String,override val senseProps: SenseProps) extends Named(name), Sensible(senseProps)

trait Named(val name:String)

//TODO: Have some "fundamental" command typing
//Maybe case classes instead of enums, or enums at the bottom with case classes holding them
trait Sensible(val senseProps:SenseProps){
  def sense = (cmd:ValidCommand) => cmd match
    case ValidCommand.Look => senseProps.look
    case ValidCommand.Smell => senseProps.smell
    case ValidCommand.Hear => senseProps.hear
    case ValidCommand.Touch => senseProps.touch
    case ValidCommand.Taste => senseProps.taste
    case _ => "That's not a sense!"
}

trait HasItems(val items:List[Item], val itemNotFoundMsg:String){ 
  def getItem = (name:String) => items.find(_.name == name) match 
      case Some(item) => Right(item)
      case None => Left(itemNotFoundMsg)
    }
      
case class Room(
  val name:String, 
  val description:String,
  override val items:List[Item]) extends HasItems(items, "That item is not in this room.") {

}

val barrel_props = SenseProps(
  "The barrel looks round.",
  "Unmistakable scent of a barrel that was once filled with rich, red wine.")
                        
val barrel = Item("barrel", barrel_props)
val sideRoom_objects = List(barrel)

val sideRoom = Room("SideRoom",
  "This is a side room. It is damp, and there is a barrel here.\nThere is a door to the MainRoom", 
  sideRoom_objects
  )

val desk_props = SenseProps("The desk is wooden.","The desk smells like fresh pine needles.")
val desk = Item("desk",desk_props)

val floor_props = SenseProps("The floor is stone.","The floor smells of dust.")
val floor = Item("floor",floor_props)

val roomObjects = List(desk, floor)

val mainRoom:Room = Room("MainRoom","You are in a room. There is a desk.\nThere is a door to the SideRoom.", roomObjects)

/**
* Input a tuple -> (Room, List[AdjascentRooms])
*/
case class RoomDirectory(val roomList:List[(Room,List[Room])]){

  def navigate = (from:Room,to:String) => 

       roomList.find(_._1.name == from.name) match 
        case Some(roomTuple) => roomTuple._2.find(_.name == to) match 
          case Some(room) =>  State(s"You go to the ${room.name}\n" + room.description,(room, this))
          case None => State("You cannot get there from this room.", (from,this))

        case None => State("How did you get here?",(from,this))
}

val lint = Item("lint",SenseProps("Looks fuzzy.","Smells extremely dusty."))

case class Inventory(override val items:List[Item]) 
  extends HasItems(items, "That item is not in your inventory.") 

val inventory = Inventory(List(lint))

val mainDirectory = RoomDirectory(List(
    (mainRoom,List(sideRoom)),
    (sideRoom,List(mainRoom))))

val testItem = Item("test",SenseProps("It looks like a test!","It smells like a test!"))

val testRoom = Room("testRoom", "This looks like the testing room.", List(testItem))
