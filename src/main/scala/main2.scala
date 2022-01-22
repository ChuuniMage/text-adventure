//https://www.scala-lang.org/api/2.12.7/scala/collection/immutable/StringOps.html

package text_adventure2

import zio.*

import scala.compiletime.ops.string
import scala.io.StdIn

@main def hello: Unit = 
  var rState = RenderState(mainRoom.sense(ValidCommand.Look))
  var dState = DataState((mainRoom,mainDirectory))
  var tuple = (rState, dState)
  var input = "look room"
  while (true) {
    tuple = roomState_loop(input, dState)
    dState = tuple._2;
    renderTextAdventurePrint(tuple._1.state)
    input = StdIn.readLine()
  } 

case class TextAdventureState(val currentRoom:Room,val roomDirectory:RoomDirectory)

def renderState[A] = (render:A=>Unit) => (rData:A) => render(rData)

val renderTextAdventurePrint = renderState((str:String) => println(str + "\n" + "-----"))

case class RenderState[A](val state:A)

case class DataState[A](val value:A) {
  def map[B] (f: A => B) = DataState(f(value))
  def flatMap[B] (f:A => DataState[B]) = f(value)
}

def validateInput = (input:String) => 
  val tokens = List.from(input.split(" ")) 
  ValidCommand.values.find(_.name == tokens.head) match 
    case Some(command) => Right((command, tokens.tail))
    case None => Left(RenderState(s"${tokens.head} is not a valid command."))

def roomState_loop = text_adventure_state_loop(doActionInRoom)

def text_adventure_state_loop[StateObjects] = 
  (doActionWithState:(command_tuple:(ValidCommand, List[String]),dState:DataState[StateObjects]) => (RenderState[String], DataState[StateObjects])) => 
  (input:String, dState:DataState[StateObjects]) => 
    validateInput(input) match 
      case Right(command_tuple) => doActionWithState(command_tuple,dState) // Success case
      case Left(rState) => (rState, dState) // Error case
  

enum Arity:
  case Nullary
  case Unary

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

//

def senseItemCommand = (cmd:ValidCommand,itemName:Option[String], sensibleItems:List[Sensible & Named]) => {

  itemName match
    case Some(name) => sensibleItems.find(_.name == name) match
      case Some(item) => item.sense(cmd)
      case None => "That item is not in this room."
    case None => s"What would you like to ${cmd.name}?"
}

def goCommand = (dState:DataState[(Room,RoomDirectory)], destination:Option[String]) =>
  val currentRoom = dState.value._1
  val directory = dState.value._2
  destination match
    case Some(destName) =>  directory.roomMap(currentRoom).find(_.name == destName) match
          case Some(room) => (RenderState(s"You go to the ${room.name}\n" + room.sense(ValidCommand.Look) ),dState.map(state => (room, state._2)))
          case None => (RenderState("You cannot get there from this room." ),dState)
    case None => (RenderState("Where would you like to go?" ),dState)



def doActionInRoom = (command_tuple:(ValidCommand, List[String]), dState:DataState[(Room,RoomDirectory)]) => 
  val tuple = dState.value
  val room = tuple._1
  val directory = tuple._2
  val command = command_tuple._1
  val list = command_tuple._2

  val arity = list.length match 
      case 0 => Some(Arity.Nullary)
      case 1 => Some(Arity.Unary)
      case _ => None

  lazy val sensibleItems = dState.value._1.items

  lazy val senseItem = room.getItem(list(0)) match
          case Right(item) => (RenderState(item.sense(command)),dState)
          case Left(string) => (RenderState(string),dState)

  lazy val maybeName = if list.isEmpty then None else Some(list(0))

  arity match
    case Some(arity) => arity match
      case Arity.Nullary => command match 
        case ValidCommand.Exit => (RenderState(command.notImplemented), dState)
        case ValidCommand.Inventory => (RenderState(command.notImplemented), dState)
        case ValidCommand.Go => goCommand(dState, maybeName)
        case ValidCommand.Look => (RenderState("What will you look at?"), dState)
        case ValidCommand.Smell => (RenderState("What will you smell?"), dState)
        case ValidCommand.Taste => (RenderState("What will you taste?"), dState)
        case ValidCommand.Touch => (RenderState("What will you touch?"), dState)
        case ValidCommand.Hear => (RenderState("What will you hear?"), dState)
      case Arity.Unary =>  command match 
        case ValidCommand.Exit => (RenderState(command.notImplemented), dState)
        case ValidCommand.Inventory => (RenderState(command.notImplemented), dState)
        case ValidCommand.Go =>  goCommand(dState, maybeName);    
        case ValidCommand.Look => if list(0) == "room" || list(0) == room.name
          then (RenderState(room.sense(command) + "\n" + "What will you do? \n"), dState) 
          else senseItem
        case ValidCommand.Smell => senseItem
        case ValidCommand.Taste => senseItem
        case ValidCommand.Touch => senseItem
        case ValidCommand.Hear =>  senseItem
    case None => (RenderState("Too many commands! Type less things.\n"), dState)

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
//This isn't going to work as a trait, since I can only add it once to a class
trait HasNamed[T](val ts:List[T & Named], val notFoundMsg:String){ 
  def getT = (name:String) => ts.find(_.name == name) match 
      case Some(t) => Right(t)
      case None => Left(notFoundMsg)
    }
case class Room(
  val name:String, 
  override val senseProps:SenseProps,
  override val items:List[Item]) extends HasItems(items, "That item is not in this room."), Sensible(senseProps) {

}

val barrel_props = SenseProps(
  "The barrel looks round.",
  "Unmistakable scent of a barrel that was once filled with rich, red wine.")
                        
val barrel = Item("barrel", barrel_props)
val sideRoom_objects = List(barrel)

val sideRoomSenseProps = SenseProps("This is a side room. It is damp, and there is a barrel here.\nThere is a door to the MainRoom")
val sideRoom = Room("SideRoom",
  sideRoomSenseProps, 
  sideRoom_objects
  )

val desk_props = SenseProps("The desk is wooden.","The desk smells like fresh pine needles.")
val desk = Item("desk",desk_props)

val floor_props = SenseProps("The floor is stone.","The floor smells of dust.")
val floor = Item("floor",floor_props)

val roomObjects = List(desk, floor)

val mainRoomSenseProps = SenseProps("You are in a room. There is a desk.\nThere is a door to the SideRoom.")
val mainRoom:Room = Room("MainRoom",mainRoomSenseProps, roomObjects)

/**
* Input a tuple -> (Room, List[AdjascentRooms])
*/

case class RoomDirectory(val roomMap:Map[Room,List[Room]])


val lint = Item("lint",SenseProps("Looks fuzzy.","Smells extremely dusty."))

case class Inventory(override val items:List[Item]) 
  extends HasItems(items, "That item is not in your inventory.") 

val inventory = Inventory(List(lint))

val mainDirectory = RoomDirectory(Map(
    (mainRoom,List(sideRoom)),
    (sideRoom,List(mainRoom))))
