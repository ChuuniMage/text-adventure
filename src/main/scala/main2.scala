//https://www.scala-lang.org/api/2.12.7/scala/collection/immutable/StringOps.html

package text_adventure2

import zio.*

import scala.compiletime.ops.string
import scala.io.StdIn

@main def hello: Unit = 
  val renderTextAdventurePrint = renderState((str:String) => println(str + "\n" + "-----"))

  var rState = RenderState(mainRoom.sense(ValidCommand.Look))
  var dState = DataState(TxtAdvState(mainRoom,mainDirectory,None))
  var tuple = (rState, dState)
  var input = "look room"
  var running = true
  var vInput:Either[RenderState[String], (ValidCommand, List[String])] = Left(rState)
  var doGameAction = doActionInRoom
  while (running) 
    vInput = validateInput(input)
    tuple = vInput match 
      case Right(input) => doGameAction(input, dState)
      case Left(rState) => (rState, dState)
    dState = tuple._2;
    rState = tuple._1
    renderTextAdventurePrint(rState.value)
    dState.value.metaCommand match
      case Some(cmd) => cmd match
        case MetaCommand.Quit => running = false
      case None => input = StdIn.readLine()

enum MetaCommand:
  case Quit

case class TxtAdvState(val room:Room,val directory:RoomDirectory, val metaCommand:Option[MetaCommand])

def renderState[A] = (render:A=>Unit) => (rData:A) => render(rData)

case class RenderState[A](val value:A)

case class DataState[A](val value:A):
  def map[B] (f: A => B) = DataState(f(value))

def validateInput = (input:String) => 
  val tokens = List.from(input.split(" ")) 
  ValidCommand.values.find(_.name == tokens.head) match 
    case Some(command) => Right((command, tokens.tail))
    case None => Left(RenderState(s"${tokens.head} is not a valid command."))

enum ValidCommand(override val name:String) extends Named(name):
  case Look extends ValidCommand("look")
  case Smell extends ValidCommand("smell")
  case Hear extends ValidCommand("hear")
  case Touch extends ValidCommand("touch")
  case Taste extends ValidCommand("taste")
  case Go extends ValidCommand("go")
  case Inventory extends ValidCommand("inventory")
  case Quit extends ValidCommand("quit")
  val notImplemented = name + " is not implemented yet."

def senseItemCommand = (args:(ValidCommand, Option[String], List[Sensible & Named])) => 
  val cmd = args._1
  val itemName = args._2
  val sensibleItems = args._3

  val renderString = itemName match
    case Some(name) => sensibleItems.find(_.name == name) match
      case Some(item) => item.sense(cmd)
      case None => "That item is not in this room."
    case None => s"What would you like to ${cmd.name}?"
    RenderState(renderString)

def goCommand = (dState:DataState[TxtAdvState], destination:Option[String]) =>
  val currentRoom = dState.value.room
  val directory = dState.value.directory

  destination match
    case Some(destName) =>  directory.roomMap(currentRoom).find(_.name == destName) match
          case Some(dest) => (RenderState(s"You go to the ${dest.name}\n" + dest.sense(ValidCommand.Look) ),dState.map(state => TxtAdvState(dest,state.directory, state.metaCommand)))
          case None => (RenderState("You cannot get there from this room." ),dState)
    case None => (RenderState("Where would you like to go?" ),dState)


def doActionInRoom = (command_tuple:(ValidCommand, List[String]), dState:DataState[TxtAdvState]) => 
  val room = dState.value.room
  val command = command_tuple._1
  val list = command_tuple._2
  
  lazy val sensibleItems:List[Sensible & Named] = room :: room.items
  lazy val maybeName = if list.isEmpty then None 
    else if list(0) == "room" then Some(room.name) else Some(list(0))

  lazy val senseCmdPayload = (command, maybeName, sensibleItems)
  val validLength = list.length <= 1
  validLength match
    case true => command match
      case ValidCommand.Quit 
        => (RenderState("Quitting game."), dState.map(state => TxtAdvState(state.room, state.directory, Some(MetaCommand.Quit))))
      case ValidCommand.Inventory 
        => (RenderState(command.notImplemented), dState)
      case ValidCommand.Go 
        => goCommand(dState, maybeName)
      case ValidCommand.Look | ValidCommand.Smell | ValidCommand.Taste | ValidCommand.Touch | ValidCommand.Hear
        => (senseItemCommand(senseCmdPayload),dState)
    case false => (RenderState("Too many commands! Type less things.\n"), dState)
    
class SenseProps(val look:String = "You cannot see it with your eyes.", 
    val smell:String = "It doesn't have a smell.", 
    val taste:String = "It doesn't have a taste.", 
    val touch:String = "It cannot be touched.",
    val hear:String = "It doesn't make a sound.")

case class Item(override val name:String,override val senseProps: SenseProps) extends Named(name), Sensible(senseProps)

trait Named(val name:String)

//TODO: Have some "fundamental" command typing
//Maybe case classes instead of enums, or enums at the bottom with case classes holding them
trait Sensible(val senseProps:SenseProps):
  def sense = (cmd:ValidCommand) => cmd match
    case ValidCommand.Look => senseProps.look
    case ValidCommand.Smell => senseProps.smell
    case ValidCommand.Hear => senseProps.hear
    case ValidCommand.Touch => senseProps.touch
    case ValidCommand.Taste => senseProps.taste
    case _ => "That's not a sense!" // Me no likey.

trait HasItems(val items:List[Item], val itemNotFoundMsg:String):
  val getItem = (name:String) => items.find(_.name == name) match 
      case Some(item) => Right(item)
      case None => Left(itemNotFoundMsg)

case class Room(
  override val name:String, 
  override val senseProps:SenseProps,
  override val items:List[Item]) 
    extends HasItems(items, "That item is not in this room."), Sensible(senseProps), Named(name) 

val barrel_props = SenseProps(
  "The barrel looks round.",
  "Unmistakable scent of a barrel that was once filled with rich, red wine.")
                        
val barrel = Item("barrel", barrel_props)
val sideRoom_objects = List(barrel)

val sideRoomSenseProps = SenseProps("This is a side room. It is damp, and there is a barrel here.\nThere is a door to the MainRoom")
val sideRoom = Room("SideRoom", sideRoomSenseProps, sideRoom_objects)

val desk_props = SenseProps("The desk is wooden.","The desk smells like fresh pine needles.")
val desk = Item("desk",desk_props)

val floor_props = SenseProps("The floor is stone.","The floor smells of dust.")
val floor = Item("floor",floor_props)

val roomObjects = List(desk, floor)

val mainRoomSenseProps = SenseProps("You are in a room. There is a desk.\nThere is a door to the SideRoom.")
val mainRoom:Room = Room("MainRoom",mainRoomSenseProps, roomObjects)

case class RoomDirectory(val roomMap:Map[Room,List[Room]])

case class Inventory(override val items:List[Item]) 
  extends HasItems(items, "That item is not in your inventory.") 

val lint = Item("lint",SenseProps("Looks fuzzy.","Smells extremely dusty."))

val mainDirectory = RoomDirectory(Map(
    (mainRoom,List(sideRoom)),
    (sideRoom,List(mainRoom))))
