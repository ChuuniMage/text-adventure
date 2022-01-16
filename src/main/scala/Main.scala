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

enum ValidCommand(val name:String, val errorMessage:Option[String] ){ 
  case Look extends ValidCommand("look",Some("What will you look at?"))
  case Smell extends ValidCommand("smell", Some("What will you smell?"))
  case Go extends ValidCommand("go", Some("Where will you go?"))
  case Inventory extends ValidCommand("inventory", None)
  case Exit extends ValidCommand("exit",None)
}

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
          then State(ValidCommand.Look.errorMessage.get,state.stateObjects)
          else if list(0) == "room" || list(0) == room.name
          then State(room.description + "\n" + "What will you do? \n", state.stateObjects) 
          else State(room.actOnItem(command_tuple),state.stateObjects)
          // else State("I'm a big stupid gorilla.",state.stateObjects)
    case ValidCommand.Smell => if list.length == 0 
      then State(ValidCommand.Smell.errorMessage.get,state.stateObjects)
      else State(room.actOnItem(command_tuple),state.stateObjects)
  

}


def actOnItemInList_generic = (itemNotFound:String) => (items:List[Item]) => 
  (command_tuple:(ValidCommand, List[String])) => 
    val command = command_tuple(0)
    val possibleItemName = command_tuple(1)(0)
    items.find(_.name == possibleItemName) match 
            case Some(item) => command match 
                case ValidCommand.Look => item.sense.look
                case ValidCommand.Smell => item.sense.smell
                case _ => "Why are you trying to sense an item with a non-sense command?"
            case None => itemNotFound
    

class SenseProps(val look:String = "You cannot see it with your eyes.", 
    val smell:String = "It doesn't have a smell.", 
    val taste:String = "It doesn't have a taste.", 
    val touch:String = "It cannot be touched.",
    val hear:String = "It doesn't make a sound.")

class Item(val name:String,val sense: SenseProps)

class Room(
  val name:String, 
  val description:String,
  val items:List[Item]) {
    def actOnItem = actOnItemInList_generic("That object is not in this room.")(items)
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

val floor_props = SenseProps("look", "The floor is stone.","smell","The floor smells of dust.")
val floor = Item("floor",floor_props)

val roomObjects = List(desk, floor)

val mainRoom:Room = Room("MainRoom","You are in a room. There is a desk.\nThere is a door to the SideRoom.", roomObjects)

/**
* Input a tuple -> (Room, List[AdjascentRooms])
*/
class RoomDirectory(val roomList:List[(Room,List[Room])]){

  def navigate = (from:Room,to:String) => 

       roomList.find(_._1.name == from.name) match 
        case Some(roomTuple) => roomTuple._2.find(_.name == to) match 
          case Some(room) =>  State(s"You go to the ${room.name}\n" + room.description,(room, this))
          case None => State("You cannot get there from this room.", (from,this))

        case None => State("How did you get here?",(from,this))
}

val lint = Item("lint",SenseProps("Looks fuzzy.","smell","Smells extremely dusty."))

class Inventory(val itemList:List[Item]) {
  def actOnItem = actOnItemInList_generic("You do not have that item in your inventory.")(itemList)
}

val inventory = Inventory(List(lint))

val mainDirectory = RoomDirectory(List(
    (mainRoom,List(sideRoom)),
    (sideRoom,List(mainRoom))))

