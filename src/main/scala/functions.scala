package txt_adv_functions
import txt_adv_classes._
import txt_adv_meta._
import txt_adv_classes.updateTxtAdvState
import txt_adv_classes.updateRoom

import javax.xml.crypto.Data

def validateInput = (input:String) => 
  val tokens = List.from(input.split(" ")) 
  ValidCommand.values.find(_.name == tokens.head) match 
    case Some(command) => Right((command, tokens.tail))
    case None => Left(RenderState(s"${tokens.head} is not a valid command."))

def renderState[A] = (render:RenderState[A]=>Unit) => (rData:RenderState[A]) => render(rData)

def senseItemCommand = (cmd:ValidCommand, maybeName:Option[String], sensibleItems:List[Sensible & Named]) => 
  maybeName match
    case None => s"What would you like to ${cmd.name}?"
    case Some(name) => sensibleItems.find(_.name == name) match
      case Some(item) => item.sense(cmd)
      case None => "That item is not in this room."


def goCommand (dState:DataState[TxtAdvState], destination:Option[String]): String|(String, DataState[TxtAdvState]) = 
  val currentRoom = dState.value.room
  val directory = dState.value.directory

  destination match
    case None => "Where would you like to go?"
    case Some(destName) =>  directory.roomMap(currentRoom).find(_.name == destName) match
      case None => "You cannot get there from this room."
      case Some(dest) => (s"You go to the ${dest.name}.\n" + dest.sense(ValidCommand.Look) ,dState.map(_.update(None,Some(dest),None,None)))

def doActionInRoom (command_tuple:(ValidCommand, List[String]), dState:DataState[TxtAdvState]):(RenderState[String], DataState[TxtAdvState]) =
  val room = dState.value.room
  val command = command_tuple._1
  val list = command_tuple._2
  val player = dState.value.player
  val inventory = dState.value.player.inventory
  
  lazy val sensibleItems:List[Sensible & Named] = inventory :: room :: room.items concat inventory.items

  lazy val maybeName = if list.isEmpty then None 
    else if list(0) == "room" then Some(room.name) else Some(list(0))

  val validLength = list.length <= 1
  val stringOrTuple:String|(String, DataState[TxtAdvState]) = validLength match
    case true => command match
      case ValidCommand.Quit 
        => ("Quitting game.", dState.map(_.update(None,None,None,Some(MetaCommand.Quit))))
      case ValidCommand.Inventory 
        => inventory.sense(ValidCommand.Look)
      case ValidCommand.Go 
        => goCommand(dState, maybeName)
      case ValidCommand.Look | ValidCommand.Smell | ValidCommand.Taste | ValidCommand.Touch | ValidCommand.Hear
        => senseItemCommand(command, maybeName, sensibleItems)
      case ValidCommand.Drop
        => dropCommand(dState,maybeName, inventory)
      case ValidCommand.Take
        => takeCommand(dState,maybeName,inventory)
    case false => "Too many commands! Type less things.\n"
  
  stringOrTuple match
    case str:String =>(RenderState(str), dState)
    case tuple:(String,DataState[TxtAdvState]) => (RenderState(tuple._1), tuple._2)

def takeCommand (dState:DataState[TxtAdvState], maybeName:Option[String], inventory:Inventory): String|(String, DataState[TxtAdvState]) = 
  val room = dState.value.room
  maybeName match
    case None => "What would you like to drop?"
    case Some(name) => room.getItem(name) match
      case Left(msg) => msg
      case Right(item) => 
        val maybeItem = room.inventoryItems.find(_.name == item.name)
        maybeItem match
          case Some(invItem) =>  
            val newPlayer = PlayerData(Inventory(invItem :: inventory.items))
            val newRoom = room.update(None,None,Some(room.removeItem(item)))
            val newDState:DataState[TxtAdvState] = dState.map(_.update(Some(newPlayer),Some(newRoom),None,None))
            (s"You take the ${invItem.name} from the ${room.name}.", newDState)
          case None => s"You cannot pick up the ${item.name}."



def dropCommand(dState:DataState[TxtAdvState], maybeName:Option[String], inventory:Inventory): String|(String, DataState[TxtAdvState]) = 
  val room = dState.value.room
  maybeName match      
    case None => "What would you like to drop?"
    case Some(name) => inventory.getItem(name) match
      case Left(msg) => msg
      case Right(item) =>  
        val newPlayer = PlayerData(Inventory(inventory.removeItem(item)))
        val newRoom = room.update(None,None,Some(item :: room.items))
        val newDState = dState.map(_.update(Some(newPlayer),Some(newRoom),None,None))
        (s"You drop the ${item.name} in the ${room.name}.", newDState)
          