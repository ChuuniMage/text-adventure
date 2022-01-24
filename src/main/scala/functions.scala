package txt_adv_functions
import txt_adv_classes.*

def validateInput = (input:String) => 
  val tokens = List.from(input.split(" ")) 
  ValidCommand.values.find(_.name == tokens.head) match 
    case Some(command) => Right((command, tokens.tail))
    case None => Left(RenderState(s"${tokens.head} is not a valid command."))

def renderState[A] = (render:RenderState[A]=>Unit) => (rData:RenderState[A]) => render(rData)

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
          case Some(dest) => (RenderState(s"You go to the ${dest.name}\n" + dest.sense(ValidCommand.Look) ),dState.map(state => TxtAdvState(state.player, dest,state.directory, state.metaCommand)))
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
        => (RenderState("Quitting game."), dState.map(state => TxtAdvState(state.player,state.room, state.directory, Some(MetaCommand.Quit))))
      case ValidCommand.Inventory 
        => (RenderState(command.notImplemented), dState)
      case ValidCommand.Go 
        => goCommand(dState, maybeName)
      case ValidCommand.Look | ValidCommand.Smell | ValidCommand.Taste | ValidCommand.Touch | ValidCommand.Hear
        => (senseItemCommand(senseCmdPayload),dState)
    case false => (RenderState("Too many commands! Type less things.\n"), dState)