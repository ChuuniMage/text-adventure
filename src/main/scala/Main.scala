//https://www.scala-lang.org/api/2.12.7/scala/collection/immutable/StringOps.html

package text_adventure

import txt_adv_classes.*
import txt_adv_functions.*
import zio.*

import scala.compiletime.ops.string
import scala.io.StdIn

@main def hello: Unit = 
  val renderTextAdventurePrint:RenderState[String] => Unit = renderState((state) => println(state.value + "\n" + "-----"))

  var rState = RenderState(mainRoom.sense(ValidCommand.Look))
  var dState = DataState(TxtAdvState(player, mainRoom,mainDirectory,None))
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
    renderTextAdventurePrint(rState)
    dState.value.metaCommand match
      case Some(cmd) => cmd match
        case MetaCommand.Quit => running = false
      case None => input = StdIn.readLine()

val barrel_props = SenseProps(
  "The barrel looks round.",
  "Unmistakable scent of a barrel that was once filled with rich, red wine.")
                        
val barrel = WorldItem("barrel", barrel_props)
val sideRoom_objects = List(barrel)

val sideRoomSenseProps = SenseProps("This is a side room. It is damp, and there is a barrel here.\nThere is a door to the MainRoom")
val sideRoom = Room("SideRoom", sideRoomSenseProps, sideRoom_objects)

val desk_props = SenseProps("The desk is wooden.","The desk smells like fresh pine needles.")
val desk = WorldItem("desk",desk_props)

val floor_props = SenseProps("The floor is stone.","The floor smells of dust.")
val floor = WorldItem("floor",floor_props)

val yeFlask = InventoryItem("yeFlask", SenseProps())

val roomObjects:List[Item] = List(desk, floor, yeFlask)

val mainRoomSenseProps = SenseProps("You are in a room.\nThere is a desk.\nThere is a door to the SideRoom.")
val mainRoom:Room = Room("MainRoom",mainRoomSenseProps, roomObjects)

val lint = InventoryItem("lint",SenseProps("Looks fuzzy.","Smells extremely dusty."))

val inventory = Inventory(List(lint))
val player = PlayerData(inventory)

val mainDirectory = RoomDirectory(Map(
    (mainRoom,List(sideRoom)),
    (sideRoom,List(mainRoom))))
