package txt_adv_classes

import txt_adv_meta._

enum MetaCommand:
  case Quit

case class TxtAdvState(val player:PlayerData, val room:Room,val directory:RoomDirectory, val metaCommand:Option[MetaCommand])

given updateTxtAdvState:Update[TxtAdvState] with
    def update = (state:TxtAdvState) => (nPlayer:Option[PlayerData], nRoom:Option[Room], nDirect:Option[RoomDirectory],nMeta:Option[Option[MetaCommand]]) => 
    TxtAdvState(nPlayer ?? state.player, nRoom ?? state.room, nDirect ?? state.directory, nMeta ?? state.metaCommand)


case class Inventory(override val items:List[InventoryItem]) 
  extends HasItems(items, "That item is not in your inventory."), Named("inventory"),
  Sensible(SenseProps({
      val folded = if items.length == 0 
        then "- Nothing\n" 
        else items.map(item => s"- ${item.name}\n").fold("")((tally,elem) => tally + elem)
      "You are carrying:\n" + folded}
        ))


case class PlayerData(val inventory:Inventory)


case class RenderState[A](val value:A)

case class DataState[A](val value:A):
  def map[B] (f: A => B) = DataState(f(value))

enum ValidCommand(override val name:String) extends Named(name):
  case Look extends ValidCommand("look")
  case Smell extends ValidCommand("smell")
  case Hear extends ValidCommand("hear")
  case Touch extends ValidCommand("touch")
  case Taste extends ValidCommand("taste")
  case Go extends ValidCommand("go")
  case Inventory extends ValidCommand("inventory")
  case Quit extends ValidCommand("quit")
  case Drop extends ValidCommand("drop")
  case Take extends ValidCommand("take")
  val notImplemented = name + " is not implemented yet."

class SenseProps(val look:String = "You cannot see it with your eyes.", 
    val smell:String = "It doesn't have a smell.", 
    val taste:String = "It doesn't have a taste.", 
    val touch:String = "It cannot be touched.",
    val hear:String = "It doesn't make a sound.")

case class WorldItem(override val name:String,override val senseProps: SenseProps) extends Named(name), Sensible(senseProps)

case class InventoryItem(override val name:String,override val senseProps: SenseProps) extends Named(name), Sensible(senseProps)

type Item = WorldItem | InventoryItem

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

//Cool type thing I learned: `Class[A <: B]` is "type A where it is a subtype of B"
trait HasItems[A <: Item](val items:List[A], val itemNotFoundMsg:String):
  val getItem = (name:String) => items.find(_.name == name) match 
      case Some(item) => Right(item)
      case None => Left(itemNotFoundMsg)
  val removeItem = (removedItem:A) => items.filter(item => item != removedItem)


case class Room(
  override val name:String, 
  override val senseProps:SenseProps,
  override val items:List[Item]) 
    extends HasItems(items, "That item is not in this room."), Sensible(senseProps), Named(name) 
    {
      val inventoryItems = items.collect{case a:InventoryItem => a}
    }

case class RoomDirectory(val roomMap:Map[Room,List[Room]])
