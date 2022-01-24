package txt_adv_classes

enum MetaCommand:
  case Quit

case class TxtAdvState(val player:PlayerData, val room:Room,val directory:RoomDirectory, val metaCommand:Option[MetaCommand])

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
  val notImplemented = name + " is not implemented yet."

class SenseProps(val look:String = "You cannot see it with your eyes.", 
    val smell:String = "It doesn't have a smell.", 
    val taste:String = "It doesn't have a taste.", 
    val touch:String = "It cannot be touched.",
    val hear:String = "It doesn't make a sound.")

case class WorldItem(override val name:String,override val senseProps: SenseProps) extends Named(name), Sensible(senseProps)

case class InventoryItem(override val name:String,override val senseProps: SenseProps) extends Named(name), Sensible(senseProps)

type Item = WorldItem | InventoryItem

val honk = WorldItem("honk",SenseProps())
val honk2 = InventoryItem("honk2",SenseProps())

val listOfItem:List[Item] = List(honk,honk2)

val honkExtracted = listOfItem.filter(item => item.isInstanceOf[InventoryItem])

val honkEx = listOfItem.collect{case a:InventoryItem => a}

val honkEx2 = listOfItem.collect((a) => {
        a match 
            case a:InventoryItem => a
        })

val newList = List("String",5)

def pickupItem = (itemName:String, item:List[InventoryItem]) => {
    item.find(_.name == itemName) match
        case Some(item) => Right(item)
        case None => Left(s"You cannot pick up the ${itemName}.")
}

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

val newInventory = Inventory(List())
val honk23 = newInventory.removeItem


case class Room(
  override val name:String, 
  override val senseProps:SenseProps,
  override val items:List[Item]) 
    extends HasItems(items, "That item is not in this room."), Sensible(senseProps), Named(name) 

case class RoomDirectory(val roomMap:Map[Room,List[Room]])
