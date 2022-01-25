package txt_adv_meta
import txt_adv_classes._

extension [A,B](opt:Option[A]) def ?? (b:B):A|B = opt getOrElse b

/**
* Returns a new object, with every Some() field updated, 
* and every None field takes from the previous params 
*
    */  
extension [T](a:T)(using gUpdate:Update[T])
    def update = gUpdate.update(a)

type UpdateParams[T] = T match
    case TxtAdvState => (Option[PlayerData], Option[Room],Option[RoomDirectory], Option[MetaCommand])
    case Room => (Option[String], Option[SenseProps], Option[List[Item]]) 

case class Person(val firstName:String, val lastName:String)

trait Update[T]:
    def update:T => UpdateParams[T] => T
