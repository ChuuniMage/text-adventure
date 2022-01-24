package txt_adv_meta

import txt_adv_classes._


extension [A,B](opt:Option[A]) def ?? (b:B):A|B = opt getOrElse b

class Foo(val fooStr:String,val fooInt:Int)
class Person(val firstName:String, val lastName:String)

val honk = ("", 2)

type UpdateParams[T] = T match
    case Foo => (Option[String],Option[Int]) 
    case Person => (Option[String],Option[String])
    case TxtAdvState => (Option[PlayerData], Option[Room],Option[RoomDirectory], Option[Option[MetaCommand]])

trait Update[T]:
    def update:T => UpdateParams[T] => T

given updateFoo:Update[Foo] with
    def update = (f:Foo) => (newStr:Option[String], newInt:Option[Int]) => 
    Foo(newStr ?? f.fooStr, 
        newInt ?? f.fooInt)

given updatePerson:Update[Person] with
    def update = (p:Person) => (fName:Option[String], lName:Option[String]) => 
    Person(fName ?? p.firstName, 
        lName ?? p.lastName)

/**
* Returns a new object, with every Some() field updated, 
* and every None field takes from the previous params 
*
    */  
extension [T](a:T)(using makeNew:Update[T]) {
    def update = makeNew.update(a)
}

val newFoo = Foo("new Foo String",2)
val gronkFoo = newFoo.update(Some(""),Some(3))
//Updates both fields

val newPerson = Person("John","Jenkins")
val updatedPerson = newPerson.update(Some("Leeroy"),None)
//only updates first field