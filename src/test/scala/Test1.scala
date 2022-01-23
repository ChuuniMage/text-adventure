import org.junit.Assert.*
import org.junit.Test
import text_adventure._

class ValidateTests:
    println("Testing ValidateTests\n-----\n")
    def validationTest = (input:String, expect:Either[RenderState[String], (ValidCommand, List[String])]) => 
        assertEquals(validateInput(input), expect)
        println(s"$input validate test passed.")
    
    assertEquals("", "")
    @Test def t1 = validationTest("look room", Right((ValidCommand.Look, List("room"))))
    @Test def t2 = validationTest("sthjsdf", Left(RenderState("sthjsdf is not a valid command.")))
    

class CommandTests:
    val honk = mainRoom

    println("Testing Commands\n-----\n")
    val testLookString = "It looks like a test!"
    val testSmellString = "It smells like a test!"
    val testItem = Item("test",SenseProps("It looks like a test!","It smells like a test!"))
    val testRoom = Room("testRoom", SenseProps("This looks like the testing room, using SenseProps. TxtAdvState"), List(testItem))
    val testDirectory = RoomDirectory(Map((testRoom,List())))
    var dState = DataState(TxtAdvState(testRoom, testDirectory,None))

    def commandTest = (tuple:(ValidCommand,List[String]), expectedText:String) => 
        val newState = doActionInRoom(tuple,dState)
        println(newState._1.value)
        println(s"${tuple._1.name} command test passed.")

    @Test def t1 = commandTest((ValidCommand.Look, List("room")), testRoom.senseProps.look)
    @Test def t2 = commandTest((ValidCommand.Look, List("test")), testItem.senseProps.look)
    @Test def t3 = commandTest((ValidCommand.Smell, List("test")), testItem.senseProps.smell)


