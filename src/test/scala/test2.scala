import org.junit.Assert.*
import org.junit.Test
import text_adventure2._




class Look_Tests_2:
    val honk = mainRoom

    println("TESTING SECOND BATCH")
    val testLookString = "It looks like a test!"
    val testSmellString = "It smells like a test!"
    val testItem = Item("test",SenseProps("It looks like a test!","It smells like a test!"))

    val testRoom = Room("testRoom", SenseProps("This looks like the testing room, using SenseProps."), List(testItem))
    val testDirectory = RoomDirectory(Map((testRoom,List())))
    var dState = DataState((testRoom, testDirectory))
        @Test def t1(): Unit = 
            val input = "look room"
            val newState = roomState_loop(input,dState)
            val expectedText = testRoom.sense(ValidCommand.Look) + "\n" + "What will you do? \n"
            assertEquals(newState._1.state, expectedText)
            println(newState._1.state)
        @Test def t2(): Unit = 
            val input = "look test"
            val newState = roomState_loop(input,dState)
            val expectedText = testItem.sense(ValidCommand.Look)
            assertEquals(newState._1.state, expectedText)
            assertEquals(newState._1.state, testLookString)
            println(newState._1.state)
        @Test def t3(): Unit = 
            val input = "smell test"
            val newState = roomState_loop(input,dState)
            val expectedText = testItem.sense(ValidCommand.Smell)
            assertEquals(newState._1.state, expectedText)
            assertEquals(newState._1.state, testSmellString)
            println(newState._1.state)

