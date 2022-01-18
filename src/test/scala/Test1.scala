import org.junit.Assert.*
import org.junit.Test
import text_adventure._

val honk = mainRoom

val msg = "I was compiled by Scala 3. :)"
val testLookString = "It looks like a test!"
val testSmellString = "It smells like a test!"
val testItem = Item("test",SenseProps("It looks like a test!","It smells like a test!"))

val testRoom = Room("testRoom", "This looks like the testing room.", List(testItem))
val testDirectory = RoomDirectory(List((testRoom,List())))

class Test1:
  @Test def t1(): Unit = 
    assertEquals("I was compiled by Scala 3. :)", msg)

class Look_Tests:
  var state = State("",(testRoom, testDirectory))
  @Test def t1(): Unit = 
      val input = "look room"
      val newState = roomState_loop(input,state)
      val expectedText = testRoom.description + "\n" + "What will you do? \n"
      assertEquals(newState.tempData, expectedText)
  @Test def t2(): Unit = 
      val input = "look test"
      val newState = roomState_loop(input,state)
      val expectedText = testItem.sense(ValidCommand.Look)
      assertEquals(newState.tempData, expectedText)
      assertEquals(newState.tempData, testLookString)
  @Test def t3(): Unit = 
      val input = "smell test"
      val newState = roomState_loop(input,state)
      val expectedText = testItem.sense(ValidCommand.Smell)
      assertEquals(newState.tempData, expectedText)
      assertEquals(newState.tempData, testSmellString)

