import org.junit.Assert.*
import org.junit.Test
import text_adventure._

val honk = mainRoom

val msg = "I was compiled by Scala 3. :)"
val testLookString = "It looks like a test!"
val testSmellString = "It smells like a test!"
val testItem = Item("test",SenseProps("It looks like a test!","It smells like a test!"))

val testChicken = Item("chicken",SenseProps("It looks like a chicken!","It smells like a chicken!"))

val testRoom2 = Room("testRoom2", "This looks like the second testing room.", List(testChicken))

val testRoom = Room("testRoom", "This looks like the testing room.", List(testItem))
val testDirectory = RoomDirectory(List((testRoom,List(testRoom2))))

class Test1:
  @Test def t1(): Unit = 
    assertEquals("I was compiled by Scala 3. :)", msg)

class Look_Tests:
  println("TESTING FIRST BATCH")
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
  @Test def t4(): Unit = 
      val input = "go testRoom2"
      val newState = roomState_loop(input,state)
      val expectedText = s"You go to the ${testRoom2.name}\n" + testRoom2.description
      assertEquals(newState.tempData, expectedText)
      val newerState = roomState_loop("look chicken", newState)
      assertEquals(newerState.tempData, testChicken.senseProps.look)

