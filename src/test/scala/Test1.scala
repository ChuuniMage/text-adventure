import org.junit.Assert.*
import org.junit.Test
import text_adventure._

val honk = mainRoom

val msg = "I was compiled by Scala 3. :)"
class Test1:
  @Test def t1(): Unit = 
    assertEquals("I was compiled by Scala 3. :)", msg)

class Look_Tests:
  var state = State("",(mainRoom, mainDirectory))
  @Test def t1(): Unit = 
      val input = "look room"
      val newState = roomState_loop(input,state)
      val expectedText = mainRoom.description + "\n" + "What will you do? \n"
      assertEquals(newState.tempData, expectedText)
  @Test def t2(): Unit = 
      val input = "look floor"
      val newState = roomState_loop(input,state)
      val expectedText = floor.sense.look
      assertEquals(newState.tempData, expectedText)
  @Test def t3(): Unit = 
      val input = "smell floor"
      val newState = roomState_loop(input,state)
      val expectedText = floor.sense.smell
      assertEquals(newState.tempData, expectedText)

