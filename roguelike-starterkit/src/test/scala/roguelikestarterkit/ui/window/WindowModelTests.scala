package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.*

class WindowModelTests extends munit.FunSuite {

  val charSheet = CharSheet(AssetName("test"), Size(10), Batch.empty, FontKey("test"))

  test("windows cannot be made smaller than the hard minimum") {
    val w = WindowModel(WindowId("a"), charSheet, ()).withBounds(Bounds(0, 0, 10, 10))

    assertEquals(w.resizeTo(1, 1).bounds.dimensions, Dimensions(3)) // No title, so 3x3
    assertEquals(
      w.withTitle("Test").resizeTo(1, 1).bounds.dimensions,
      Dimensions(3, 5)
    ) // With title, so 3x5
    assertEquals(w.resizeTo(6, 6).bounds.dimensions, Dimensions(6))
    assertEquals(w.resizeTo(6, 2).bounds.dimensions, Dimensions(6, 3))
  }

  test(
    "windows with a minimum size set, cannot be made smaller than that, or the hard value, whichever is larger"
  ) {
    val w = WindowModel(WindowId("a"), charSheet, ()).withBounds(Bounds(0, 0, 10, 10))
    assertEquals(w.noMinSize.resizeTo(6, 6).bounds.dimensions, Dimensions(6))
    assertEquals(w.withMinSize(9, 8).resizeTo(6, 6).bounds.dimensions, Dimensions(9, 8))
  }

  test("windows with a maximum size set, cannot be made larger than that") {
    val w = WindowModel(WindowId("a"), charSheet, ()).withBounds(Bounds(0, 0, 10, 10))
    assertEquals(w.noMaxSize.resizeTo(20, 20).bounds.dimensions, Dimensions(20))
    assertEquals(w.withMaxSize(12, 15).resizeTo(20, 20).bounds.dimensions, Dimensions(12, 15))
  }

}
