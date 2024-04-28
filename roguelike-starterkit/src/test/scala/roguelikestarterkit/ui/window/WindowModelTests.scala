package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.component.*
import roguelikestarterkit.ui.datatypes.*

class WindowModelTests extends munit.FunSuite:

  test("model cascades bounds changes") {

    given WindowContent[Bounds] with
      def updateModel[StartupData, ContextData](
          context: UiContext[StartupData, ContextData],
          model: Bounds
      ): GlobalEvent => Outcome[Bounds] =
        _ => Outcome(model)

      def present[StartupData, ContextData](
          context: UiContext[StartupData, ContextData],
          model: Bounds
      ): Outcome[SceneUpdateFragment] =
        Outcome(SceneUpdateFragment.empty)

      def cascade(model: Bounds, newBounds: Bounds): Bounds =
        newBounds

    val charSheet =
      CharSheet(
        AssetName("test"),
        Size(10),
        Batch.empty,
        FontKey("test")
      )

    val bounds =
      Bounds(0, 0, 10, 10)

    val model: WindowModel[Unit, Unit, Bounds] =
      WindowModel(
        WindowId("test"),
        charSheet,
        bounds
      )

    assertEquals(model.withBounds(Bounds(10, 10, 20, 20)).contentModel, Bounds(11, 11, 18, 18))
  }

  val charSheet =
    CharSheet(
      AssetName("test"),
      Size(10),
      Batch.empty,
      FontKey("test")
    )

  val component = ComponentEntry(Coords(0, 0), "abc", summon[Component[String]])
  val group = ComponentGroup(
    Bounds(0, 0, 100, 100),
    BoundsType.Fixed,
    ComponentLayout.Horizontal(Padding(5), Overflow.Wrap),
    Batch(component)
  )

  test("model cascades bounds changes to component group - BoundsType.Fixed") {
    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(0, 0, 100, 100))
  }

  test("model cascades bounds changes to component group - BoundsType.Inherit") {
    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group.inheritBounds
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(1, 1, 38, 38))
  }

  test("model cascades bounds changes to component group - BoundsType.Relative") {
    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group.relative(0.5, 0.5, 0.25, 0.25)
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(19, 19, 9, 9))
  }

  test("model cascades bounds changes to component group - BoundsType.Relative 100%") {
    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group.relative(0, 0, 1, 1)
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(0, 0, 38, 38))
  }

  test("model cascades bounds changes to component group - BoundsType.RelativePosition") {
    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group.relativePosition(0.5, 0.5)
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(19, 19, 100, 100))
  }

  test("model cascades bounds changes to component group - BoundsType.RelativeSize") {
    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group.relativeSize(0.5, 0.5)
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(0, 0, 19, 19))
  }

  test("model cascades bounds changes to component group - BoundsType.Offset") {
    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group.offset(5, 5, -5, -5)
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(6, 6, 33, 33))
  }

  test("model cascades bounds changes to component group - BoundsType.OffsetPosition") {
    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group.offsetPosition(5, 5)
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(6, 6, 100, 100))
  }

  test("model cascades bounds changes to component group - BoundsType.OffsetSize") {
    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group.offsetSize(-5, -5)
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(0, 0, 33, 33))
  }

  test("model cascades bounds changes to nested component groups") {

    val group = ComponentGroup(
      Bounds(0, 0, 100, 100),
      BoundsType.Inherit,
      ComponentLayout.Horizontal(Padding(5), Overflow.Wrap),
      Batch(
        ComponentGroup(
          Bounds(0, 0, 100, 100),
          BoundsType.Inherit,
          ComponentLayout.Horizontal(Padding(5), Overflow.Wrap),
          Batch(component)
        )
      )
    )

    val model: WindowModel[Unit, Unit, ComponentGroup] =
      WindowModel(
        WindowId("test"),
        charSheet,
        group.inheritBounds
      ).withBounds(Bounds(0, 0, 40, 40))

    assertEquals(model.contentModel.bounds, Bounds(1, 1, 38, 38))
  }

  given Component[String] = new Component[String] {
    def bounds(model: String): Bounds = Bounds(0, 0, model.length, 1)

    def updateModel[StartupData, ContextData](
        context: UiContext[StartupData, ContextData],
        model: String
    ): GlobalEvent => Outcome[String] =
      _ => Outcome(model)

    def present[StartupData, ContextData](
        context: UiContext[StartupData, ContextData],
        model: String
    ): Outcome[ComponentFragment] =
      Outcome(ComponentFragment.empty)

    def reflow(model: String): String =
      model

    def cascade(model: String, parentBounds: Bounds): String =
      model
  }
