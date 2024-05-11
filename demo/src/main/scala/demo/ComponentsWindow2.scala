package demo

import indigo.*
import roguelikestarterkit.*

object ComponentsWindow2:

  val windowId: WindowId = WindowId("ComponentsWindow2")

  def window(
      charSheet: CharSheet
  ): WindowModel[ComponentGroup[Int], Int] =
    WindowModel(
      windowId,
      charSheet,
      ComponentGroup(Bounds(0, 0, 23, 23))
        .withLayout(ComponentLayout.Vertical(Padding(0, 0, 1, 0)))
        .add(
          ComponentGroup(Bounds(0, 0, 23, 5))
            .withLayout(ComponentLayout.Horizontal(Padding(0, 1, 0, 0)))
            .add(
              Label("label 1", Label.Theme(charSheet)),
              Label("label 2", Label.Theme(charSheet)),
              Label("label 3", Label.Theme(charSheet))
            )
        )
        .add(
          ComponentGroup(Bounds(0, 0, 23, 5))
            .withLayout(ComponentLayout.Horizontal(Padding(0, 1, 0, 0)))
            .add(
              Batch(
                "History"  -> Batch(),
                "Controls" -> Batch(),
                "Quit"     -> Batch()
              ).map { case (label, clickEvents) =>
                Button(
                  label,
                  Button.Theme(charSheet)
                ).onClick(clickEvents)
              }
            )
        )
    )
      .withTitle("More component examples")
      .moveTo(2, 2)
      .resizeTo(25, 25)
      .isDraggable
      .isResizable
      .isCloseable
