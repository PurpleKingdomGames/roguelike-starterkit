package demo.windows

import indigo.*
import roguelikestarterkit.*
import roguelikestarterkit.syntax.*
import roguelikestarterkit.ui.components.ComponentGroup
import roguelikestarterkit.ui.components.TerminalButton
import roguelikestarterkit.ui.components.TerminalLabel
import roguelikestarterkit.ui.components.datatypes.ComponentLayout
import roguelikestarterkit.ui.components.datatypes.Padding
import roguelikestarterkit.ui.window.TerminalWindow

object ComponentsWindow:

  private val graphic = Graphic(0, 0, TerminalMaterial(AssetName(""), RGBA.White, RGBA.Black))

  val windowId: WindowId = WindowId("ComponentsWindow")

  def window(
      charSheet: CharSheet
  ): Window[ComponentGroup[Int], Int] =
    TerminalWindow(
      windowId,
      charSheet,
      ComponentGroup()
        .withLayout(ComponentLayout.Vertical(Padding(0, 0, 1)))
        .add(
          TerminalButton(
            "Hello!",
            TerminalButton.Theme(
              charSheet,
              RGBA.Silver -> RGBA.Black,
              RGBA.White  -> RGBA.Black,
              RGBA.Black  -> RGBA.White,
              hasBorder = true
            )
          )
        )
        .add(
          TerminalButton(
            "World!",
            TerminalButton.Theme(
              charSheet,
              RGBA.Silver -> RGBA.Black,
              RGBA.Green  -> RGBA.Black,
              RGBA.Black  -> RGBA.Yellow,
              hasBorder = false
            )
          ),
          TerminalButton("Default!", TerminalButton.Theme(charSheet))
        )
        .add {
          TerminalButton(
            (i: Int) => "Count" + (if i > 0 then s": $i" else ""),
            TerminalButton.Theme(charSheet).addBorder
          )
        }
        .add(
          Button[Int](Bounds(0, 0, 5, 2)) { case (coords, bounds, _) =>
            Outcome(
              Layer(
                Shape.Box(
                  bounds.toScreenSpace(charSheet.size).moveTo(coords.toScreenSpace(charSheet.size)),
                  Fill.LinearGradient(Point.zero, RGBA.Cyan, Point(50, 0), RGBA.Magenta)
                )
              )
            )
          }
        )
        .add(
          Label[Int]("Custom rendered label", (_, t) => Bounds(0, 0, t.length, 1)) {
            case (offset, label, dimensions) =>
              val size = dimensions.unsafeToSize

              val terminal =
                RogueTerminalEmulator(size)
                  .putLine(Point.zero, label, RGBA.Red, RGBA.Zero)
                  .toCloneTiles(
                    CloneId("label"),
                    offset.toScreenSpace(charSheet.size),
                    charSheet.charCrops
                  ) { case (fg, bg) =>
                    graphic.withMaterial(TerminalMaterial(charSheet.assetName, fg, bg))
                  }

              Outcome(Layer.Content(terminal))
          }
        )
        .add(
          TerminalLabel(
            "Terminal rendered label",
            TerminalLabel.Theme(charSheet, RGBA.Magenta, RGBA.Cyan)
          ),
          TerminalLabel("Default theme", TerminalLabel.Theme(charSheet)),
          TerminalLabel(
            (count: Int) => "Pointer over windows: " + count,
            TerminalLabel.Theme(charSheet)
          )
        )
    )
      .moveTo(0, 3)
      .resizeTo(25, 25)
