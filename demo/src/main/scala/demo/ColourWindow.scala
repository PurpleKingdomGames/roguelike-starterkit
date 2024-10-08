package demo

import indigo.*
import roguelikestarterkit.*
import roguelikestarterkit.syntax.*

object ColourWindow:

  final case class ColorPaletteReference(name: String, count: Int, colors: Batch[RGBA])

  val outrunner16 = ColorPaletteReference(
    "outrunner-16",
    16,
    Batch(
      RGBA.fromHexString("4d004c"),
      RGBA.fromHexString("8f0076"),
      RGBA.fromHexString("c70083"),
      RGBA.fromHexString("f50078"),
      RGBA.fromHexString("ff4764"),
      RGBA.fromHexString("ff9393"),
      RGBA.fromHexString("ffd5cc"),
      RGBA.fromHexString("fff3f0"),
      RGBA.fromHexString("000221"),
      RGBA.fromHexString("000769"),
      RGBA.fromHexString("00228f"),
      RGBA.fromHexString("0050c7"),
      RGBA.fromHexString("008bf5"),
      RGBA.fromHexString("00bbff"),
      RGBA.fromHexString("47edff"),
      RGBA.fromHexString("93fff8")
    )
  )

  val windowId: WindowId = WindowId("Color palette")

  def window(
      charSheet: CharSheet
  ): WindowModel[ColorPalette, Unit] =
    WindowModel(
      windowId,
      charSheet,
      ColorPalette(
        ComponentGroup()
          .withLayout(ComponentLayout.Vertical(Padding.zero.withBottom(1)))
          .add(
            ComponentGroup()
              .withLayout(ComponentLayout.Horizontal(Overflow.Wrap))
              .add(
                // Custom rendered buttons for the swatches
                outrunner16.colors.map { rgba =>
                  Button(Bounds(0, 0, 3, 3))(presentSwatch(charSheet, rgba, None))
                    // .onClick(<Emit some event...>)
                    .presentOver(presentSwatch(charSheet, rgba, Option(RGBA.White)))
                    .presentDown(presentSwatch(charSheet, rgba, Option(RGBA.Black)))
                }
              )
          )
          .add(
            // Default button renderer
            Button(
              "Load palette",
              Button.Theme(
                charSheet,
                RGBA.Silver -> RGBA.Black,
                RGBA.White  -> RGBA.Black,
                RGBA.Black  -> RGBA.White,
                hasBorder = true
              )
            )
          )
      )
    )
      .withTitle("Colour Palette")
      .moveTo(0, 0)
      .resizeTo(25, 25)
      .isDraggable
      .isResizable
      .isCloseable

  def presentSwatch(
      charSheet: CharSheet,
      colour: RGBA,
      stroke: Option[RGBA]
  ): (Coords, Bounds, Unit) => Outcome[ComponentFragment] =
    (offset, bounds, _) =>
      Outcome(
        ComponentFragment(
          stroke match
            case None =>
              Shape.Box(
                Rectangle(
                  offset.toScreenSpace(charSheet.size),
                  bounds.dimensions.toScreenSpace(charSheet.size)
                ),
                Fill.Color(colour)
              )

            case Some(strokeColor) =>
              Shape.Box(
                Rectangle(
                  offset.toScreenSpace(charSheet.size),
                  bounds.dimensions.toScreenSpace(charSheet.size)
                ),
                Fill.Color(colour),
                Stroke(2, strokeColor)
              )
        )
      )

final case class ColorPalette(componentGroup: ComponentGroup[Unit])
object ColorPalette:

  given WindowContent[ColorPalette, Unit] with

    def updateModel(
        context: UIContext[Unit],
        model: ColorPalette
    ): GlobalEvent => Outcome[ColorPalette] =
      case e =>
        model.componentGroup.update(context)(e).map { c =>
          model.copy(componentGroup = c)
        }

    def present(
        context: UIContext[Unit],
        model: ColorPalette
    ): Outcome[Layer] =
      model.componentGroup.present(context).map(_.toLayer)

    def refresh(reference: Unit, model: ColorPalette, contentDimensions: Dimensions): ColorPalette =
      model.copy(
        componentGroup = model.componentGroup.refresh(reference, contentDimensions)
      )
