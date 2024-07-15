package roguelikestarterkit.ui.components.group

import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions

/** Describes how a ComponentGroup responds to changes in its parents bounds.
  */
final case class BoundsMode(width: FitMode, height: FitMode)

object BoundsMode:

  val default: BoundsMode =
    BoundsMode(FitMode.Available, FitMode.Content)

  def fixed(dimensions: Dimensions): BoundsMode =
    BoundsMode(
      FitMode.Fixed(dimensions.width),
      FitMode.Fixed(dimensions.height)
    )

  def fixed(width: Int, height: Int): BoundsMode =
    fixed(Dimensions(width, height))

  def available: BoundsMode =
    BoundsMode(FitMode.Available, FitMode.Available)
  def inherit: BoundsMode =
    available

  def offset(offsetWidth: Int, offsetHeight: Int): BoundsMode =
    BoundsMode(FitMode.Offset(offsetWidth), FitMode.Offset(offsetHeight))

  def fit: BoundsMode =
    BoundsMode(FitMode.Content, FitMode.Content)

  def halfHorizontal: BoundsMode =
    BoundsMode(
      FitMode.Relative(0.5),
      FitMode.Available
    )

  def halfVertical: BoundsMode =
    BoundsMode(
      FitMode.Available,
      FitMode.Relative(0.5)
    )

  def relative(relativeWidth: Double, relativeHeight: Double): BoundsMode =
    BoundsMode(
      FitMode.Relative(relativeWidth),
      FitMode.Relative(relativeHeight)
    )
