package roguelikestarterkit.ui.component

import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions

/** Describes how a ComponentGroup responds to changes in its parents bounds.
  */
enum BoundsType:

  /** The component group ignores parent bounds changes
    */
  case Fixed

  /** The component group uses its parents bounds
    */
  case Inherit

  /** The component group positions and resizes itself within / based on the parents bounds
    * according to percentages expressed as a value from 0.0 to 1.0, e.g. 50% is 0.5
    */
  case Relative(x: Double, y: Double, width: Double, height: Double)

  /** The component group positions itself within the parents bounds according to percentages
    * expressed as a value from 0.0 to 1.0, e.g. 50% is 0.5
    */
  case RelativePosition(x: Double, y: Double)

  /** The component group resizes itself based on the parents bounds according to percentages
    * expressed as a value from 0.0 to 1.0, e.g. 50% is 0.5
    */
  case RelativeSize(width: Double, height: Double)

  /** The component group positions and resizes itself within / based on the parents bounds, offset
    * by the amounts given.
    */
  case Offset(coords: Coords, dimensions: Dimensions)

  /** The component group positions itself within the parents bounds, offset by the amount given.
    */
  case OffsetPosition(coords: Coords)

  /** The component group resizes itself based on the parents bounds, offset by the amount given.
    */
  case OffsetSize(dimensions: Dimensions)