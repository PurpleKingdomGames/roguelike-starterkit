package roguelikestarterkit.ui.components

import roguelikestarterkit.ui.components.datatypes.Padding
import roguelikestarterkit.ui.datatypes.Bounds

enum BoundsType[ReferenceData, A]:
  case Fixed(bounds: Bounds)
  case Calculated(calculate: (ReferenceData, A) => Bounds)
  case FillWidth(height: Int, padding: Padding)
  case FillHeight(width: Int, padding: Padding)
  case Fill(padding: Padding)

object BoundsType:

  object Calculated:
    def apply[ReferenceData](f: ReferenceData => Bounds): BoundsType[ReferenceData, Unit] =
      BoundsType.Calculated((ref, _) => f(ref))
