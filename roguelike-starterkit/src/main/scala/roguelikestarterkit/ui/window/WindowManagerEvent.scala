package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions

enum WindowManagerEvent extends GlobalEvent:
  case Close(id: WindowId)
  case GiveFocusAt(coords: Coords)
  case Open(id: WindowId)
  case OpenAt(id: WindowId, coords: Coords)
  case Move(id: WindowId, position: Coords)
  case Resize(id: WindowId, dimensions: Dimensions)
  case Transform(id: WindowId, bounds: Bounds)
  case ChangeMagnification(newMagnification: Int)
