package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.datatypes.Coords

/** Internal events */
enum WindowInternalEvent extends GlobalEvent:
  case Redraw
