package roguelikestarterkit.ui.components.common

import roguelikestarterkit.ui.components.group.Padding
import roguelikestarterkit.ui.datatypes.Bounds

object ContainerLikeFunctions:

  extension (b: Bounds)
    def withPadding(p: Padding): Bounds =
      b.moveBy(p.left, p.top).resize(b.width + p.right, b.height + p.bottom)
