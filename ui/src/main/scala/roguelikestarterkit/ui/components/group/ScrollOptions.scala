package roguelikestarterkit.ui.components.group

final case class ScrollOptions(
    scrollMode: ScrollMode,
    scrollSpeed: Int
):

  def withScrollMode(scrollMode: ScrollMode): ScrollOptions =
    copy(scrollMode = scrollMode)

  def withScrollSpeed(scrollSpeed: Int): ScrollOptions =
    copy(scrollSpeed = scrollSpeed)

  def isScrollingEnabled: Boolean =
    scrollMode != ScrollMode.None

object ScrollOptions:
  val default: ScrollOptions = ScrollOptions(ScrollMode.None, 1)

enum ScrollMode:
  case None
  case Vertical
  case Horizontal
  case Both
