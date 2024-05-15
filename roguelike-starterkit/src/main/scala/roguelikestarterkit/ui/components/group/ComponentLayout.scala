package roguelikestarterkit.ui.components.group

/** `ComponentLayout` instructs a `ComponentGroup` how it should layout the components it contains.
  * They are always placed one after another, optionally with some padding unless the layout type is
  * `None`.
  */
enum ComponentLayout:
  case None
  case Horizontal(padding: Padding, overflow: Overflow)
  case Vertical(padding: Padding)

object ComponentLayout:

  object Horizontal:
    def apply(): Horizontal =
      Horizontal(Padding.zero, Overflow.Hidden)
    def apply(padding: Padding): Horizontal =
      Horizontal(padding, Overflow.Hidden)
    def apply(overflow: Overflow): Horizontal =
      Horizontal(Padding.zero, overflow)

    extension (h: Horizontal)
      def withPadding(value: Padding): Horizontal   = h.copy(padding = value)
      def withOverflow(value: Overflow): Horizontal = h.copy(overflow = value)

  object Vertical:
    def apply(): Vertical =
      Vertical(Padding.zero)

    extension (h: Vertical) def withPadding(value: Padding): Vertical = h.copy(padding = value)

/** Describes the padding between components.
  */
final case class Padding(top: Int, right: Int, bottom: Int, left: Int):
  def withTop(amount: Int): Padding        = this.copy(top = amount)
  def withRight(amount: Int): Padding      = this.copy(right = amount)
  def withBottom(amount: Int): Padding     = this.copy(bottom = amount)
  def withLeft(amount: Int): Padding       = this.copy(left = amount)
  def withHorizontal(amount: Int): Padding = this.copy(right = amount, left = amount)
  def withVerticl(amount: Int): Padding    = this.copy(top = amount, bottom = amount)

object Padding:
  def apply(amount: Int): Padding =
    Padding(amount, amount, amount, amount)
  def apply(topAndBottom: Int, leftAndRight: Int): Padding =
    Padding(topAndBottom, leftAndRight, topAndBottom, leftAndRight)
  def apply(top: Int, leftAndRight: Int, bottom: Int): Padding =
    Padding(top, leftAndRight, bottom, leftAndRight)

  val zero: Padding = Padding(0)
  val one: Padding  = Padding(1)

  def top(amount: Int): Padding        = Padding(amount, 0, 0, 0)
  def right(amount: Int): Padding      = Padding(0, amount, 0, 0)
  def bottom(amount: Int): Padding     = Padding(0, 0, amount, 0)
  def left(amount: Int): Padding       = Padding(0, 0, 0, amount)
  def horizontal(amount: Int): Padding = Padding(0, amount, 0, amount)
  def verticl(amount: Int): Padding    = Padding(amount, 0, amount, 0)

/** Overflow describes what to do in the event that a component's layout position is beyond the
  * bounds of the `ComponentGroup`.
  */
enum Overflow:
  case Hidden, Wrap
