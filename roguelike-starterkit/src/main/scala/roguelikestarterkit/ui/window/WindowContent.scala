// package roguelikestarterkit.ui.window

// import indigo.shared.Outcome
// import indigo.shared.events.GlobalEvent
// import indigo.shared.scenegraph.Layer
// import roguelikestarterkit.Dimensions
// import roguelikestarterkit.ui.component.Component
// import roguelikestarterkit.ui.datatypes.Bounds
// import roguelikestarterkit.ui.datatypes.UIContext

// /** A typeclass that confirms that some type `A` can be used as a `WindowContent` provides the
//   * necessary operations for that type to act as a window content.
//   */
// trait WindowContent[A, ReferenceData]:

//   /** Update this content's model.
//     */
//   def updateModel(
//       context: UIContext[ReferenceData],
//       model: A
//   ): GlobalEvent => Outcome[A]

//   /** Produce a renderable output for this content base on the model
//     */
//   def present(
//       context: UIContext[ReferenceData],
//       model: A
//   ): Outcome[Layer]

//   /** Called when a window has been told to refresh its content, possibly by the content itself.
//     */
//   def refresh(reference: ReferenceData, model: A, contentDimensions: Dimensions): A

// /** Companion object for `WindowContent` */
// object WindowContent:

//   /** A `WindowContent` instance for any A with a `Component` instance.
//     */
//   given [A, ReferenceData](using
//       comp: Component[A, ReferenceData]
//   ): WindowContent[A, ReferenceData] with

//     def updateModel(
//         context: UIContext[ReferenceData],
//         model: A
//     ): GlobalEvent => Outcome[A] =
//       e => comp.updateModel(context, model)(e)

//     def present(
//         context: UIContext[ReferenceData],
//         model: A
//     ): Outcome[Layer] =
//       comp.present(context, model).map(_.toLayer)

//     def refresh(reference: ReferenceData, model: A, contentDimensions: Dimensions): A =
//       comp.refresh(reference, model, contentDimensions)
