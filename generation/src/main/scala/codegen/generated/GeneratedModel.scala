package codegen.generated
import codegen.internal.GeneratedIdentifiable
import codegen.model.Bool._
import codegen.model.ConditionNode._
import codegen.model.Condition
import java.util.UUID
import monocle.macros.Lenses
import play.api.libs.json._
@Lenses case class Generated__attributes_attributes(
    someAttr: String = "kalle",
    numberMaybe: Int = 255
)

@Lenses case class Generated_mainThing(
    name: String = "mainThing",
    attributes: Generated__attributes_attributes =
      Generated__attributes_attributes(),
    id: UUID = UUID.fromString("da2030c0-5171-458c-9b7f-d229b7051fb9"),
    someOperation: Generated_mainThing_someOperation =
      Generated_mainThing_someOperation()
) extends GeneratedIdentifiable

@Lenses case class Generated_mainThing_someOperation(
    name: String = "someOperation",
    conditions: List[Condition] = List[Condition](
      Condition(
        And(
          Not(Equal(Value(JsNumber(5)), Value(JsNumber(2)))),
          Equal(Value(JsString("foo")), Value(JsString("bar")))
        ),
        Definition(
          IdNode(UUID.fromString("f13f1426-21a7-4750-81ff-4dd067354e44")),
          JsNumber(25)
        )
      )
    ),
    attributes: JsObject = JsObject.empty,
    id: UUID = UUID.fromString("6fbab77b-0266-4569-8ad5-54bd6a762bfb"),
    subThing: Generated_mainThing_someOperation_subThing =
      Generated_mainThing_someOperation_subThing()
) extends GeneratedIdentifiable

@Lenses case class Generated_mainThing_someOperation__attributes_attributes(
    domain: Seq[java.lang.String] = Seq("foo", "bar")
)

@Lenses case class Generated_mainThing_someOperation_subThing(
    name: String = "subThing",
    attributes: Generated_mainThing_someOperation__attributes_attributes =
      Generated_mainThing_someOperation__attributes_attributes(),
    id: UUID = UUID.fromString("f13f1426-21a7-4750-81ff-4dd067354e44")
) extends GeneratedIdentifiable

@Lenses case class GeneratedModel(
    items: List[Any] = List(Generated_mainThing()),
    mainThing: Generated_mainThing = Generated_mainThing()
)
