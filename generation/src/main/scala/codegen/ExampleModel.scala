package codegen

import codegen.internal.Effect

object ExampleModel {
  import codegen.internal.Effect.Implicits._
  import codegen.internal.GeneratedIdentifiable
  import codegen.model.Bool._
  import codegen.model.EffectConditional
  import codegen.model.EffectConditional._
  import java.util.UUID
  import monocle.macros.Lenses
  import play.api.libs.json._
  @Lenses case class Generated_r1(
                                   name: String = "r1",
                                   attributes: Generated_r1_attributes = Generated_r1_attributes(),
                                   id: UUID = UUID.fromString("d36f18c2-920f-4b73-b2d9-6dcbe6f29524"),
                                   o1: Generated_r1_o1 = Generated_r1_o1(),
                                   o2: Generated_r1_o2 = Generated_r1_o2()
                                 ) extends GeneratedIdentifiable

  @Lenses case class Generated_r1_attributes(
                                              domain: Seq[java.lang.String] = Seq("foo", "bar")
                                            )

  @Lenses case class Generated_r1_o1(
                                      name: String = "o1",
                                      conditions: List[EffectConditional[Unit]] = List[EffectConditional[Unit]](
                                        EffectConditional[Unit](
                                          And(
                                            Equal(JsNumber(5), JsNumber(2)),
                                            Equal(JsString("foo"), JsString("bar"))
                                          )
                                        )
                                      ),
                                      attributes: JsObject = JsObject.empty,
                                      id: UUID = UUID.fromString("b2d5e39a-188d-4e47-aec0-a6a8173fc3d8"),
                                      t1: Generated_r1_o1_t1 = Generated_r1_o1_t1(),
                                      t2: Generated_r1_o1_t2 = Generated_r1_o1_t2()
                                    ) extends GeneratedIdentifiable

  @Lenses case class Generated_r1_o1_t1(
                                         name: String = "t1",
                                         attributes: Generated_r1_o1_t1_attributes =
                                         Generated_r1_o1_t1_attributes(),
                                         id: UUID = UUID.fromString("6a305370-721f-4dcc-ad20-ec1eb7005f24")
                                       ) extends GeneratedIdentifiable

  @Lenses case class Generated_r1_o1_t1_attributes(domain: Int = 15)

  @Lenses case class Generated_r1_o1_t2(
                                         name: String = "t2",
                                         attributes: Generated_r1_o1_t2_attributes =
                                         Generated_r1_o1_t2_attributes(),
                                         id: UUID = UUID.fromString("bf121849-d5aa-4b2c-9ef0-2d11ddb6d727")
                                       ) extends GeneratedIdentifiable

  @Lenses case class Generated_r1_o1_t2_attributes(someAttr: String = "kalle")

  @Lenses case class Generated_r1_o2(
                                      name: String = "o2",
                                      conditions: List[EffectConditional[Unit]] = List[EffectConditional[Unit]](
                                        EffectConditional[Unit](Equal(JsString("k"), JsString("e")))
                                      ),
                                      attributes: JsObject = JsObject.empty,
                                      id: UUID = UUID.fromString("a555a25d-0597-436d-a5b4-3f8e86a0ca0b"),
                                      t2: Generated_r1_o2_t2 = Generated_r1_o2_t2()
                                    ) extends GeneratedIdentifiable

  @Lenses case class Generated_r1_o2_t2(
                                         name: String = "t2",
                                         attributes: Generated_r1_o2_t2_attributes =
                                         Generated_r1_o2_t2_attributes(),
                                         id: UUID = UUID.fromString("bf121849-d5aa-4b2c-9ef0-2d11ddb6d727")
                                       ) extends GeneratedIdentifiable

  @Lenses case class Generated_r1_o2_t2_attributes(someAttr: String = "kalle")
  @Lenses case class TestModel(
                                items: List[Any] = List(Generated_r1()),
                                r1: Generated_r1 = Generated_r1()
                              )

/*
  def create(): List[Identifiable] = {
    val t1 = Thing("v1", AttributeMap("domain"-> List("foo", "bar")))
    val t2 = Thing("v2", AttributeMap("domain"-> List(true, false)))

    val conditionForO1 = Condition(AND(List(EQ(SVIDEval(t1.id), ValueHolder("foo")), NEQ(SVIDEval(t2.id), ValueHolder(true)))))
    val o1 = Operation("o1", List(conditionForO1))

    val conditionForO2 = Condition(EQ(SVIDEval(o1.id), ValueHolder("e")))
    val o2 = Operation("o2", List(conditionForO2))

    val r1 = Thing("r1", AttributeMap("someAttr" -> "kalle"))

    val sop = OperationOrder.Specification("Sequential", OperationOrder.Sequential(o1, o2))

    val graph = r1 has (
      o1 has (t1, t2),
      o2 has t2
    )


    val model: List[Identifiable] = List(t1, t2, o1, o2, r1, sop)

    model
  }
*/
}