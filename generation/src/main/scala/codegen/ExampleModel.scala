package codegen

import codegen.internal.Effect

object ExampleModel {
  import codegen.internal.Effect.Implicits._
  import codegen.internal.Result
  import codegen.model.Bool._
  import codegen.model.Conditional
  import codegen.model.Conditional._
  import java.util.UUID
  import monocle.macros.Lenses
  import play.api.libs.json._
  @Lenses case class Generated_r1(
                                   name: String = "r1",
                                   attributes: Generated_r1_attributes = Generated_r1_attributes(),
                                   id: UUID = UUID.fromString("c0193d3c-7b2f-4c53-a89b-9916b5c1308e"),
                                   o1: Generated_r1_o1 = Generated_r1_o1(),
                                   o2: Generated_r1_o2 = Generated_r1_o2()
                                 )

  @Lenses case class Generated_r1_attributes(
                                              domain: Seq[java.lang.String] = Seq("foo", "bar")
                                            )

  @Lenses case class Generated_r1_o1(
                                      name: String = "o1",
                                      conditions: List[Conditional[Unit]] = List[Conditional[Unit]](
                                        Conditional[Unit](
                                          And(
                                            Equal(JsNumber(5), JsNumber(2)),
                                            Equal(JsString("foo"), JsString("bar"))
                                          )
                                        )
                                      ),
                                      attributes: JsObject = JsObject.empty,
                                      id: UUID = UUID.fromString("cbf5f3f6-570a-4c0c-8047-75a49d0da615"),
                                      t1: Generated_r1_o1_t1 = Generated_r1_o1_t1(),
                                      t2: Generated_r1_o1_t2 = Generated_r1_o1_t2()
                                    )

  @Lenses case class Generated_r1_o1_t1(
                                         name: String = "t1",
                                         attributes: Generated_r1_o1_t1_attributes =
                                         Generated_r1_o1_t1_attributes(),
                                         id: UUID = UUID.fromString("79e256c4-d547-4c92-b165-60560e672b75")
                                       )

  @Lenses case class Generated_r1_o1_t1_attributes(
                                                    domain: Seq[scala.Boolean] = Seq(true, false)
                                                  )

  @Lenses case class Generated_r1_o1_t2(
                                         name: String = "t2",
                                         attributes: Generated_r1_o1_t2_attributes =
                                         Generated_r1_o1_t2_attributes(),
                                         id: UUID = UUID.fromString("19baf703-a109-4014-af5a-d08b48f6c2b1")
                                       )

  @Lenses case class Generated_r1_o1_t2_attributes(someAttr: String = "kalle")

  @Lenses case class Generated_r1_o2(
                                      name: String = "o2",
                                      conditions: List[Conditional[Unit]] = List[Conditional[Unit]](
                                        Conditional[Unit](Equal(JsString("k"), JsString("e")))
                                      ),
                                      attributes: JsObject = JsObject.empty,
                                      id: UUID = UUID.fromString("a7407eb8-3e93-4670-b399-1e016a2b7ac6"),
                                      t2: Generated_r1_o2_t2 = Generated_r1_o2_t2()
                                    )

  @Lenses case class Generated_r1_o2_t2(
                                         name: String = "t2",
                                         attributes: Generated_r1_o2_t2_attributes =
                                         Generated_r1_o2_t2_attributes(),
                                         id: UUID = UUID.fromString("19baf703-a109-4014-af5a-d08b48f6c2b1")
                                       )

  @Lenses case class Generated_r1_o2_t2_attributes(someAttr: String = "kalle")
  @Lenses case class TestModel(
                                items: List[Any] = List(Generated_r1()),
                                r1: Generated_r1 = Generated_r1()
                              )

  val id = java.util.UUID.randomUUID()
  java.util.UUID.fromString(id.toString)

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