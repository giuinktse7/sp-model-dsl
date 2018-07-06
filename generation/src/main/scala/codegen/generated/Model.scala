package codegen.generated

import monocle.macros.Lenses
@Lenses case class TopClass(
    data: Model = Model()
)
@Lenses case class Model(
    b: Thing = Thing(),
    a: String = "A string!"
)
@Lenses case class Thing(
    c: BigDecimal = 10
)
