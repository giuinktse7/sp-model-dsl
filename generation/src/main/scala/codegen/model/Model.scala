package codegen.model

case class Model(
                  className: String,
                  items: IdentifiableGraph*
                )
