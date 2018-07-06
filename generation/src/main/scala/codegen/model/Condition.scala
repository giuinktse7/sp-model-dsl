package codegen.model

import Types._


case class Condition(guard: Proposition,
                     action: List[Action] = List(),
                     attributes: AttributeMap = AttributeMap())