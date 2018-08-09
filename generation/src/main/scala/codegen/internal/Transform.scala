package codegen.internal

import codegen.internal.Attribute.AttrObject
import codegen.model.{GenThing, Identifiable, Thing}

/**
  * Transforms (adding or removing functionality) one type into another
  */
trait Transform[A, +B] {
  def into: A => B
}

object Transform {
  implicit val thingToGenThing: Transform[Thing, GenThing] = new Transform[Thing, GenThing] {
    override def into = t =>
      GenThing(t.name, Attribute.fromSPValue(t.spAttributes).asInstanceOf[AttrObject])
  }

  implicit val genThingToThing: Transform[GenThing, Thing] = new Transform[GenThing, Thing] {
    override def into = t => Thing(t.name, t.spAttributes, t.id)
  }

  implicit def identifiableToAttribute[A <: Identifiable]: Transform[A, Attribute] = new Transform[A, Attribute] {
    override def into = identifiable => Attribute.fromSPValue(identifiable.spAttributes)
  }

  implicit class TransformerDSL[A](val a: A) extends AnyVal {
    def into[B](implicit T: Transform[A, B]): B = T.into(a)
  }
}