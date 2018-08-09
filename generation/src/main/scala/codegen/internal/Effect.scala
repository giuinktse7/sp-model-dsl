package codegen.internal

/**
  *
  * @tparam R Result type when applying the effect
  * @tparam I Type that the effect is defined for
  */
trait Effect[R, I[_]] {
  def effect(i: I[R]): R
}

object Effect {
  type Partial[R, I[_, _], A] = Effect[R, ({type f[x] = I[A, x]})#f]
  type Partial2[R, I[_, _, _], A, B] = Effect[R, ({type f[x] = I[A, B, x]})#f]

  object Implicits {
    implicit def effect[I[_]]: Effect[Unit, I] = _ => println("effect")
    implicit def effect2[I[_, _], A]: Effect.Partial[Unit, I, A] = _ => println("effect2")
    implicit def effect3[I[_, _, _], A, B]: Effect.Partial2[Unit, I, A, B] = _ => println("effect3")

    object ForGen {
      implicit def effect[I[_]](implicit G: Generate[I[Result]]): Effect[Result, I] = i => G.generated(i)
      implicit def effect2[I[_, _], A](implicit G: Generate[I[A, Result]]): Effect.Partial[Result, I, A] = i => G.generated(i)
      implicit def effect3[I[_, _, _], A, B](implicit G: Generate[I[A, B, Result]]): Effect.Partial2[Result, I, A, B] = i => G.generated(i)
    }
  }
}