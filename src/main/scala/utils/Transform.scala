package utils

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

trait :->[-A, +B] {
  def cast(a: A): B
}

object :-> {
  implicit def self[A] = new :->[A, A] {
    def cast(a: A): A = a
  }

  implicit def superclass[A, B >: A] = new :->[A, B] {
    def cast(a: A): B = a
  }

  implicit def transitive[A, B, C](implicit L2: A :-> B, L3: B :-> C) =  new :->[A, C] {
    def cast(a: A): C = L3.cast(L2.cast(a))
  }
}


object Transform {
  def castable[A, B]: A :-> B = macro castableImpl[A, B]

  def castableImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context): c.Expr[A :-> B] = {
    import c.universe._

    val fromClassSym = c.weakTypeOf[A].typeSymbol
    val toClassSym = c.weakTypeOf[B].typeSymbol

    c.Expr[A :-> B](q"""new :->[$fromClassSym, $toClassSym] {
      def cast(a: $fromClassSym): $toClassSym = ${toClassSym.companion}.apply(a)
      }""")
  }
}
