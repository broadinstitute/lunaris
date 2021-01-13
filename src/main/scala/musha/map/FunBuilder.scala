package musha.map

object FunBuilder {

  implicit class T1[A, B1](f1: A => B1) {
    def &[B2](f2: A => B2): T2[A, B1, B2] = new T2(f1, f2)

    def apply[C](g: B1 => C): F1[A, B1, C] = new F1(f1)(g)
  }

  class F1[A, B1, C](f1: A => B1)(g: B1 => C) extends (A => C) {
    override def apply(a: A): C = g(f1(a))
  }

  class T2[A, B1, B2](f1: A => B1, f2: A => B2) {
    def &[B3](f3: A => B3): T3[A, B1, B2, B3] = new T3(f1, f2, f3)

    def apply[C](g: (B1, B2) => C): F2[A, B1, B2, C] = new F2(f1, f2)(g)
  }

  class F2[A, B1, B2, C](f1: A => B1, f2: A => B2)(g: (B1, B2) => C) extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a))
  }

  class T3[A, B1, B2, B3](f1: A => B1, f2: A => B2, f3: A => B3) {
    def &[B4](f4: A => B4): T4[A, B1, B2, B3, B4] = new T4(f1, f2, f3, f4)

    def apply[C](g: (B1, B2, B3) => C): F3[A, B1, B2, B3, C] = new F3(f1, f2, f3)(g)
  }

  class F3[A, B1, B2, B3, C](f1: A => B1, f2: A => B2, f3: A => B3)(g: (B1, B2, B3) => C) extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a), f3(a))
  }

  class T4[A, B1, B2, B3, B4](f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4) {
    def &[B5](f5: A => B5): T5[A, B1, B2, B3, B4, B5] = new T5(f1, f2, f3, f4, f5)

    def apply[C](g: (B1, B2, B3, B4) => C): F4[A, B1, B2, B3, B4, C] = new F4(f1, f2, f3, f4)(g)
  }

  class F4[A, B1, B2, B3, B4, C](f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4)(g: (B1, B2, B3, B4) => C)
    extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a), f3(a), f4(a))
  }

  class T5[A, B1, B2, B3, B4, B5](f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5) {
    def &[B6](f6: A => B6): T6[A, B1, B2, B3, B4, B5, B6] = new T6(f1, f2, f3, f4, f5, f6)

    def apply[C](g: (B1, B2, B3, B4, B5) => C): F5[A, B1, B2, B3, B4, B5, C] = new F5(f1, f2, f3, f4, f5)(g)
  }

  class F5[A, B1, B2, B3, B4, B5, C]
  (f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5)(g: (B1, B2, B3, B4, B5) => C)
    extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a), f3(a), f4(a), f5(a))
  }

  class T6[A, B1, B2, B3, B4, B5, B6](f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6) {
    def &[B7](f7: A => B7): T7[A, B1, B2, B3, B4, B5, B6, B7] = new T7(f1, f2, f3, f4, f5, f6, f7)

    def apply[C](g: (B1, B2, B3, B4, B5, B6) => C): F6[A, B1, B2, B3, B4, B5, B6, C] =
      new F6(f1, f2, f3, f4, f5, f6)(g)
  }

  class F6[A, B1, B2, B3, B4, B5, B6, C]
  (f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6)(g: (B1, B2, B3, B4, B5, B6) => C)
    extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a), f3(a), f4(a), f5(a), f6(a))
  }

  class T7[A, B1, B2, B3, B4, B5, B6, B7]
  (f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7) {
    def &[B8](f8: A => B8): T8[A, B1, B2, B3, B4, B5, B6, B7, B8] = new T8(f1, f2, f3, f4, f5, f6, f7, f8)

    def apply[C](g: (B1, B2, B3, B4, B5, B6, B7) => C): F7[A, B1, B2, B3, B4, B5, B6, B7, C] =
      new F7(f1, f2, f3, f4, f5, f6, f7)(g)
  }

  class F7[A, B1, B2, B3, B4, B5, B6, B7, C]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7
  )(g: (B1, B2, B3, B4, B5, B6, B7) => C)
    extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a), f3(a), f4(a), f5(a), f6(a), f7(a))
  }

  class T8[A, B1, B2, B3, B4, B5, B6, B7, B8]
  (f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8) {
    def &[B9](f9: A => B9): T9[A, B1, B2, B3, B4, B5, B6, B7, B8, B9] = new T9(f1, f2, f3, f4, f5, f6, f7, f8, f9)

    def apply[C](g: (B1, B2, B3, B4, B5, B6, B7, B8) => C): F8[A, B1, B2, B3, B4, B5, B6, B7, B8, C] =
      new F8(f1, f2, f3, f4, f5, f6, f7, f8)(g)
  }

  class F8[A, B1, B2, B3, B4, B5, B6, B7, B8, C]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8
  )(g: (B1, B2, B3, B4, B5, B6, B7, B8) => C)
    extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a), f3(a), f4(a), f5(a), f6(a), f7(a), f8(a))
  }

  class T9[A, B1, B2, B3, B4, B5, B6, B7, B8, B9]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9
  ) {
    def &[B10](f10: A => B10): T10[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] =
      new T10(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)

    def apply[C](g: (B1, B2, B3, B4, B5, B6, B7, B8, B9) => C): F9[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, C] =
      new F9(f1, f2, f3, f4, f5, f6, f7, f8, f9)(g)
  }

  class F9[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, C]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9
  )(g: (B1, B2, B3, B4, B5, B6, B7, B8, B9) => C)
    extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a), f3(a), f4(a), f5(a), f6(a), f7(a), f8(a), f9(a))
  }

  class T10[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10
  ) {
    def &[B11](f11: A => B11): T11[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] =
      new T11(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)

    def apply[C](g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10) => C):
    F10[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, C] =
      new F10(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)(g)
  }

  class F10[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, C]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10
  )(g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10) => C)
    extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a), f3(a), f4(a), f5(a), f6(a), f7(a), f8(a), f9(a), f10(a))
  }

  class T11[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10, f11: A => B11
  ) {
    def &[B12](f12: A => B12): T12[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] =
      new T12(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)

    def apply[C](g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11) => C):
    F11[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, C] =
      new F11(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)(g)
  }

  class F11[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, C]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10, f11: A => B11
  )(g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11) => C)
    extends (A => C) {
    override def apply(a: A): C = g(f1(a), f2(a), f3(a), f4(a), f5(a), f6(a), f7(a), f8(a), f9(a), f10(a), f11(a))
  }

  class T12[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10, f11: A => B11, f12: A => B12
  ) {
    def &[B13](f13: A => B13): T13[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] =
      new T13(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)

    def apply[C](g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12) => C):
    F12[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, C] =
      new F12(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)(g)
  }

  class F12[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, C]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10, f11: A => B11, f12: A => B12
  )(g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12) => C)
    extends (A => C) {
    override def apply(a: A): C =
      g(f1(a), f2(a), f3(a), f4(a), f5(a), f6(a), f7(a), f8(a), f9(a), f10(a), f11(a), f12(a))
  }

  class T13[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10, f11: A => B11, f12: A => B12, f13: A => B13
  ) {
    def &[B14](f14: A => B14): T14[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] =
      new T14(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)

    def apply[C](g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13) => C):
    F13[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, C] =
      new F13(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)(g)
  }

  class F13[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, C]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10, f11: A => B11, f12: A => B12, f13: A => B13
  )(g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13) => C)
    extends (A => C) {
    override def apply(a: A): C =
      g(f1(a), f2(a), f3(a), f4(a), f5(a), f6(a), f7(a), f8(a), f9(a), f10(a), f11(a), f12(a), f13(a))
  }

  class T14[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10, f11: A => B11, f12: A => B12, f13: A => B13, f14: A => B14
  ) {
//    def &[B15](f15: A => B15): T15[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] =
//      new T15(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)

    def apply[C](g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14) => C):
    F14[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, C] =
      new F14(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)(g)
  }

  class F14[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, C]
  (
    f1: A => B1, f2: A => B2, f3: A => B3, f4: A => B4, f5: A => B5, f6: A => B6, f7: A => B7, f8: A => B8,
    f9: A => B9, f10: A => B10, f11: A => B11, f12: A => B12, f13: A => B13, f14: A => B14
  )(g: (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14) => C)
    extends (A => C) {
    override def apply(a: A): C =
      g(f1(a), f2(a), f3(a), f4(a), f5(a), f6(a), f7(a), f8(a), f9(a), f10(a), f11(a), f12(a), f13(a), f14(a))
  }
}
