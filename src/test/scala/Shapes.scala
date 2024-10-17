import org.scalatest.flatspec.AnyFlatSpec

class Shapes extends AnyFlatSpec:
  "A pixel matrix" should "be created correctly from string art" in {
    assert(Shape(" ## #\n#### ").pixels == Vector(
      Vector(false, true, true, false, true),
      Vector(true, true, true, true, false)))
  }

  it should "throw IllegalArgumentException if the shape has different lengths" in {
    assertThrows[IllegalArgumentException] {
      Shape(
        "###  #\n" +
        "# ## ##"
      )
    }
  }

  "A straight horizontal shape" should "rotate into a straight vertical shape" in {
    assert(
      Shape("###").rotate ==
        Shape("#\n#\n#")
    )
  }

  it should "rotate twice into the same shape" in {
    assert(
      Shape("###").rotate.rotate ==
        Shape("###")
    )
  }

  it should "repeat same two shapes infinitely" in {
    assert(
      (1 to 20).scan(Shape("###"))((s1, _) => s1.rotate)
    )
  }