import PixelMatrix.fromArt

class Shape(val pixels: Seq[Seq[Boolean]]) extends PixelMatrix:
  override def matrix: Seq[Seq[Boolean]] = pixels
  def rotate: Shape =
    new Shape(Vector.tabulate(pixels.head.size, pixels.size)
      ((prevX, prevY) => pixels(prevY)(prevX)))
  override def equals(obj: Any): Boolean = obj match
    case s: Shape => pixels == s.pixels
    case _ => false

object Shape:
  def apply(art: String): Shape = new Shape(fromArt(art))

  object Straight extends Shape(fromArt("####"))
  object Square extends Shape(fromArt("##\n##"))
  object T extends Shape(fromArt("###\n # "))
  object L extends Shape(fromArt("# \n# \n##"))
  object S extends Shape(fromArt("## \n ##"))