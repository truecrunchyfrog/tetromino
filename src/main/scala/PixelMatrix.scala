trait PixelMatrix:
  def matrix: Seq[Seq[Boolean]]

object PixelMatrix:
  def fromArt(art: String): Seq[Seq[Boolean]] =
    val lines = art.split('\n')
    // Ensure all lines are of equal length:
    require(lines.map(_.length).distinct.length == 1)
    lines.map(_.map(!_.isWhitespace).toVector).toVector