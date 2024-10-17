import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class Board(val width: Int, val height: Int) extends PixelMatrix:
  private val board = ArrayBuffer.fill(height, width)(false)

  private val entities = ListBuffer[Entity]()

  def isPositionLegal(pos: (Int, Int)): Boolean =
    board.lift(pos._1).exists(_.lift(pos._2).exists(!_)) ||
      pos._1 < 0

  def isEntityLegal(entity: Entity): Boolean =
    entity.blocks.forall(isPositionLegal)

  def insertEntity(entity: Entity): Unit =
    entities += entity

  def freeze(entity: Entity): Unit =
    // freeze shape into place on board
    entity.blocks.foreach((y, x) => board(y)(x) = true)

  def tick(): Unit =
    moveEntities()
    clearFilledRows()

  private def clearFilledRows(): Unit =
    board
      .reverse
      .filter(_.forall(_ == true))
      .foreach(_.mapInPlace(_ => false))

  private def collapseEmptyRows(): Unit =
    board
      .filterInPlace(_.forall(!_))
      .prependAll(
        ArrayBuffer.fill(height - board.size)(
          ArrayBuffer.fill(width)(false)))

  private def moveEntities(): Unit =
    // move shapes down. freeze those who cant move.
    val movedEntities = entities
      .map(e => (e, e.copy(topLeft = (e.topLeft._1 - 1, e.topLeft._2))))

    movedEntities
      .filter((_, to) => !isEntityLegal(to))
      .foreach((from, _) => freeze(from))

    entities.clear()
    entities ++= movedEntities
      .map((_, to) => to)
      .filter(isEntityLegal)

  override def matrix: Seq[Seq[Boolean]] = board.map(_.toSeq).toSeq