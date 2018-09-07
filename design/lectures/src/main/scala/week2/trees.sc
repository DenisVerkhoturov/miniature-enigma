sealed trait IntSet {
  def contains(value: Int): Boolean
  def include(value: Int): IntSet
  def union(another: IntSet): IntSet
}

case object Empty extends IntSet {
  override def contains(value: Int) = false

  override def include(value: Int) = NonEmpty(value, Empty, Empty)

  override def union(another: IntSet) = another
}

case class NonEmpty(data: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(value: Int) =
    if (value < data) left contains value
    else if (value > data) right contains value
    else true

  override def include(value: Int) =
    if (value < data) NonEmpty(data, left include value, right)
    else if (value > data) NonEmpty(data, left, right include value)
    else this

  override def union(another: IntSet) = (left union another.left) union (right union another.right) include data
}
