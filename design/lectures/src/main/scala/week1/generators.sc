trait Generator[+T] {
  self =>

  def generate: T

  def map[S](mapper: T => S): Generator[S] = new Generator[S] {
    override def generate: S = mapper(self.generate)
  }

  def flatMap[S](mapper: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = mapper(self.generate).generate
  }
}

val integers: Generator[Int] = new Generator[Int] {
  val random = new java.util.Random
  override def generate: Int = random.nextInt
}

val booleans: Generator[Boolean] = new Generator[Boolean] {
  def generate: Boolean = integers.generate > 0
}

val pairs: Generator[(Int, Int)] = new Generator[(Int, Int)] {
  override def generate: (Int, Int) = (integers.generate, integers.generate)
}

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

sealed trait Tree

case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(data: Int) extends Tree

val trees: Generator[Tree] = new Generator[Tree] {
  override def generate: Tree = ???
}
