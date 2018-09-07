package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    new Signal(math.pow(b(), 2) - 4 * a() * c())

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    new Signal({
      val discriminant = delta()
      if (discriminant > 0) Set((-b() + math.sqrt(delta())) / (2 * a()), (-b() - math.sqrt(delta())) / (2 * a()))
      else if (discriminant == 0) Set(-b() / (2 * a()))
      else Set.empty
    })
  }
}
