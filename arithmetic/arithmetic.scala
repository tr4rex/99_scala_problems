def isPrime(x: Int) = {
  import scala.math.sqrt
  x > 1 && (2 to sqrt(x).floor.toInt).forall(x % _ != 0)
}

def gcd(x: Int, y: Int): Int = if (y == 0) { x } else { gcd(y, x % y) }
