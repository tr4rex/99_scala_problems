import scala.language.postfixOps

def isPrime(x: Int) = {
  import scala.math.sqrt
  x > 1 && (2 to sqrt(x).floor.toInt).forall(x % _ != 0)
}

def primes: Stream[Int] = Stream.from(2).filter(x => isPrime(x))

def gcd(x: Int, y: Int): Int = if (y == 0) { x } else { gcd(y, x % y) }

def isCoprime(x: Int, y: Int) = gcd(x, y) == 1

def totient(x: Int) = (1 to x).filter(isCoprime(_, x)).size

def primeFactors(n: Int) = {
  def loop(n: Int, primes: Stream[Int], acc: List[Int]): List[Int] =
    if (n == 1) acc
    else if (n % primes.head == 0) loop(n / primes.head, primes, primes.head :: acc)
    else loop(n, primes.tail, acc)

  loop(n, primes, Nil).reverse
}

def primeFactorMultiplicity(n: Int): Map[Int, Int] = 
  primeFactors(n).groupBy(x => x).map { case (k, v) => (k, v.size) }

def pow(x: Int, y: Int): Int =
  y match {
    case 0 => 1
    case 1 => x
    case p if p % 2 == 0 => pow(x * x, p / 2)
    case p if p % 2 == 1 => x * pow(x, p - 1)
  }

def totientImproved(x: Int): Int =
  primeFactorMultiplicity(x).
    map { case(k, v) => ((k - 1) * pow(k, v - 1)).toInt }.
    product

def functionComparison[I, T](f1: I => T, f2: I => T)(v: I): (Long, Long) = {
  def measure[I, T](f: I => T)(v: I): Long = {
    val start = System.currentTimeMillis()
    f(v)
    val end = System.currentTimeMillis()
    end - start
  }

  (measure(f1)(v), measure(f2)(v))
}

def primesRange(x: Int, y: Int): List[Int] = primes takeWhile(_ <= y) dropWhile(_ < x) toList

def goldbach(x: Int): (Int, Int) = primes.find(p => isPrime(x - p)).map(p => (p, x - p)).head
