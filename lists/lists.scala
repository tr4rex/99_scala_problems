	def duplicateElements[T](list: List[T]): List[T] =
  list.flatMap(i => List(i, i))

def duplicate2[T](list: List[T]): List[T] = {
  def loop(initial: List[T], acc: List[T]): List[T] = initial match {
    case x :: xs => loop(xs, x :: x :: acc)
    case _ => acc
  }

  loop(list, Nil).reverse
}

def duplicateN[T](count: Int, list: List[T]) =
  list.flatMap(e => List.fill(count)(e))

def drop1[T](position: Int, list: List[T]) =
  ((1 to list.size) zip list).withFilter( _._1 % position != 0).map(_._2)

def drop2[T](position: Int, list: List[T]) = {
  def loop(list: List[T], acc: List[T], index: Int): List[T] =
    list match {
      case x :: xs => loop(xs, if (index % position == 0) acc else x :: acc, index + 1)
      case _ => acc.reverse
    }

  loop(list, Nil, 1)
}

def splitList[T](n: Int, xs: List[T]) = (xs take n, xs drop n)

def slice[T](from: Int, to: Int, xs: List[T]) = (xs drop from) take (to - from)

def rotate[T](n: Int, xs: List[T]) = slice(n, xs.length, xs) ++ xs.take(n)

def removeAt1[T](n: Int, xs: List[T]) = xs.zipWithIndex.withFilter(n != _._2).map(_._1)

def insertAt[T](x: T, n: Int, xs: List[T]) = {
    val (left, right) = xs.splitAt(n)
    left ++ (x :: right)
}

def range(from: Int, to: Int) = {
  def step(n: Int, acc: List[Int]): List[Int] =
    if (n > to) acc.reverse
    else step(n + 1, n :: acc)

  step(from, Nil)
}