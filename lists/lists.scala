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