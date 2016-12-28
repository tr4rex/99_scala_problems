def and(a: Boolean, b: Boolean): Boolean = if (a) b else false

// all the functions can be rewritten with using pattern matching
def and_pm(a: Boolean, b: => Boolean): Boolean = a match {
  case true => b
  case _ => false
}

def or(a: Boolean, b: Boolean): Boolean = if (a) true else b

def not(a: Boolean): Boolean = if (a) false else true

def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))

def xor(a: Boolean, b: Boolean): Boolean = if (a) not(b) else b

def equ(a: Boolean, b: Boolean): Boolean = not(xor(a, b))

def table2(f: (Boolean,  Boolean) => Boolean) = {
  println("A     B     result")
  val bs = Seq(true, false)
  for {
    a <- bs
    b <- bs
  } println(f"$a%-5s $b%-5s ${f(a, b)}%-5s")
}