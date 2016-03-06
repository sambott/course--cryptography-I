import Helpers._

def setKey(p: Int, l: Int, c: Char) = {
  key(p) = ( c.toInt ^ ciphertexts(l)(p)).asInstanceOf[Byte]
}
(0 to 10) foreach { i =>
  val psc = potentialSpaceCharPositions(allXorsFor(i))
  psc foreach { p =>
    key(p) = (32 ^ ciphertexts(i)(p)).asInstanceOf[Byte]
  }
}

// Above got enough of the key to read as human, now manually fill in the blanks
setKey(74, 1, 'c')
setKey(92, 2, 's')
setKey(43, 10, 'c')
setKey(69, 4, ' ')
setKey(48, 3, 'o')
setKey(51, 3, 't')
setKey(59, 6, 'e')
setKey(61, 6, 'n')
setKey(63, 6, 'e')
setKey(42, 0, 'o')
setKey(41, 0, 'c')
setKey(40, 1, 't')
setKey(39, 2, 'c')
setKey(37, 10, 't')
setKey(36, 10, 's')
setKey(35, 8, 's')
setKey(54, 6, 'e')
setKey(55, 6, ' ')
setKey(67, 0, 't')
setKey(82, 6, 'r')
setKey(84, 6, 'e')
setKey(86, 0, 'i')
setKey(92, 1, 's')
setKey(93, 5, 'e')
setKey(99, 0, 'i')
setKey(101, 0, 'e')
setKey(103, 8, 'd')
setKey(95, 7, 'r')
setKey(109, 1, 'r')
setKey(110, 1, 'e')
setKey(111, 1, 'm')
setKey(23, 5, 'c')
setKey(24, 5, 'r')
setKey(25, 5, 'y')
setKey(26, 5, 'p')
setKey(27, 5, 't')
setKey(28, 5, 'o')
setKey(29, 5, 'g')
setKey(30, 5, 'r')
setKey(31, 5, 'a')
setKey(32, 5, 'p')
setKey(33, 5, 'h')
setKey(21, 9, 'c')
setKey(18, 0, 'n')
setKey(17, 8, 'e')
setKey(1, 10, 'h')
setKey(2, 10, 'e')
setKey(3, 10, ' ')
setKey(15, 3, 'p')
setKey(16, 3, 'r')
setKey(14, 3, ' ')
setKey(4, 1, 'r')
setKey(12, 5, 'o')
setKey(13, 5, ' ')
setKey(5, 10, 'e')
setKey(6, 10, 'c')
setKey(7, 10, 'r')
setKey(8, 10, 'e')
setKey(9, 10, 't')
setKey(10, 10, ' ')


def printNumbers() = {
  println()
  (0 until longest) foreach (i => print(if (i % 10 == 0) i / 10 else " "))
  println()
  (0 until longest) foreach (i => print(i % 10))
  println()
}
{
  printNumbers()
}
ciphertexts foreach {ct =>
  val decoded = xor_texts(ct, key.toList)
  val lst = decoded zip key map {case (d,k) => if (k == 0) 32.asInstanceOf[Byte] else d}



















  println(new String(lst.toArray))
}

