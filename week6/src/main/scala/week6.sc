import scala.util.{Success, Try}

def sqrtFloor(number : BigInt) = {
  def next(n : BigInt, i : BigInt) : BigInt = (n + i/n) >> 1
  val one = BigInt(1)
  var n = one
  var n1 = next(n, number)
  while ((n1 - n).abs > one) {
    n = n1
    n1 = next(n, number)
  }
  while (n1 * n1 > number) {
    n1 -= one
  }
  n1
}
def sqrt(number: BigInt) = {
  val g = sqrtFloor(number)
  if (g*g != number) throw new ArithmeticException("No integer square root")
  g
}
def sqrtCeil(number: BigInt) = {
  val g = sqrtFloor(number)
  if (g*g == number) g else g + 1
}

// challenge 1
val n1 = BigInt("179769313486231590772930519078902473361797697894230657273430081157732675805505620686985379449212982959585501387537164015710139858647833778606925583497541085196591615128057575940752635007475935288710823649949940771895617054361149474865046711015101563940680527540071584560878577663743040086340742855278549092581")
val a1 = sqrtCeil(n1)
val x1 = sqrt(a1*a1 - n1)
val p1 = a1 - x1
val q1 = a1 + x1
if (p1 * q1 == n1) println(p1) else println("not quite")

// challenge 2
val n2 = BigInt("648455842808071669662824265346772278726343720706976263060439070378797308618081116462714015276061417569195587321840254520655424906719892428844841839353281972988531310511738648965962582821502504990264452100885281673303711142296421027840289307657458645233683357077834689715838646088239640236866252211790085787877")
val minA2 = sqrtCeil(n2)
val maxA2 = n2 + Math.pow(2, 20).toLong
def tryGetX(a2: BigInt): Try[(BigInt, BigInt)] = Try((a2,sqrt(a2*a2 - n2)))
val (a2,x2) = Stream.iterate(minA2)(_ + 1) takeWhile (_ < maxA2) map tryGetX collect {case Success(o) => o} head
val p2 = a2 - x2
val q2 = a2 + x2
if (p2 * q2 == n2) println(p2) else println("not quite")

// challenge 3
val n3 = BigInt("720062263747350425279564435525583738338084451473999841826653057981916355690188337790423408664187663938485175264994017897083524079135686877441155132015188279331812309091996246361896836573643119174094961348524639707885238799396839230364676670221627018353299443241192173812729276147530748597302192751375739387929")
val m3 = 24 * n3
val b3 = sqrtCeil(m3)
val y3 = sqrt(b3 * b3 - m3)
val r3 = b3 - y3
val s3 = b3 + y3
val p3 = r3 / 6
val q3 = s3 / 4
if (p3 * q3 == n3) println(p3) else println("not quite")

// challenge 4
val ct = BigInt("22096451867410381776306561134883418017410069787892831071731839143676135600120538004282329650473509424343946219751512256465839967942889460764542040581564748988013734864120452325229320176487916666402997509188729971690526083222067771600019329260870009579993724077458967773697817571267229951148662959627934791540")
val n = n1
val e = BigInt(65537)
// already factorised this
val phi = (p1 - 1) * (q1 - 1)
val d = e.modInverse(phi)
val paddedMsg = ct.modPow(d, n).toByteArray
assert(paddedMsg.head == 2)
val msgBytes = paddedMsg.dropWhile(0 != _).tail
println(new String(msgBytes))