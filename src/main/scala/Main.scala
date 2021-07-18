
final case class Seed(long: Long):
  // def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
  def next = Seed(long + 1)


class Random[A](val next: Seed => (Seed, A)):

  def run: A = next(Seed(0L))._2

  def map[B](f: A => B): Random[B] =
    val newNext: Seed => (Seed, B) = { seed =>
      val (newSeed, a) = next(seed)
      (newSeed, f(a))
    }

    new Random(newNext)

  def flatMap[B](f: A => Random[B]): Random[B] =
    val newNext: Seed => (Seed, B) = { seed =>
      val (newSeed, a) = next(seed)
      f(a).next(newSeed)
    }

    new Random(newNext)

  def mapSeed(s: Long): Random[A] =
    new Random[A](_ => next(Seed(s)))

object Random:

  def apply[T](using random: Random[T]): Random[T] = random

  extension (rnd: scala.util.Random)
    private def nextRandomString: String =
      val l = rnd.between(0, 1000)
      rnd.nextString(l)

  given Random[Int](seed => (seed.next, scala.util.Random(seed.long).nextInt))
  given Random[Boolean](seed => (seed.next, scala.util.Random(seed.long).nextBoolean))
  given Random[Char](seed => (seed.next, scala.util.Random(seed.long).nextPrintableChar))
  given Random[String](seed => (seed.next, scala.util.Random(seed.long).nextRandomString))

  def between(minInclusive: Long, maxExclusive: Long): Random[Long] =
    new Random[Long](seed => (seed.next, scala.util.Random(seed.long).between(minInclusive, maxExclusive)))

  def between(minInclusive: Int, maxExclusive: Int): Random[Int] =
    new Random[Int](seed => (seed.next, scala.util.Random(seed.long).between(minInclusive, maxExclusive)))

  def oneOf[T](in: List[T]): Random[T] =
    for {
      i <- between(0, in.length)
    } yield in(i)

  def pure[T](t: T): Random[T] =
    new Random(seed => (seed.next, t))

  def sequence[T](l: List[Random[T]]): Random[List[T]] =
    l match {
      case Nil =>
        pure[List[T]](List.empty[T])
      case head :: tail =>
        sequence(tail).flatMap(l => head.map(h => h :: l))
    }


object Gnomenclator:

  val gnomishConsonants = List(
   "b", "c", "d", "dh", "f", "gh", "h", "h'", "j", "k", "l", "gl", "m", "n", "gn", "p", "q", "kr", "r", "sh", "s", "t", "t'", "v", "w", "x", "y", "z", "ð"
  )

  def randomConsonant(c: Char): Random[String] =
    (for {
      p <- Random.between(0, 100)
      same = p < 20
      o <- Random.oneOf(gnomishConsonants)
    } yield if same then c.toString else o).mapSeed(c.toLong)

  def randomConsonant2(c: Char): Random[String] =
    (for {
      p <- Random.between(0, 100)
      same = p < 20
      o <- Random.oneOf(gnomishConsonants)
    } yield if same then "g" + c.toString else "g" + o).mapSeed(c.toLong << 1)

  val gnomishVowels = List(
   "a", "e", "i", "o", "u",
   "aw", "ew", "iw", "ow", "uw",
   "ä", "ë", "ï", "ö", "ü",
   "äw", "ëw", "ïw", "öw", "üw"
  )

  def randomVowel(c: Char): Random[String] =
    (for {
      p <- Random.between(0, 100)
      same = p < 20
      o <- Random.oneOf(gnomishVowels)
    } yield if same then c.toString else o).mapSeed(c.toLong)

  def randomVowel2(c: Char): Random[String] =
    (for {
      p <- Random.between(0, 100)
      same = p < 20
      o <- Random.oneOf(gnomishVowels)
    } yield if same then c.toString else o).mapSeed(c.toLong << 1)

  def isVowel(c: Char): Boolean =
    "aeiou".toList.contains(c)

  def randomSyllable(in: String): Random[String] =
    Random.sequence(in.toList.map(c => if isVowel(c) then randomVowel(c) else randomConsonant(c))).map(_.mkString)

  def randomSyllable2(in: String): Random[String] =
    Random.sequence(in.toList.map(c => if isVowel(c) then randomVowel2(c) else randomConsonant2(c))).map(_.mkString)

  def randomSyllables(in: String): Random[String] =
    (for {
      p <- Random.between(0, 100)
      one = p < 50
      two = p < 80
      a <- randomSyllable(in)
      b <- randomSyllable2(in)
      c <- randomSyllable(in)
    } yield if one then a else (if two then a + b else a + b + c)).mapSeed(in.toList.sum)

  def randomWord(in: List[String]): Random[String] =
    Random.sequence(in.map(_.toLowerCase).map(randomSyllables)).map(_.mkString)


@main def hello(args: String*): Unit =
  println("Gnomic word generated:")
  println(args.toList.map { w =>
    Gnomenclator.randomWord(List(w.split("\\.") :_*)).run
  }.mkString(" "))
