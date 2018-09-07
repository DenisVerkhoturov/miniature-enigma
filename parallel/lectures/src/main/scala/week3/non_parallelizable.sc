def sum(xs: Array[Int]): Int = (xs.par fold 0) (_ + _)

def max(xs: Array[Int]): Int = (xs.tail.par fold xs.head) (math.max)

def isVowel(char: Char): Boolean = Set('A', 'E', 'I', 'O', 'U').contains(char.toUpper)

def countVowels(sentence: String): Int = (sentence.par aggregate 0)(
  (count, char) => if (isVowel(char)) count + 1 else count
  , _ + _
)
