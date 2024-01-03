
import scala.annotation.tailrec

abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

// Part 1: Basics
def weight(tree: CodeTree): Int = tree match
  case Fork(left, right, chars, weight) => weight
  case Leaf(char, weight) => weight
  case _ => throw new RuntimeException("No such CodeTree found")

def chars(tree: CodeTree): List[Char] = tree match
  case Fork(left, right, chars, weight) => chars
  case Leaf(char, weight) => List(char)
  case _ => throw new RuntimeException("There is not a value for List[Char]")

def makeCodeTree(left: CodeTree, right: CodeTree): Fork =
  Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

// Part 2: Generating huffman trees
def string2Chars(str: String): List[Char] = str.toList

  //Times. Count how many appearances make a char.
@tailrec
def timesChar(x: Char, xs: List[Char], acc: Int): (Char, Int) = xs match
  case y :: ys => if (x == y) timesChar(x, ys, acc + 1) else timesChar(x, ys, acc)
  case Nil => (x, acc)

def times(chars: List[Char]): List[(Char, Int)] = chars match
  case x :: xs => timesChar(x, xs, 1) :: times(xs.filterNot(_ == x))
  case _ => List()

val Text: String = "Hello my name is Inigo I am from Logrono"
val charList: List[Char] = string2Chars(Text)
val charTimes = times(charList)

val charGroups = Text.groupBy(identity)

val EmptyText = ""
val EmptyCharList = string2Chars(EmptyText)
times(EmptyCharList)

  //MakeOrderedLeafList. Define an ordered List based on the list got in times
def makeLeafList(list: List[(Char, Int)]): List[Leaf] =
  list.map((char, int) => Leaf(char, int))

//val LeafList = makeLeafList(charTimes)

def insertOrder (leaf: Leaf, acc: List[Leaf]): List[Leaf] = acc match
  case y :: ys =>
    if (leaf.weight <= y.weight) leaf :: acc
    else y :: insertOrder(leaf, ys)
  case Nil => List(leaf)

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match
  case x :: xs =>
    val leafList = makeLeafList(freqs)
    leafList.foldLeft(List[Leaf]())((acc, leaf) => insertOrder(leaf, acc))
  case Nil => List()

val charTimesOrdered = makeOrderedLeafList(charTimes)

  //Check wether the list contain only one element CodeTree
def singleton(trees: List[CodeTree]): Boolean = trees.tail.isEmpty

@tailrec
def combine(trees: List[CodeTree]): List[CodeTree] = trees match
  case t1 :: t2 :: trees =>
    val fork = makeCodeTree(t1, t2)
    combine(fork :: trees)
  case _ => trees

val combination = combine(charTimesOrdered).head

def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
  if (done(trees)) trees
  else merge(trees)

val untilList = until(singleton, combine)(makeOrderedLeafList(times(charList)))

def createCodeTree(chars: List[Char]): CodeTree = (until(singleton, combine)(makeOrderedLeafList(times(chars)))).head

val codeTree = createCodeTree(charList)

type Bit = Int

/**
 * This function decodes the bit sequence `bits` using the code tree `tree` and returns
 * the resulting list of characters.
 */
def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  @annotation.tailrec
  def decodeHelper(currentTree: CodeTree, remainingBits: List[Bit], acc: List[Char]): List[Char] = {
    currentTree match {
      case Leaf(char, _) => decodeHelper(tree, remainingBits, acc :+ char)
      case Fork(left, right, _, _) =>
        if (remainingBits.isEmpty) acc
        else if (remainingBits.head == 0) decodeHelper(left, remainingBits.tail, acc)
        else decodeHelper(right, remainingBits.tail, acc)
    }
  }
  decodeHelper(tree, bits, List.empty[Char])
}
/*
def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
  if (chars(tree).contains(text.head)){
    @tailrec
    def encodeHelper(tree: CodeTree, acc: List[Bit])(text: Char): List[Bit] = {
      tree match
        case Leaf(char, weight) => acc
        case Fork(left, right, _, _) =>
          if (chars(left).contains(text)) encodeHelper(left, acc :+ 0)(text)
          else encodeHelper(right, acc :+ 1)(text)
    }
  }else{
    List[Bit]()
    throw new RuntimeException("This letter is not contained in this Code Tree")
  }
*/
/*
def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  def encodeChar(currentTree: CodeTree, char: Char): List[Bit] = currentTree match {
    case Leaf(_, _) => List()  // Leaf nodes have no bits associated with them
    case Fork(left, right, _, _) =>
      if (left.chars.contains(char)) 0 :: encodeChar(left, char)
      else if (right.chars.contains(char)) 1 :: encodeChar(right, char)
      else throw new RuntimeException(s"Character '$char' not found in the tree")
  }

  text.flatMap(char => encodeChar(tree, char))
}

// Example usage with frenchCode
val inputText: List[Char] = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
val encodedBits: List[Bit] = encode(frenchCode)(inputText)

println(encodedBits.mkString)  // Output: 010001001111011011110011011100010111010101011011010111011001110101100011001110011001011111110



val bits: Int = 1
val listBits = bits.toString.map(_.asDigit).toList

val decoding = decode(codeTree, listBits)

val bits1: Int = 0101
val listBits1 = bits1.toString.map(_.asDigit).toList

val decoding1 = decode(codeTree, listBits1)

val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

/**
 * What does the secret message say? Can you decode it?
 * For the decoding use the `frenchCode' Huffman tree defined above.
 */
val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

/**
 * Write a function that returns the decoded secret
 */
//def decodedSecret: List[Char] = decode(frenchCode, secret)
val decodeSecret = decode(frenchCode, secret)
*/