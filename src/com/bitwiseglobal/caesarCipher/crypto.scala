package com.bitwiseglobal.caesarCipher

/**
  * Created by vaijnathp on 11/30/2016.
  */
object crypto {
  def main(args: Array[String]): Unit = {
    //Encryption

    def rotate(str: List[Char], num: Int): List[Char] = (str, num) match {
      case (_, x) if x == 0 => str
      case (x :: xs, n) => rotate(xs ++ List(x), n - 1)
    }

    val alphabets = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toList
    println("Rotate:" + rotate(alphabets, 3))


    def makeKey(k: Int): List[(Char, Char)] = alphabets.zip(rotate(alphabets, k))
    println("MakeKey: " + makeKey(5))


    def lookUp(char: Char, mapping: List[(Char, Char)]): Char = mapping.find(a => a._1.equals(char)).map(e => e._2).getOrElse(char)
    println("LookUp: " + lookUp('D', makeKey(5)))


    def encipher(offset: Int, key: Char): Char = lookUp(key, makeKey(offset))
    println("Encipher: " + encipher(5, 'C'))


    def normalize(str: String): String = str.replaceAll("[^a-zA-Z0-9]", "").toUpperCase()
    println("Normalize: " + normalize("July 4th !"))


    def encipehrStr(index: Int, str: String): String = normalize(str).toList.map(x => encipher(index, x)).mkString
    println("Encipher: " + encipehrStr(5, "July 4th !"))


    //Decryption
    def reverseKey(keyList: List[(Char, Char)]): List[(Char, Char)] = keyList.map(e => (e._2, e._1))
    println("ReverseKey: " + reverseKey(makeKey(5)))

    def decipher(index: Int, char: Char): Char = reverseKey(makeKey(index)).find(i => i._1.equals(char)).getOrElse((char, char))._2
    println("DeCipher: " + decipher(5, 'V'))

    def deCipherStr(Index: Int, string: String): String = normalize(string).toList.map(e => decipher(Index, e)).mkString
    println("DeCipher: " + deCipherStr(5, "OZQD4YM"))

    def contains(str1: String, str2: String): Boolean = str1.contains(str2)
    println("Contains: " + contains("ABCDE", "ABC"))

    def candidate(str: String): List[(Int,String)] = (1 to 26).map(i => (i,deCipherStr(i, str))).filter(e => e._2.contains("THE") || e._2.contains("AND")).toList
    println("Candidate: " + candidate("DGGADBCOOCZYMJHZYVMTOJOCZHVS"))

  }
}
