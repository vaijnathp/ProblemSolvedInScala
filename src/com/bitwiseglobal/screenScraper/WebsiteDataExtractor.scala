package com.bitwiseglobal.screenScraper

/**
  * Created by vaijnathp on 11/30/2016.
  */
object WebsiteDataExtractor {

  def main(args: Array[String]): Unit = {

    def sameString(string1: String,string2: String): Boolean = string1.equalsIgnoreCase(string2)
    println("Is Same String: "+sameString("Vaijnath","VAIJNATH"))

    def prefix(string1: String,string2: String): Boolean = string2.toLowerCase().startsWith(string1.toLowerCase())
    println("Prefix: "+prefix("bc" ,"bCDE"))

    def contains(s: String, s1: String): Boolean = s.toLowerCase().contains(s1.toLowerCase())
    println("Contains: "+contains("abcde","bc"))

    def takeUntil(s: String, s1: String): String = s1.toList.take(s1.indexOf(s)).mkString
    println("takeUntil: "+takeUntil("cd","abcdef"))

    def dropUntil(s: String, s1: String): String = s1.toList.drop(s1.indexOf(s)+1).mkString
    println("DropUntil: "+dropUntil(",",dropUntil(",",dropUntil(",","comma,separated,string"))))

    def getSplit(delim: String, str: String, list: List[String]): List[String] =
      (delim,str) match {
        case (delim,str) if !contains(str,delim) => list ++ List(str)
        case (delim,str) => getSplit(delim,dropUntil(delim,str),list ++ List(takeUntil(delim,str)))
      }

    def split(delim:String,str:String): List[String] = getSplit(delim,str,Nil)
    println("split: "+split(",","comma,separated,string"))

    def reconstruct(string: String,list: List[String]):String = list.mkString(string)
    println("reconstruct: "+reconstruct("vaij",split(",","comma,separated,string")))




    }
}
