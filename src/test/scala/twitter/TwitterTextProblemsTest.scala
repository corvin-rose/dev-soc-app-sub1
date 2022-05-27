package twitter

import org.scalatest.funsuite.AnyFunSuite
import twitter.models.Tweet

import scala.collection.immutable.HashSet

class TwitterTextProblemsTest extends AnyFunSuite {

  trait teststrings {

    val s1 = "This is the 88 Test! The result !!!should be: 9 Words"
    val words_s1 = List("be", "is", "result", "should", "test", "the","the", "this", "words")
    val s2 = "This is another test. It contains a lot of words which are also in string 1."
    val words_s2 = List("a", "also", "another", "are", "contains", "in", "is", "it", "lot", "of", "string", "test", "this", "which", "words")
    val test_list= List((0,s1),(1,""),(2,s2),(3,""))
    val wordlist=List("a", "also", "another", "are", "be", "contains", "in", "is", "is", "it", "lot", "of", "result", "should", "string",
      "test", "test", "the", "the", "this", "this", "which", "words", "words")
    val wordOccurences= List(("a",1), ("also",1), ("another",1), ("are",1), ("be",1), ("contains",1), ("in",1), ("is",2),
      ("it",1), ("lot",1), ("of",1), ("result",1), ("should",1), ("string",1), ("test",2), ("the",2), ("this",2), ("which",1), ("words",2))
    val tweets= List(Tweet(1L, null, -1, "noUser", "noName", s1,List(),"noParty"),
      Tweet(2L, null, -1, "noUser", "noName", s2,List(),"noParty"))
    val stopW:HashSet[String]= HashSet("a","the")
  }
  test("Test Word Extraction 1"){

    new teststrings{
      val r= TwitterTextProblems.getWords(s1)

      assert(r.length===9)
      assert(r.sorted===words_s1)
    }
  }

  test("Test Word Extraction 2"){

    new teststrings{
      val r= TwitterTextProblems.getWords(s2)
      assert(r.length===15)
      assert(r.sorted===words_s2)
    }
  }

  test("get All Words Extraction"){

    new teststrings{

      val result= TwitterTextProblems.getAllWords(test_list)
      assert (result.length===24)
      assert (result.sorted===wordlist)
    }
  }

  test("count Words"){

    new teststrings{

      val result= TwitterTextProblems.countWords(TwitterTextProblems.getAllWords(test_list))
      assert (result.length===19)
      assert (result.sorted===wordOccurences)
    }
  }

  test("getMost10UsedWords testData"){

    new teststrings{

      val result= TwitterTextProblems.getMost10UsedWords(tweets)
      assert (result.length===10)
      assert (result===List(("is",2), ("test",2), ("the",2), ("this",2), ("words",2), ("a",1), ("also",1),
        ("another",1), ("are",1), ("be",1)))
    }
  }

  test("prepare Data"){

    new teststrings {
      val expected= List("also", "another", "are", "contains", "lot", "result",
        "should", "string", "test", "test", "this", "this", "which", "words", "words")
      val data=TwitterTextProblems.prepareData(tweets, stopW)
      val result=data.flatMap(t=>t._2).sorted
      assert(result.sorted===expected)
    }
  }

  test("get All Words Extraction with Indizees"){

    new teststrings{

      val expected= List((1,"be"), (1,"is"), (1,"result"), (1,"should"), (1,"test"), (1,"the"), (1,"this"), (1,"words"),
        (2,"a"), (2,"also"), (2,"another"), (2,"are"), (2,"contains"), (2,"in"), (2,"is"), (2,"it"), (2,"lot"), (2,"of"),
        (2,"string"), (2,"test"), (2,"this"), (2,"which"), (2,"words"))
      val result= TwitterTextProblems.getAllWordsWithIndex(tweets).toList.sorted
      assert (result.length===23)
      assert (result.sorted===expected)
    }
  }

  test("create Inverse Indizees"){

    new teststrings{

      val expected= Map("test" -> Set(2, 1), "another" -> Set(2), "result" -> Set(1), "it" -> Set(2), "a" -> Set(2),
        "string" -> Set(2), "also" -> Set(2), "should" -> Set(1), "which" -> Set(2), "be" -> Set(1),
        "contains" -> Set(2), "of" -> Set(2), "the" -> Set(1), "this" -> Set(1, 2), "in" -> Set(2), "are" -> Set(2),
        "is" -> Set(1, 2), "lot" -> Set(2), "words" -> Set(2, 1))
      val result= TwitterTextProblems.createInverseIndex(TwitterTextProblems.getAllWordsWithIndex(tweets))
      assert (result===expected)
    }
  }



  test("test or Conjunction 1"){

    new teststrings{

      val invInd: Map[String, Set[Long]] = TwitterTextProblems.createInverseIndex(TwitterTextProblems.getAllWordsWithIndex(tweets))
      val result= TwitterTextProblems.orConjunction(List("hello","test"), invInd)
      assert (result===Set(1L,2L))
    }
  }

  test("test or Conjunction 2"){

    new teststrings{

      val invInd= TwitterTextProblems.createInverseIndex(TwitterTextProblems.getAllWordsWithIndex(tweets))
      val result= TwitterTextProblems.orConjunction(List("hello","contains"), invInd)
      assert (result===Set(2))
    }
  }

  test("test or Conjunction 3"){

    new teststrings{

      val invInd= TwitterTextProblems.createInverseIndex(TwitterTextProblems.getAllWordsWithIndex(tweets))
      val result= TwitterTextProblems.orConjunction(List("hello","bang"), invInd)
      assert (result===Set())
    }
  }

  test("test and Conjunction 1"){

    new teststrings{

       val invInd= TwitterTextProblems.createInverseIndex(TwitterTextProblems.getAllWordsWithIndex(tweets))
       val result= TwitterTextProblems.andConjunction(List("this","test","contains"), invInd)
       assert (result===Set(2))
    }
  }

    test("test and Conjunction 2"){

    new teststrings{

       val invInd= TwitterTextProblems.createInverseIndex(TwitterTextProblems.getAllWordsWithIndex(tweets))
       val result= TwitterTextProblems.andConjunction(List("this","is"), invInd)
       assert (result===Set(1,2))
    }
  }

  test("test and Conjunction 3"){

    new teststrings{

       val invInd= TwitterTextProblems.createInverseIndex(TwitterTextProblems.getAllWordsWithIndex(tweets))
       val result= TwitterTextProblems.andConjunction(List("this","hello"), invInd)
       assert (result===Set())
    }
  }


}
