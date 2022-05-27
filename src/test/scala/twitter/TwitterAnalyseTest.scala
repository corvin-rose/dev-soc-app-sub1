package twitter

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import twitter.utils.IOUtils
import twitter.utils.IOUtils.twitterDataFromFile

import scala.collection.immutable.HashSet

class TwitterAnalyseTest extends AnyFunSuite with BeforeAndAfterAll{

  val tweets= twitterDataFromFile("tweets2021.json")
  val words=IOUtils.readStopwordsFromFile("stopwords.txt")
  val invInd= TwitterTextProblems.createInverseIndex(TwitterTextProblems.getAllWordsWithIndex(tweets))

  test("Read Twitter Data") {
    println("Example Tweets:")
    tweets.take(5).foreach(println)
    assert(tweets.size===106133)
  }

  test("getMost10UsedWords all Tweets"){

    val expected= List(("die",64503), ("der",51834), ("und",48896), ("t",48043), ("co",42067), ("https",41160),
      ("in",32052), ("r",30944), ("f",29322), ("das",29212))
    val result= TwitterTextProblems.getMost10UsedWords(tweets)
    assert (result.length===10)
    assert (result===expected)
  }

  test("read Stopwords into HashMap"){

    println("Example Stopwords:")
    words.take(5).foreach(println)
    assert(words.size===595)
  }

  test("getMost10UsedWords Cleaned Tweets"){

    val expected= List(("https",36859), ("ber",6615), ("amp",6144), ("heute",5601), ("cdu",4751), ("corona",4516),
      ("geht",4290), ("mal",4218), ("fdp",3950), ("ssen",3802))
    val result= TwitterTextProblems.getMost10UsedWordsCleaned(tweets,words)
    assert (result.length===10)
    assert (result===expected)
  }

  test("getLeast10UsedWords Cleaned Tweets"){

    val expected= List(("aaaalrighty",5), ("abendmahl",5), ("abendspaziergang",5), ("aberkennung",5), ("abfahrt",5),
      ("abgebildet",5), ("abgehobene",5), ("abgemeldet",5), ("abgeordnetendi",5), ("abgerissen",5))
    val result= TwitterTextProblems.getLeast10UsedWordsCleaned(tweets,words)
    assert (result.length===10)
    assert (result===expected)
  }

  test("test or conjunction 1"){

    val expected= HashSet(1358707430272679936L, 1346091425478430720L, 1385628201578270725L, 1381509659375329282L,
      1391890893561544705L, 1388032944477024258L, 1366404143196299264L)
    val result= TwitterTextProblems.orConjunction(List("vollpfosten","idiot"), invInd)
    assert(result===expected)
  }

  test("test or conjunction 2"){

    val expected= Set(1358707430272679936L, 1385628201578270725L, 1366404143196299264L)
    val result= TwitterTextProblems.orConjunction(List("vollpfosten","stinkstiefel"), invInd)
    assert(result===expected)
  }

  test("test and conjunction 1"){

    val expected= HashSet()
    val result= TwitterTextProblems.andConjunction(List("vollpfosten","idiot"), invInd)
    assert(result===expected)
  }
  test("test and conjunction 2"){

    val expected= Set(1346091425478430720L)
    val result= TwitterTextProblems.andConjunction(List("menschenverachtender","idiot"), invInd)
    assert(result===expected)
  }

}

/*
class TwitterAnalyseTest extends AnyFunSuite with BeforeAndAfterAll{

  val tweets= twitterDataFromFile("tweets2021_klein.json", true)
  val words=IOUtils.readStopwordsFromFile("stopwords.txt")


  test("Read Twitter Data") {
    println("Example Tweets:")
    tweets.take(5).foreach(println)
    assert(tweets.size===38530)
  }

  test("getMost10UsedWords all Tweets"){

    val expected= List(("die",23071), ("der",18705), ("und",17773), ("t",17012), ("co",15054), ("https",14627),
      ("in",11359), ("r",11251), ("das",10824), ("f",10729))
    val result= TwitterTextProblems.getMost10UsedWords(tweets)
    assert (result.length===10)
    assert (result===expected)
  }

  test("read Stopwords into HashMap"){

    println("Example Stopwords:")
    words.take(5).foreach(println)
    assert(words.size===595)
  }

  test("getMost10UsedWords Cleaned Tweets"){

    val result= TwitterTextProblems.getMost10UsedWordsCleaned(tweets,words)
    assert (result.length===10)
    assert (result===List(("https",13088), ("ber",2341), ("amp",2247), ("heute",2031), ("mal",1557), ("fdp",1541),
      ("geht",1519), ("cdu",1518), ("bundestag",1449), ("ssen",1306)))
  }

  test("getLeast10UsedWords Cleaned Tweets"){

    val result= TwitterTextProblems.getLeast10UsedWordsCleaned(tweets,words)
    assert (result.length===10)
    assert (result===List(("aaaalrighty",5), ("abbruch",5), ("abenteuerlich",5), ("abermals",5), ("abgebrochen",5),
      ("abgesch",5), ("abgesichert",5), ("abgrund",5), ("abhalten",5), ("abitur",5)))
  }


}

 */