package objsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.Stream.Empty

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2: TweetSet = set1.incl(new Tweet("a", "a body", 20))
    val set3: TweetSet = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c: TweetSet = set3.incl(c)
    val set4d: TweetSet = set3.incl(d)
    val set4e: TweetSet = set2.incl(new Tweet("e1", "e1 body", 21)).incl(new Tweet("e2", "e2 body)", 22))
    val set5: TweetSet = set4c.incl(d)
    val set6: TweetSet = set4e.incl(new Tweet("k2", "k2 body", 210)).incl(new Tweet("k1", "k1 body", 39))
    val set7: TweetSet = set6.incl(new Tweet("p2", "p2 body", 1210)).incl(new Tweet("p1", "p1 body", 390))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("contains: k2") {
    new TestSets {
      assert(set6.contains(new Tweet("k2", "k2 body", 210)))
    }
  }

  test("contains: k1") {
    new TestSets {
      assert(set6.contains(new Tweet("k1", "k1 body", 39)))
    }
  }

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(_.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(_.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(_.retweets == 20)) === 2)
    }
  }

  test("filter: > 100 set6") {
    new TestSets {
      assert(size(set6.filter(_.retweets > 100)) === 1)
    }
  }

  test("filter: < 100 set6") {
    new TestSets {
      assert(size(set6.filter(_.retweets < 100)) === 4)
    }
  }

  test("filter: == 20 set3") {
    new TestSets {
      assert(size(set3.filter(_.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("union: empty set with empty set") {
    new TestSets {
      assert(size(set1 union set1) === 0)
    }
  }

  test("union: empty set") {
    new TestSets {
      assert(size(set1) === 0)
    }
  }

  test("union: set(1) with set(3) with set(3)") {
    new TestSets {
      assert(size(set2 union set4e union set5) === 6)
    }
  }

  test("union: tree1 and tree2") {
    val tree1 = new NonEmpty(new Tweet("100", "100", 100), new Empty, new Empty)
      .incl(new Tweet("105", "105", 105))
    assert(size(tree1) === 2)
    val tree2 = new NonEmpty(new Tweet("500", "500", 500), new Empty, new Empty)
      .incl(new Tweet("510", "510", 510))
      .incl(new Tweet("520", "520", 520))
    assert(size(tree2) === 3)
    val t = tree1 union tree2
    assert(size(t) === 5)
    assert(t.descendingByRetweet.head.retweets == 520)
    assert(t.descendingByRetweet.tail.head.retweets == 510)
    assert(t.descendingByRetweet.tail.tail.head.retweets == 500)
    assert(t.descendingByRetweet.tail.tail.tail.head.retweets == 105)
    assert(t.descendingByRetweet.tail.tail.tail.tail.head.retweets == 100)
  }

  test("descending: set5") {
    new TestSets {
      val trends: TweetList = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descending: set4e") {
    new TestSets {
      val trends: TweetList = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(set4e.descendingByRetweet.head.user == "e2")
    }
  }

  test("descending: set7") {
    new TestSets {
      assert(set7.descendingByRetweet.head.user == "p2")
      assert(set7.incl(new Tweet("p3", "p3 body", 3010)).descendingByRetweet.head.retweets == 3010)
    }
  }

  test("mostRetweeted") {
    new TestSets {
      assert((set2 union set4e union set5).mostRetweeted.retweets === 22)
      assert(set3.mostRetweeted.retweets === 20)
      assert(set6.mostRetweeted.retweets === 210)
    }
  }

  test("mostRetweeted: set7") {
    new TestSets {
      assert(set7.mostRetweeted.retweets === 1210)
    }
  }

  test("mostRetweeted: set7 remove tweet with 1210") {
    new TestSets {
      assert(set7.remove(new Tweet("p2", "p2 body", 1210)).mostRetweeted.retweets === 390)
    }
  }

  test("googleTweets") {
    assert(size(GoogleVsApple.googleTweets) > 1)
  }

  test("appleTweets") {
    assert(size(GoogleVsApple.appleTweets) > 1)
  }

  test("mostRetweeted: googleTweets") {
    assert(GoogleVsApple.googleTweets.mostRetweeted.retweets == 290)
  }

  test("mostRetweeted: appleTweets") {
    assert(GoogleVsApple.appleTweets.mostRetweeted.retweets == 321)
  }

  test("mostRetweeted: googleTweets.union(appleTweets)") {
    val r = GoogleVsApple.googleTweets.union(GoogleVsApple.appleTweets)
    assert(r.mostRetweeted.retweets == 321)
  }

  test("mostRetweeted: googleTweets.union(set6)") {
    new TestSets {
      val r: TweetSet = GoogleVsApple.googleTweets.union(set6)
      assert(r.mostRetweeted.retweets == 290)
      assert(r.contains(new Tweet("e1", "e1 body", 21)))
      assert(r.contains(new Tweet("e1", "e1 body", 21)))
    }
  }

  test("descendingByRetweet") {
    val s = new NonEmpty(new Tweet("a900", "a900", 1), new Empty, new Empty)
      .incl(new Tweet("b901", "b902", 210))
      .incl(new Tweet("c902", "c902", 212))
      .incl(new Tweet("d903", "d903", 213))
      .incl(new Tweet("b904", "b904", 122))
      .incl(new Tweet("c905", "c905", 123))
      .incl(new Tweet("d906", "d906", 114))
      .incl(new Tweet("max", "max", 6614))
      .incl(new Tweet("e907", "e907", 174))
      .incl(new Tweet("r908", "r908", 901))
      .incl(new Tweet("t910", "t910", 8))
      .incl(new Tweet("b9011", "b9021", 2101))
      .incl(new Tweet("c9021", "c9021", 2122))
      .incl(new Tweet("d9031", "d9031", 2131))
      .incl(new Tweet("b9041", "b9041", 1221))
      .incl(new Tweet("c9051", "c9051", 1231))
      .incl(new Tweet("d9061", "d9061", 1141))
      .incl(new Tweet("d9062", "d9062", 1141))
    assert(s.descendingByRetweet.head.user == "max")
  }

  test("descendingByRetweet: sample") {
    val s = new NonEmpty(new Tweet("CNET", "1Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 10), new Empty, new Empty)
      .incl(new Tweet("gizmodo", "2Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290))
      .incl(new Tweet("CNET", "3Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 25))
      .incl(new Tweet("gizmodo", "4This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 101))
      .incl(new Tweet("CNET", "5Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 1105))
      .incl(new Tweet("gizmodo", "6Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 1290))
      .incl(new Tweet("CNET", "7Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 125))
      .incl(new Tweet("gizmodo", "8This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 1101))
      .incl(new Tweet("CNET", "9Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 21105))
      .incl(new Tweet("gizmodo", "10Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 2290))
      .incl(new Tweet("CNET", "11Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 225))
      .incl(new Tweet("gizmodo", "12This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 2101))
      .incl(new Tweet("CNET", "13Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 32105))
      .incl(new Tweet("gizmodo", "14Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 3290))
      .incl(new Tweet("CNET", "15Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 325))
      .incl(new Tweet("gizmodo", "16This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 3101))
      .incl(new Tweet("CNET", "17Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 305))
      .incl(new Tweet("gizmodo", "2Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 90))
      .incl(new Tweet("CNET", "31Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 5))
      .incl(new Tweet("gizmodo", "4This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 101))
      .incl(new Tweet("CNET", "51Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 105))
      .incl(new Tweet("gizmodo", "6Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290))
      .incl(new Tweet("CNET", "71Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 25))
      .incl(new Tweet("gizmodo", "8This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 1101))
      .incl(new Tweet("CNET", "91Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 1105))
      .incl(new Tweet("gizmodo", "10Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290))
      .incl(new Tweet("CNET", "111Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 25))
      .incl(new Tweet("gizmodo", "12This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 2101))
      .incl(new Tweet("CNET", "131Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 2105))
      .incl(new Tweet("gizmodo", "14Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290))
      .incl(new Tweet("CNET", "151Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 25))
      .incl(new Tweet("gizmodo", "16This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 3101))
      .incl(new Tweet("CNET", "171Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 5))
    assert(s.mostRetweeted.retweets == 32105)
    assert(s.descendingByRetweet.head.retweets == 32105)
  }

  test("remove") {
    val m1 = new Tweet("CNET", "MAX. Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 32105)
    val m2 = new Tweet("gizmodo", "12This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 2101)
    val m3 = new Tweet("CNET", "1Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 10)
    val m4 = new Tweet("CNET", "171Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 5)
    val m5 = new Tweet("CNET", "9Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 21105)
    val m6 = new Tweet("gizmodo", "14Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 3290)
    val s = new NonEmpty(m3, new Empty, new Empty)
      .incl(new Tweet("gizmodo", "2Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290))
      .incl(new Tweet("CNET", "3Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 25))
      .incl(new Tweet("gizmodo", "4This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 101))
      .incl(new Tweet("CNET", "5Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 1105))
      .incl(new Tweet("gizmodo", "6Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 1290))
      .incl(new Tweet("CNET", "7Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 125))
      .incl(new Tweet("gizmodo", "8This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 1101))
      .incl(m5)
      .incl(new Tweet("gizmodo", "10Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 2290))
      .incl(new Tweet("CNET", "11Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 225))
      .incl(m2)
      .incl(m1)
      .incl(m6)
      .incl(new Tweet("CNET", "15Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 325))
      .incl(new Tweet("gizmodo", "16This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 3101))
      .incl(new Tweet("CNET", "17Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 305))
      .incl(new Tweet("gizmodo", "2Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 90))
      .incl(new Tweet("CNET", "31Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 5))
      .incl(new Tweet("gizmodo", "4This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 101))
      .incl(new Tweet("CNET", "51Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 105))
      .incl(new Tweet("gizmodo", "6Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290))
      .incl(new Tweet("CNET", "71Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 25))
      .incl(new Tweet("gizmodo", "8This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 1101))
      .incl(new Tweet("CNET", "91Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 1105))
      .incl(new Tweet("gizmodo", "10Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290))
      .incl(new Tweet("CNET", "111Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 25))
      .incl(new Tweet("gizmodo", "12This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 2101))
      .incl(new Tweet("CNET", "131Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx", 2105))
      .incl(new Tweet("gizmodo", "14Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290))
      .incl(new Tweet("CNET", "151Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl", 25))
      .incl(new Tweet("gizmodo", "16This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 3101))
      .incl(new Tweet("gizmodo", "14Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290))
      .incl(m4)
    val s1 = s
      .remove(m1)
      .remove(m2)
      .remove(m3)
      .remove(m4)
    val desc = s1.descendingByRetweet
    assert(!s1.contains(m1))
    assert(!s1.contains(m2))
    assert(!s1.contains(m3))
    assert(!s1.contains(m4))
    assert(s1.contains(m5))
    assert(desc.head == m5)
    assert(desc.tail.head == m6)
    val ss = s1 union s
    assert(ss.mostRetweeted == m1)
    assert(ss.descendingByRetweet.head == m1)
  }

  test("descendingByRetweet: google") {
    val s = GoogleVsApple.googleTweets
    assert(s.mostRetweeted.retweets == 290)
    assert(s.descendingByRetweet.head.retweets == 290)
  }

  test("trending") {
    assert(!GoogleVsApple.trending.isEmpty)
  }
}
