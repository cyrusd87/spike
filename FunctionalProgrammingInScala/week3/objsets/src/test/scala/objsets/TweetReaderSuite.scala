package objsets

import org.scalatest.FunSuite

/**
  * Created by Ciro on 27/11/2016.
  */
class TweetReaderSuite extends FunSuite{
  test("all tweets works") {
    val tweets: TweetSet = TweetReader.allTweets
    assert(tweets  != null)

  }

}
