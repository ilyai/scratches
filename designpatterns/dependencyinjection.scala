import java.util.Date
import java.text.DateFormat

class TwitterUserProfile(val userName: String) {
  override def toString: String = "@" + userName
}

case class Tweet(tweeter: TwitterUserProfile,
                 message: String,
                 time: Date) {
  override def toString: String = "(" +
    DateFormat.getDateInstance(DateFormat.FULL).format(time) + ")" +
    tweeter + ": " + message
}

trait Tweeter {
  def tweet(message: String)
}

trait TwitterClientUIComponent {
  val ui: TwitterClientUI

  abstract class TwitterClientUI(val client: Tweeter) {
    def sendTweet(message: String) = client.tweet(message)
    def showTweet(tweet: Tweet): Unit
  }
}

trait TwitterLocalCacheComponent {
  val localCache: TwitterLocalCache

  trait TwitterLocalCache {
    def saveTweet(tweet: Tweet): Unit
    def history: List[Tweet]
  }
}

trait TwitterServiceComponent {
  val service: TwitterService

  trait TwitterService {
    def sendTweet(tweet: Tweet): Boolean
    def history: List[Tweet]
  }
}

trait TwitterClientComponent {
  self: TwitterClientUIComponent with
    TwitterLocalCacheComponent with
    TwitterServiceComponent =>

  val client: TwitterClient

  class TwitterClient(val user: TwitterUserProfile) extends Tweeter {
    override def tweet(message: String): Unit = {
      val twt = Tweet(user, message, new Date)
      if (service.sendTweet(twt)) {
        localCache.saveTweet(twt)
        ui.showTweet(twt)
      }
    }
  }
}

class TextClient(userProfile: TwitterUserProfile) extends TwitterClientComponent
    with TwitterClientUIComponent
    with TwitterLocalCacheComponent
    with TwitterServiceComponent {
  val client = new TwitterClient(userProfile)

  val ui = new TwitterClientUI(client) {
    override def showTweet(tweet: Tweet): Unit = println(tweet)
  }

  val localCache = new TwitterLocalCache {
    private var tweets: List[Tweet] = Nil

    override def saveTweet(tweet: Tweet): Unit = tweets ::= tweet
    override def history: List[Tweet] = tweets
  }

  val service = new TwitterService {
    override def sendTweet(tweet: Tweet): Boolean = {
      println("Sending tweet to Twitter HQ")
      true
    }
    override def history: List[Tweet] = List[Tweet]()
  }
}

val client = new TextClient(new TwitterUserProfile("BuckTrends"))
client.ui.sendTweet("My First Tweet. How's this thing work?")
client.ui.sendTweet("Is this thing on?")
client.ui.sendTweet("Heading to the bathroom...")
client.localCache.history.foreach { println }