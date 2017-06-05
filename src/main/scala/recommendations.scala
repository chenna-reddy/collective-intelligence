import scala.collection.immutable.SortedMap

/**
  * Created by Chenna Reddy
  */
object recommendations extends App {

  val critics = Map(
    "Lisa Rose" -> Map(
      "Lady in the Water" -> 2.5,
      "Snakes on a Plane" -> 3.5,
      "Just My Luck" -> 3.0,
      "Superman Returns" -> 3.5,
      "You, Me and Dupree" -> 2.5,
      "The Night Listener" -> 3.0
    ),
    "Gene Seymour" -> Map(
      "Lady in the Water" -> 3.0,
      "Snakes on a Plane" -> 3.5,
      "Just My Luck" -> 1.5,
      "Superman Returns" -> 5.0,
      "The Night Listener" -> 3.0,
      "You, Me and Dupree" -> 3.5
    ),
    "Michael Phillips" -> Map(
      "Lady in the Water" -> 2.5,
      "Snakes on a Plane" -> 3.0,
      "Superman Returns" -> 3.5,
      "The Night Listener" -> 4.0
    ),
    "Claudia Puig" -> Map(
      "Snakes on a Plane" -> 3.5,
      "Just My Luck" -> 3.0,
      "The Night Listener" -> 4.5,
      "Superman Returns" -> 4.0,
      "You, Me and Dupree" -> 2.5
    ),
    "Mick LaSalle" -> Map(
      "Lady in the Water" -> 3.0,
      "Snakes on a Plane" -> 4.0,
      "Just My Luck" -> 2.0,
      "Superman Returns" -> 3.0,
      "The Night Listener" -> 3.0,
      "You, Me and Dupree" -> 2.0
    ),
    "Jack Matthews" -> Map(
      "Lady in the Water" -> 3.0,
      "Snakes on a Plane" -> 4.0,
      "The Night Listener" -> 3.0,
      "Superman Returns" -> 5.0,
      "You, Me and Dupree" -> 3.5
    ),
    "Toby" -> Map(
      "Snakes on a Plane" -> 4.5,
      "You, Me and Dupree" -> 1.0,
      "Superman Returns" -> 4.0
    )
  )

  def simDistance(prefs: Map[String, Map[String, Double]], person1: String, person2: String): Double = {
    val si = sharedItems(prefs, person1, person2)
    val ss = si map (i => math.pow(prefs(person1)(i) - prefs(person2)(i), 2))
    1 / (1 + (ss.sum))
  }

  def sharedItems(prefs: Map[String, Map[String, Double]], person1: String, person2: String): Seq[String] = {
    val p1 = prefs.getOrElse(person1, Map())
    val p2 = prefs.getOrElse(person2, Map())
    (p1.keySet intersect p2.keySet) toSeq
  }

  def simPearson(prefs: Map[String, Map[String, Double]], person1: String, person2: String): Double = {
    val si = sharedItems(prefs, person1, person2)

    def sum(person: String) = si map (x => prefs(person)(x)) sum

    def sumSq(person: String) = si map (x => math.pow(prefs(person)(x), 2)) sum

    val pSum = si map (x => prefs(person1)(x) * prefs(person2)(x)) sum
    val sum1 = sum(person1)
    val sum2 = sum(person2)
    val sum1Sq = sumSq(person1)
    val sum2Sq = sumSq(person2)
    val num = pSum - (sum1 * sum2 / si.size)
    val den = math.sqrt((sum1Sq - math.pow(sum1, 2) / si.size) * (sum2Sq - math.pow(sum2, 2) / si.size))
    if (den == 0) 0 else num / den
  }

  def topMatches(prefs: Map[String, Map[String, Double]], person: String, n: Int = 5, similarity: (Map[String, Map[String, Double]], String, String) => Double = simPearson) = {
    val scores = prefs.keys.toSeq.filter(x => x != person).map(o => (similarity(prefs, person, o), o))
    SortedMap(scores: _*)(Ordering.fromLessThan(_ > _))
  }

  def getRecommendations(prefs: Map[String, Map[String, Double]], person: String, similarity: (Map[String, Map[String, Double]], String, String) => Double = simPearson) = {
    def score(s: Seq[(String, Double, Double)]) = s.map(x => x._2).sum / s.map(x => x._3).sum
    val scores = prefs.keySet.-(person).toSeq.flatMap{p =>
      val s = similarity(prefs, person, p)
      prefs(p).map(i => (i._1, i._2 * s, s)).toSeq
    }
    val ranks = scores.filterNot(x => prefs(person).contains(x._1)).groupBy(x => x._1).map(x => (score(x._2), x._1)).toSeq
    SortedMap(ranks: _*)(Ordering.fromLessThan(_ > _))
  }

  def transformPrefs(prefs: Map[String, Map[String, Double]]) = {
    val f = prefs.toSeq.flatMap(x => x._2.toSeq.map(y => (y._1, (x._1, y._2))))
    f.groupBy(x => x._1).mapValues(y => y.map(z => z._2).toMap)
  }


  def calculateSimilarItems(prefs: Map[String, Map[String, Double]], n: Int, similarity: (Map[String, Map[String, Double]], String, String) => Double = simPearson) = {
    val itemBased = transformPrefs(prefs)
    itemBased.keys.toSeq.map(i => (i, topMatches(itemBased, i, n, similarity))).toMap
  }


  def getRecommendedItems(prefs: Map[String, Map[String, Double]], itemMatch: Map[String, Map[Double, String]], user: String) = {
    def score(s: Seq[(String, Double, Double)]) = s.map(x => x._2).sum / s.map(x => x._3).sum
    val ratedItems = prefs.getOrElse(user, Map.empty).toSeq.flatMap { i =>
      itemMatch(i._1).filterNot(x => x._2 == i._1).toSeq.map(e => (e._2, e._1 * i._2, e._1))
    }
    val similar = ratedItems.filterNot(x => prefs(user).contains(x._1)).groupBy(x => x._1).map(x => (score(x._2), x._1)).toSeq
    SortedMap(similar: _*)(Ordering.fromLessThan(_ > _))
  }

  println("simDistance: " + simDistance(critics, "Lisa Rose", "Gene Seymour"))
  println("simPearson: " + simPearson(critics, "Lisa Rose", "Gene Seymour"))
  println("topMatches: " + topMatches(recommendations.critics, "Toby", 3, simPearson))
  println("recommendations (pearson): " + getRecommendations(critics, "Toby", simPearson))
  println("recommendations (distance): " + getRecommendations(critics, "Toby", simDistance))
  println("transform: " + topMatches(transformPrefs(critics), "Superman Returns"))
  println("calculateSimilarItems: " + calculateSimilarItems(critics, 10, simDistance))
  println("recommendations: " + getRecommendedItems(critics, calculateSimilarItems(critics, 10, simDistance), "Toby"))
}
